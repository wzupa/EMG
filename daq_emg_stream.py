import nidaqmx
from nidaqmx.constants import TerminalConfiguration, AcquisitionType
import csv
import time
import sys
import os
import argparse
from collections import deque
from datetime import datetime

def main():
    parser = argparse.ArgumentParser(description="NI USB-6009 EMG Acquisition Daemon")
    parser.add_argument("--sample-rate", type=int, default=1000, help="Sampling rate in Hz (default: 1000)")
    parser.add_argument("--channels", type=str, default="Dev1/ai0,Dev1/ai1", help="Comma-separated channels (e.g. Dev1/ai0,Dev1/ai1)")
    parser.add_argument("--channel-names", type=str, default="", help="Custom names for the channels, comma-separated")
    parser.add_argument("--live-output", type=str, default="emg_live.csv", help="Path for the live rolling CSV file")
    parser.add_argument("--stop-file", type=str, default="stop_emg.txt", help="Path for the stop trigger file")
    parser.add_argument("--record-control", type=str, default="record_control.txt", help="Path for the record control file")
    
    args = parser.parse_args()
    
    sample_rate = args.sample_rate
    channel_list = [c.strip() for c in args.channels.split(",") if c.strip()]
    
    # Process custom channel names
    if args.channel_names.strip():
        chan_names = [n.strip() for n in args.channel_names.split(",") if n.strip()]
        # If lengths don't match, fallback or pad
        if len(chan_names) < len(channel_list):
            for i in range(len(chan_names), len(channel_list)):
                chan_names.append(channel_list[i].split("/")[-1])
        elif len(chan_names) > len(channel_list):
            chan_names = chan_names[:len(channel_list)]
    else:
        chan_names = [c.split("/")[-1] for c in channel_list]
        
    csv_headers = ["time_s", "clock_time"] + chan_names
    
    # Calculate block size: 100ms worth of data, min 10 samples
    block_size = max(10, int(sample_rate * 0.1))
    
    # Live buffer: keep last 65 seconds
    live_history_seconds = 65
    max_history_samples = int(sample_rate * live_history_seconds)
    
    # Deque to hold history: each element is a list [t, clock_time, val_ch1, val_ch2, ...]
    live_buffer = deque(maxlen=max_history_samples)
    
    # Clean up stop file if exists before starting
    if os.path.exists(args.stop_file):
        try:
            os.remove(args.stop_file)
        except Exception:
            pass
            
    # Clean up record control file if exists before starting
    if os.path.exists(args.record_control):
        try:
            os.remove(args.record_control)
        except Exception:
            pass

    # Ensure output files are reset
    if os.path.exists(args.live_output):
        try:
            os.remove(args.live_output)
        except Exception:
            pass

    current_record_path = None
    record_file_handle = None
    record_writer = None
    
    # Track sample index and absolute t0
    sample_index = 0
    t0_epoch = time.time()
    
    last_live_write_time = 0
    live_write_interval = 0.25  # seconds (4Hz update is plenty for Shiny plot)
    
    print(f"Starting acquisition on channels: {channel_list} ({chan_names}) at {sample_rate} Hz")
    sys.stdout.flush()
    
    try:
        with nidaqmx.Task() as task:
            for chan in channel_list:
                task.ai_channels.add_ai_voltage_chan(
                    chan,
                    terminal_config=TerminalConfiguration.RSE,
                    min_val=-10.0,
                    max_val=10.0
                )
                
            task.timing.cfg_samp_clk_timing(
                rate=sample_rate,
                sample_mode=AcquisitionType.CONTINUOUS,
                samps_per_chan=block_size * 2
            )
            
            task.start()
            print("DAQ Task Started successfully.")
            sys.stdout.flush()
            
            while True:
                # 1. Check for stop command
                if os.path.exists(args.stop_file):
                    try:
                        with open(args.stop_file, "r") as sf:
                            if sf.read().strip().lower() == "stop":
                                print("Stop command detected. Exiting.")
                                break
                    except Exception:
                        pass
                
                # 2. Check record control file
                target_record_path = None
                if os.path.exists(args.record_control):
                    try:
                        with open(args.record_control, "r") as rc:
                            content = rc.read().strip()
                            if content and content.lower() != "stop":
                                target_record_path = content
                    except Exception:
                        pass # Ignore concurrent read issues
                
                # Update recording file handle if state changed
                if target_record_path != current_record_path:
                    if record_file_handle:
                        record_file_handle.close()
                        record_file_handle = None
                        record_writer = None
                        print(f"Stopped recording to: {current_record_path}")
                        sys.stdout.flush()
                        
                    current_record_path = target_record_path
                    if current_record_path:
                        # Open new file
                        file_exists = os.path.exists(current_record_path)
                        try:
                            # Semicolon delimited and dot decimal
                            record_file_handle = open(current_record_path, "a", newline="")
                            record_writer = csv.writer(record_file_handle, delimiter=";")
                            if not file_exists or os.path.getsize(current_record_path) == 0:
                                record_writer.writerow(csv_headers)
                            
                            # Initialize recording specific counter
                            record_sample_index = 0
                            
                            print(f"Started recording to: {current_record_path}")
                            sys.stdout.flush()
                        except Exception as e:
                            print(f"Error opening recording file: {e}")
                            sys.stdout.flush()
                            record_file_handle = None
                            record_writer = None
                            current_record_path = None
                
                # 3. Read block of data
                data = task.read(
                    number_of_samples_per_channel=block_size,
                    timeout=5.0
                )
                
                # Handle single channel data vs multi channel data
                if len(channel_list) == 1:
                    if isinstance(data, list) and len(data) > 0 and not isinstance(data[0], list):
                        data = [data]
                
                # 4. Process block
                n_samples = len(data[0])
                for idx in range(n_samples):
                    t = sample_index / sample_rate
                    t_epoch = t0_epoch + t
                    
                    # Format clock time with milliseconds
                    dt = datetime.fromtimestamp(t_epoch)
                    clock_time = dt.strftime('%H:%M:%S.%f')[:-3]
                    
                    row_live = [t, clock_time] + [data[ch][idx] for ch in range(len(channel_list))]
                    
                    # Append to live buffer deque
                    live_buffer.append(row_live)
                    
                    # Write to recording file if active (with relative time starting at 0.0)
                    if record_writer:
                        rec_t = record_sample_index / sample_rate
                        row_record = [rec_t, clock_time] + [data[ch][idx] for ch in range(len(channel_list))]
                        record_writer.writerow(row_record)
                        record_sample_index += 1
                    
                    sample_index += 1

                
                if record_file_handle:
                    record_file_handle.flush()
                
                # 5. Write live file atomically (semicolon delimited)
                now = time.time()
                if now - last_live_write_time >= live_write_interval:
                    last_live_write_time = now
                    tmp_file = args.live_output + ".tmp"
                    try:
                        with open(tmp_file, "w", newline="") as lf:
                            # Semicolon delimited
                            lw = csv.writer(lf, delimiter=";")
                            lw.writerow(csv_headers)
                            lw.writerows(live_buffer)
                        # Atomic replace
                        os.replace(tmp_file, args.live_output)
                    except Exception as e:
                        # Ignore brief write collision errors
                        pass

    except Exception as e:
        print(f"Exception in acquisition daemon: {e}")
        sys.stdout.flush()
    finally:
        # Cleanup
        if record_file_handle:
            record_file_handle.close()
        print("Acquisition daemon finished.")
        sys.stdout.flush()

if __name__ == "__main__":
    main()
