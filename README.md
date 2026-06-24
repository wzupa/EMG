# Real-Time EMG Acquisition & Analysis System (v0.1.0)

This repository contains an integrated high-performance system for real-time acquisition, digital filtering, and offline post-processing of biological electromyographic (EMG) signals. The software is specifically optimized for animal physiology studies and critical swimming speed (**Ucrit**) fish protocols.

Developed for **Fondazione COISPA ETS**.

---

## 🚀 System Architecture

The system uses a hybrid architecture to ensure maximum GUI responsiveness and hardware sampling efficiency:
* **Frontend (R Shiny)**: An interactive user interface based on `bslib` (styled in dark mode for low-light laboratory environments) that handles synchronized multi-channel plot visualization, digital filter processing, and metadata configuration.
* **Backend (Python)**: A background acquisition daemon (`daq_emg_stream.py`) that communicates at a low level with the **National Instruments NI USB-6009** DAQ card using the official NI-DAQmx API, ensuring continuous data streaming without memory leaks or UI freezes.

---

## ✨ Key Features

* **Real-Time Multi-Channel Monitoring**: Simultaneous, vertically aligned plotting of up to 8 analog channels (from `ai0` to `ai7`) using interactive `plotly` charts.
* **Optimized Rendering**: Dynamic downsampling (decimation) algorithm to display large time windows (e.g., 60 seconds) smoothly by limiting the number of plotted data points.
* **Digital Signal Processing (DSP)**:
  * **Offset Removal (High-Pass 5 Hz)**: Filters out electrical DC baseline shifts and motion artifacts. Uses signal pre-padding to eliminate filter startup transients at $t=0$.
  * **Notch Filter (50 Hz)**: Selectively removes electrical AC grid interference (hum from nearby power outlets, lights, etc.).
  * **Low-Pass Filter (450 Hz)**: Attenuates high-frequency electronic noise that carries no useful physiological data.
  * **Signal Conditioning**: Supports raw, rectified (full-wave), and envelope signals via a customizable **moving RMS** window.
* **DC Offset Calibration**: A "Zero from live signal" button subtracts residual baseline voltage at rest on a per-channel basis.
* **Conflict-Free & Lossless Saving**:
  * Atomic and isolated data writing prevents file lock errors.
  * Automated and normalized file naming combining Ucrit metadata (Species, ID, Size, Date, and Speed Step).
  * Auto-increment index: if the file already exists on disk, a numerical suffix is dynamically appended (e.g., `_01`, `_02`, etc.) to prevent accidental overwrites.
* **Native Bilingualism**: Instantaneous translation (Italian/English) of the entire interface without resetting tab states or losing active chart data.
* **Offline Analysis Tab**: Load previous recordings in `.csv` or `.rds` formats to inspect specific slices, with automated metrics calculation (Mean, RMS, Peak, and iEMG) over the selected window.

---

## 🛠️ System Requirements & Installation

### 1. Hardware Driver
To interface with the NI USB-6009 card, you must install the official National Instruments drivers:
* Download and install [NI-DAQmx](https://www.ni.com/en/support/downloads/drivers/download.ni-daqmx.html).

### 2. Python Environment
The background streaming daemon requires Python 3 (added to the system PATH) and the following libraries:
```bash
pip install nidaqmx numpy
```

### 3. R / RStudio Environment
The graphical interface requires R (version $\ge$ 4.0 recommended) and the packages listed below. To install them, run the following in RStudio:
```R
install.packages(c("shiny", "bslib", "plotly", "tidyverse"))
```

---

## 🏃 How to Run the Application

1. Connect the NI USB-6009 DAQ card to your computer via USB (verify it is recognized by the system as `Dev1` using the NI MAX utility).
2. Open RStudio.
3. Set your working directory to the folder containing the application:
   ```R
   setwd("path/to/EMG/folder")
   ```
4. Run the Shiny app:
   ```R
   shiny::runApp()
   ```

---

## 📝 Recording File Structure

Files saved at the end of a recording (both in `.csv` and high-speed binary `.rds` formats) contain the following columns:
* `time_s`: Relative elapsed time from the start of the recording (in seconds, starting at `0.0`).
* `clock_time`: Precise wall-clock time in `HH:MM:SS.FFF` format derived from the system clock.
* Active Channels (e.g., `ai0`, `ai1`): Calibrated voltage values (expressed in Volts), adjusted by the offset determined during calibration.

---

## 👥 Credits and Contact

* **Scientific Author**: Walter Zupa ([zupa@fondazionecoispa.org](mailto:zupa@fondazionecoispa.org))
* **Development & Affiliation**: [Fondazione COISPA ETS](https://www.coispa.it)
