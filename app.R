library(shiny)
library(bslib)
library(ggplot2)
library(readr)
library(dplyr)
library(plotly)

# Configuration
py_exe <- "C:/Users/Walter/AppData/Local/Python/pythoncore-3.14-64/python.exe"
app_dir <- normalizePath(".", winslash = "/")
script_path <- file.path(app_dir, "daq_emg_stream.py")
data_file <- file.path(app_dir, "emg_live.csv")
stop_file <- file.path(app_dir, "stop_emg.txt")
record_control_file <- file.path(app_dir, "record_control.txt")

# Helper function to sanitize user inputs for filenames/headers
clean_filename_part <- function(text) {
    if (is.null(text) || trimws(text) == "") return("NA")
    clean <- gsub("[^a-zA-Z0-9_-]", "_", text)
    clean <- gsub("_+", "_", clean)
    clean <- gsub("^_+|_+$", "", clean)
    clean
}

# Native DSP Filters in R (No 'signal' package required)
filter_highpass <- function(x, fs = 1000, fc = 5) {
    if (length(x) == 0) return(x)
    # Pre-pad with 0.2 seconds of the first value to completely eliminate startup transient
    n_pad <- min(length(x), round(0.2 * fs))
    if (n_pad > 0) {
        x_padded <- c(rep(x[1], n_pad), x)
    } else {
        x_padded <- x
    }
    
    # Subtract first value to prevent step-response startup transient from DC offset
    x_shifted <- x_padded - x_padded[1]
    alpha <- 1 / (1 + 2 * pi * fc / fs)
    dx <- c(x_shifted[1], diff(x_shifted))
    y <- stats::filter(dx, filter = alpha, method = "recursive")
    y <- as.numeric(y)
    
    # Discard padding
    if (n_pad > 0) {
        y <- y[(n_pad + 1):length(y)]
    }
    y
}

filter_lowpass <- function(x, fs = 1000, fc = 450) {
    beta <- (2 * pi * fc / fs) / (1 + 2 * pi * fc / fs)
    y <- stats::filter(beta * x, filter = 1 - beta, method = "recursive")
    as.numeric(y)
}

filter_notch <- function(x, fs = 1000, f0 = 50, r = 0.98) {
    n <- length(x)
    if (n < 3) return(x)
    w0 <- 2 * pi * f0 / fs
    lag1 <- c(x[1], x[-n])
    lag2 <- c(x[1], x[1], x[-c(n-1, n)])
    y_ma <- x - 2 * cos(w0) * lag1 + lag2
    y <- stats::filter(y_ma, filter = c(2 * r * cos(w0), -r^2), method = "recursive")
    as.numeric(y)
}

moving_rms <- function(x, window_ms = 50, fs = 1000) {
    window_samples <- max(5, round((window_ms / 1000) * fs))
    n <- length(x)
    if (n < window_samples) return(rep(0, n))
    x2 <- x^2
    ma <- stats::filter(x2, filter = rep(1/window_samples, window_samples), method = "convolution", sides = 1)
    ma[is.na(ma)] <- 0
    sqrt(as.numeric(ma))
}

# Translation helper function
tr <- function(key, lang = "it") {
    if (is.null(lang) || !(lang %in% c("it", "en"))) {
        lang <- "it"
    }
    dict <- list(
        it = list(
            title = "Sistema di Acquisizione e Analisi EMG Real-Time v0.1.0",
            tab_live = "Acquisizione Live",
            tab_offline = "Analisi File Registrati",
            tab_info = "Guida & Info",
            ctrl_panel = "Pannello di Controllo",
            daq_monitor = "Monitoraggio DAQ",
            btn_start = "Avvia",
            btn_stop = "Ferma",
            ucrit_meta = "Metadati Prova (Ucrit)",
            specie = "Specie:",
            individuo = "Individuo (ID):",
            taglia = "Taglia (cm):",
            giorno = "Giorno Prova:",
            step = "Step Velocità:",
            preview_filename = "Anteprima Nome File:",
            btn_record = "Registra",
            btn_stop_rec = "Ferma",
            export = "Esportazione",
            btn_csv = "CSV",
            btn_rds = "RDS",
            no_recent_rec = "Nessuna registrazione recente.",
            rec_in_progress = "In corso...",
            rec_completed = "Completata",
            hw_config = "Hardware (NI USB-6009)",
            channels_acq = "Canali da Acquisire:",
            chan_name = "Nome Canale:",
            color = "Colore:",
            offset_v = "Offset (V):",
            zero_live = "Azzera da segnale live",
            zero_off = "Azzera da finestra",
            sample_rate = "Frequenza di Campionamento:",
            view_config = "Visualizzazione",
            time_window = "Finestra Temporale (s):",
            downsample = "Abilita Downsampling Ottimizzato",
            dsp_config = "Elaborazione Segnale (DSP)",
            enable_filters = "Abilita Filtri",
            rm_offset = "Rimuovi Offset (HP 5 Hz)",
            lp_filter = "Filtro Passa-Basso (LP 450 Hz)",
            notch_filter = "Filtro Notch (50 Hz)",
            conditioning = "Condizionamento Segnale:",
            cond_raw = "Segnale Grezzo/Filtrato",
            cond_rect = "Rettificato (Onda Intera)",
            cond_rms = "Inviluppo (Moving RMS)",
            rms_window = "Finestra RMS (ms):",
            status_monitor = "Monitor: ",
            status_record = "Record: ",
            status_freq = "Freq: ",
            samples_buffer = "Campioni Buffer: ",
            dur_rec = "Durata Rec: ",
            waiting_data = "In attesa di dati EMG o monitoraggio disattivato...",
            initial_samples = "Acquisizione campioni iniziali in corso...",
            tempo_s = "Tempo (s)",
            upload_prompt = "Carica un file per configurare i canali.",
            include_analysis = "Includi in analisi",
            upload_file = "Carica File Registrato (CSV / RDS)",
            analysis_offline = "Analisi Offline",
            time_range_off = "Intervallo Temporale da Analizzare (s):",
            metrics_header = "Metriche Totali e Stime (Finestra Selezionata)",
            val_media = "Valore Medio (V)",
            val_rms = "Valore RMS (V)",
            amp_picco = "Ampiezza di Picco (V)",
            iemg = "iEMG (V·s)",
            canale = "Canale",
            upload_first = "Seleziona e carica un file CSV o RDS per visualizzare i dati.",
            daq_success = "Monitoraggio DAQ avviato con successo.",
            daq_stopped = "Monitoraggio interrotto.",
            rec_stopped_warn = "Registrazione terminata.",
            rec_started = "Registrazione avviata:",
            rec_saved = "Registrazione salvata.",
            calib_done = "Calibrazione eseguita per",
            calib_err = "Nessun dato live disponibile per calibrare!",
            calib_off_done = "Calibrazione offline per",
            err_channels = "Seleziona almeno un canale da acquisire!",
            err_monitor_first = "Devi prima avviare il monitoraggio per poter registrare!",
            status_active = "ATTIVO",
            status_inactive = "SPENTO",
            status_recording = "REGISTRAZIONE",
            status_stopped = "FERMO",
            fix_yaxis = "Fissa limiti asse Y",
            yaxis_min = "Min (V):",
            yaxis_max = "Max (V):"
        ),
        en = list(
            title = "Real-Time EMG Acquisition & Analysis System v0.1.0",
            tab_live = "Live Acquisition",
            tab_offline = "Analyze Recorded Files",
            tab_info = "Guide & Info",
            ctrl_panel = "Control Panel",
            daq_monitor = "DAQ Monitoring",
            btn_start = "Start",
            btn_stop = "Stop",
            ucrit_meta = "Trial Metadata (Ucrit)",
            specie = "Species:",
            individuo = "Individual (ID):",
            taglia = "Size (cm):",
            giorno = "Trial Day:",
            step = "Velocity Step:",
            preview_filename = "Filename Preview:",
            btn_record = "Record",
            btn_stop_rec = "Stop",
            export = "Export",
            btn_csv = "CSV",
            btn_rds = "RDS",
            no_recent_rec = "No recent recording.",
            rec_in_progress = "Recording...",
            rec_completed = "Completed",
            hw_config = "Hardware (NI USB-6009)",
            channels_acq = "Channels to Acquire:",
            chan_name = "Channel Name:",
            color = "Color:",
            offset_v = "Offset (V):",
            zero_live = "Zero from live signal",
            zero_off = "Zero from window",
            sample_rate = "Sampling Rate:",
            view_config = "Visualization",
            time_window = "Time Window (s):",
            downsample = "Enable Optimized Downsampling",
            dsp_config = "Signal Processing (DSP)",
            enable_filters = "Enable Filters",
            rm_offset = "Remove Offset (HP 5 Hz)",
            lp_filter = "Low-Pass Filter (LP 450 Hz)",
            notch_filter = "Notch Filter (50 Hz)",
            conditioning = "Signal Conditioning:",
            cond_raw = "Raw/Filtered Signal",
            cond_rect = "Rectified (Full Wave)",
            cond_rms = "Envelope (Moving RMS)",
            rms_window = "RMS Window (ms):",
            status_monitor = "Monitor: ",
            status_record = "Record: ",
            status_freq = "Freq: ",
            samples_buffer = "Buffer Samples: ",
            dur_rec = "Rec Duration: ",
            waiting_data = "Waiting for EMG data or monitoring disabled...",
            initial_samples = "Acquiring initial samples...",
            tempo_s = "Time (s)",
            upload_prompt = "Upload a file to configure channels.",
            include_analysis = "Include in analysis",
            upload_file = "Upload Recorded File (CSV / RDS)",
            analysis_offline = "Offline Analysis",
            time_range_off = "Time Range to Analyze (s):",
            metrics_header = "Overall Metrics & Estimates (Selected Window)",
            val_media = "Mean Value (V)",
            val_rms = "RMS Value (V)",
            amp_picco = "Peak Amplitude (V)",
            iemg = "iEMG (V·s)",
            canale = "Channel",
            upload_first = "Select and upload a CSV or RDS file to view data.",
            daq_success = "DAQ monitoring successfully started.",
            daq_stopped = "Monitoring stopped.",
            rec_stopped_warn = "Recording stopped.",
            rec_started = "Recording started:",
            rec_saved = "Recording saved.",
            calib_done = "Calibration completed for",
            calib_err = "No live data available to calibrate!",
            calib_off_done = "Offline calibration for",
            err_channels = "Select at least one channel to acquire!",
            err_monitor_first = "You must start monitoring first to record!",
            status_active = "ACTIVE",
            status_inactive = "INACTIVE",
            status_recording = "RECORDING",
            status_stopped = "STOPPED",
            fix_yaxis = "Fix Y-Axis Limits",
            yaxis_min = "Min (V):",
            yaxis_max = "Max (V):"
        )
    )
    if (is.null(dict[[lang]][[key]])) {
        return(key)
    }
    dict[[lang]][[key]]
}

# Theme Configuration
custom_theme <- bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#00bc8c",
    secondary = "#375a7f",
    success = "#00bc8c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c"
)

# Static UI structure to prevent tab state loss during language toggle
ui <- page_navbar(
    id = "main_navbar",
    theme = custom_theme,
    title = uiOutput("title_ui", inline = TRUE),
    
    # TAB 1: ACQUISIZIONE LIVE
    nav_panel(
        title = "Acquisizione Live",
        value = "tab_live",
        layout_sidebar(
            sidebar = sidebar(
                title = uiOutput("sidebar_title_ui"),
                
                h6(uiOutput("daq_monitor_lbl", inline = TRUE), class = "mt-1 mb-1 text-muted", style = "font-size: 0.8rem;"),
                layout_column_wrap(
                    width = 1/2,
                    actionButton("btn_start_monitor", "Avvia", class = "btn btn-success btn-sm w-100"),
                    actionButton("btn_stop_monitor", "Ferma", class = "btn btn-danger btn-sm w-100")
                ),
                
                hr(class = "my-1"),
                
                h6(uiOutput("ucrit_meta_lbl", inline = TRUE), class = "mt-1 mb-1 text-warning", style = "font-size: 0.85rem;"),
                textInput("meta_specie", "Specie:", placeholder = "es. Spigola", value = "Mugil"),
                textInput("meta_individuo", "Individuo (ID):", placeholder = "es. Fish_01", value = "001"),
                numericInput("meta_taglia", "Taglia (cm):", value = 20, min = 1, max = 150),
                dateInput("meta_giorno", "Giorno Prova:", value = Sys.Date(), format = "yyyy-mm-dd"),
                textInput("meta_step", "Step Velocità:", placeholder = "es. Step_1", value = "0.1"),
                
                div(
                    style = "font-size: 0.75rem; color: #aaa; margin-bottom: 5px; border: 1px dashed #555; padding: 4px; border-radius: 4px; background: #222;",
                    strong(uiOutput("preview_filename_lbl", inline = TRUE)), br(),
                    textOutput("filename_preview_text")
                ),
                
                layout_column_wrap(
                    width = 1/2,
                    actionButton("btn_start_record", "Registra", class = "btn btn-warning btn-sm w-100"),
                    actionButton("btn_stop_record", "Ferma", class = "btn btn-secondary btn-sm w-100")
                ),
                
                # h6(uiOutput("export_lbl", inline = TRUE), class = "mt-2 mb-1 text-muted", style = "font-size: 0.8rem;"),
                # layout_column_wrap(
                #     width = 1/2,
                #     downloadButton("download_csv", "CSV", class = "btn btn-outline-success btn-sm w-100"),
                #     downloadButton("download_rds", "RDS", class = "btn btn-outline-primary btn-sm w-100")
                # ),
                
                uiOutput("active_record_info_sidebar"),
                
                hr(class = "my-2"),
                
                accordion(
                    open = FALSE,
                    accordion_panel(
                        title = uiOutput("hw_config_lbl", inline = TRUE),
                        value = "panel_hw",
                        checkboxGroupInput(
                            "channels", 
                            "Canali da Acquisire:",
                            choices = c(
                                "ai0" = "Dev1/ai0",
                                "ai1" = "Dev1/ai1",
                                "ai2" = "Dev1/ai2",
                                "ai3" = "Dev1/ai3",
                                "ai4" = "Dev1/ai4",
                                "ai5" = "Dev1/ai5",
                                "ai6" = "Dev1/ai6",
                                "ai7" = "Dev1/ai7"
                            ),
                            selected = c("Dev1/ai0", "Dev1/ai1")
                        ),
                        
                        # Dynamic channel cards
                        uiOutput("channel_name_inputs_ui"),
                        
                        selectInput(
                            "sample_rate", 
                            "Frequenza di Campionamento:",
                            choices = c("500 Hz" = 500, "1000 Hz" = 1000, "2000 Hz" = 2000),
                            selected = 1000
                        )
                    ),
                    
                    accordion_panel(
                        title = uiOutput("view_config_lbl", inline = TRUE),
                        value = "panel_view",
                        selectInput(
                            "window_s", 
                            "Finestra Temporale (s):",
                            choices = c("5 s" = 5, "10 s" = 10, "30 s" = 30, "60 s" = 60),
                            selected = 10
                        ),
                        checkboxInput("downsample", "Abilita Downsampling Ottimizzato", TRUE),
                        checkboxInput("fix_yaxis", "Fissa limiti asse Y", FALSE),
                        conditionalPanel(
                            condition = "input.fix_yaxis",
                            layout_column_wrap(
                                width = 1/2,
                                numericInput("yaxis_min", "Min (V):", value = -1, step = 0.1),
                                numericInput("yaxis_max", "Max (V):", value = 1, step = 0.1)
                            )
                        )
                    ),
                    
                    accordion_panel(
                        title = uiOutput("dsp_config_lbl", inline = TRUE),
                        value = "panel_dsp",
                        checkboxInput("enable_filters", "Abilita Filtri", FALSE),
                        checkboxInput("filter_hp", "Rimuovi Offset (HP 5 Hz)", TRUE),
                        checkboxInput("filter_lp", "Filtro Passa-Basso (LP 450 Hz)", FALSE),
                        checkboxInput("filter_notch", "Filtro Notch (50 Hz)", TRUE),
                        hr(class = "my-1"),
                        selectInput(
                            "conditioning", 
                            "Condizionamento Segnale:",
                            choices = c(
                                "Segnale Grezzo/Filtrato" = "raw",
                                "Rettificato (Onda Intera)" = "rectified",
                                "Inviluppo (Moving RMS)" = "rms"
                            ),
                            selected = "raw"
                        ),
                        conditionalPanel(
                            condition = "input.conditioning == 'rms'",
                            numericInput("rms_window_ms", "Finestra RMS (ms):", value = 50, min = 5, max = 500)
                        )
                    )
                )
            ),
            
            # Main Panel Content (Live Mode)
            div(
                class = "py-1 px-3 mb-2",
                style = "background-color: #2b2b2b; border-radius: 4px; border: 1px solid #444; font-size: 0.85rem; line-height: 24px;",
                fluidRow(
                    column(2, span(strong(uiOutput("status_monitor_lbl", inline = TRUE)), uiOutput("status_monitor", inline = TRUE))),
                    column(2, span(strong(uiOutput("status_record_lbl", inline = TRUE)), uiOutput("status_record", inline = TRUE))),
                    column(2, span(strong(uiOutput("status_freq_lbl", inline = TRUE)), textOutput("active_sr", inline = TRUE))),
                    column(3, span(strong(uiOutput("samples_buffer_lbl", inline = TRUE)), textOutput("live_samples", inline = TRUE))),
                    column(3, span(strong(uiOutput("dur_rec_lbl", inline = TRUE)), textOutput("record_duration", inline = TRUE)))
                )
            ),
            card(
                card_body(
                    class = "p-1",
                    plotlyOutput("emg_plot", height = "calc(100vh - 110px)")
                )
            )
        )
    ),
    
    # TAB 2: ANALISI POST-ELABORAZIONE
    nav_panel(
        title = "Analisi File Registrati",
        value = "tab_offline",
        layout_sidebar(
            sidebar = sidebar(
                title = uiOutput("analysis_offline_lbl"),
                fileInput("upload_file", "Carica File Registrato (CSV / RDS)", accept = c(".csv", ".rds")),
                
                uiOutput("offline_channel_selector"),
                
                uiOutput("offline_time_range_ui"),
                
                numericInput("offline_fs", "Frequenza Campionamento (Hz):", value = 1000, min = 100, max = 10000),
                
                hr(class = "my-2"),
                
                h6(uiOutput("dsp_config_off_lbl", inline = TRUE), style = "font-size: 0.85rem; color: #aaa;"),
                checkboxInput("off_enable_filters", "Abilita Filtri", FALSE),
                checkboxInput("off_filter_hp", "Rimuovi Offset (HP 5 Hz)", TRUE),
                checkboxInput("off_filter_lp", "Filtro Passa-Basso (LP 450 Hz)", FALSE),
                checkboxInput("off_filter_notch", "Filtro Notch (50 Hz)", TRUE),
                hr(class = "my-1"),
                selectInput(
                    "off_conditioning", 
                    "Condizionamento Segnale:",
                    choices = c(
                        "Segnale Grezzo/Filtrato" = "raw",
                        "Rettificato (Onda Intera)" = "rectified",
                        "Inviluppo (Moving RMS)" = "rms"
                    ),
                    selected = "raw"
                ),
                conditionalPanel(
                    condition = "input.off_conditioning == 'rms'",
                    numericInput("off_rms_window_ms", "Finestra RMS (ms):", value = 50, min = 5, max = 500)
                ),
                hr(class = "my-2"),
                checkboxInput("off_fix_yaxis", "Fissa limiti asse Y", FALSE),
                conditionalPanel(
                    condition = "input.off_fix_yaxis",
                    layout_column_wrap(
                        width = 1/2,
                        numericInput("off_yaxis_min", "Min (V):", value = -1, step = 0.1),
                        numericInput("off_yaxis_max", "Max (V):", value = 1, step = 0.1)
                    )
                )
            ),
            
            # Main Panel Content (Offline Mode)
            card(
                card_header(uiOutput("metrics_header_lbl")),
                card_body(
                    class = "py-2 px-3",
                    style = "background-color: #2b2b2b;",
                    tableOutput("offline_metrics_table")
                )
            ),
            card(
                card_body(
                    class = "p-1",
                    plotlyOutput("offline_plot", height = "calc(100vh - 230px)")
                )
            )
        )
    ),
    
    # TAB 3: GUIDA & INFO
    nav_panel(
        title = "Guida & Info",
        value = "tab_info",
        fluidRow(
            column(8,
                card(
                    card_body(
                        uiOutput("manual_ui")
                    )
                )
            ),
            column(4,
                card(
                    card_body(
                        uiOutput("credits_ui")
                    )
                )
            )
        )
    ),
    
    nav_spacer(),
    nav_item(
        selectInput(
            inputId = "lang",
            label = NULL,
            choices = c("🇮🇹 IT" = "it", "🇺🇸 EN" = "en"),
            selected = "it",
            width = "90px"
        )
    ),
    
    header = tags$head(
        # Custom CSS for compact vertical spacing
        tags$style(HTML("
            .shiny-input-container {
                margin-bottom: 4px !important;
            }
            .sidebar .card {
                margin-bottom: 4px !important;
            }
            .sidebar .card-body {
                padding: 4px !important;
            }
            .form-control, .form-select, .form-control-color {
                padding-top: 2px !important;
                padding-bottom: 2px !important;
                height: 28px !important;
                font-size: 0.85rem !important;
            }
            .shiny-date-input input {
                height: 28px !important;
            }
            .accordion-body {
                padding: 6px !important;
            }
            .accordion-button {
                padding: 4px 8px !important;
                font-size: 0.8rem !important;
            }
            hr {
                margin-top: 4px !important;
                margin-bottom: 4px !important;
            }
            h6, .h6 {
                margin-top: 4px !important;
                margin-bottom: 2px !important;
                font-size: 0.85rem !important;
            }
            label {
                margin-bottom: 1px !important;
                font-size: 0.75rem !important;
                font-weight: 500;
            }
            .navbar {
                padding-top: 2px !important;
                padding-bottom: 2px !important;
                min-height: 36px !important;
            }
            .navbar-brand, .nav-link, .nav-item select {
                padding-top: 1px !important;
                padding-bottom: 1px !important;
                font-size: 0.9rem !important;
            }
            .container-fluid {
                padding-top: 1px !important;
                padding-bottom: 1px !important;
            }
            .card {
                margin-bottom: 4px !important;
            }
            .card-body {
                padding: 6px !important;
            }
            html, body {
                height: 100vh !important;
                overflow: hidden !important;
            }
            .tab-content, .tab-pane {
                height: calc(100vh - 42px) !important;
                overflow: hidden !important;
            }
            .sidebar {
                max-height: calc(100vh - 50px) !important;
                overflow-y: auto !important;
            }
        ")),
        tags$script(HTML("
            $(document).on('change', 'input[type=color]', function() {
                var id = $(this).attr('id');
                var val = $(this).val();
                Shiny.setInputValue(id, val);
            });
            $(document).on('shiny:value shiny:bound shiny:connected', function() {
                setTimeout(function() {
                    $('input[type=color]').each(function() {
                        var id = $(this).attr('id');
                        var val = $(this).val();
                        Shiny.setInputValue(id, val);
                    });
                }, 100);
            });
            Shiny.addCustomMessageHandler('update_tab_titles', function(message) {
                $('a[data-value=\"tab_live\"]').text(message.tab_live);
                $('a[data-value=\"tab_offline\"]').text(message.tab_offline);
                $('a[data-value=\"tab_info\"]').text(message.tab_info);
            });
        "))
    )
)

server <- function(input, output, session) {
    
    # -------------------------------------------------------------
    # STATO E CONTROLLI LIVE
    # -------------------------------------------------------------
    monitoring_active <- reactiveVal(FALSE)
    recording_active <- reactiveVal(FALSE)
    
    active_record_file <- reactiveVal(NULL)
    record_start_time <- reactiveVal(NULL)
    
    # Dynamic text outputs for static labels translation
    output$title_ui <- renderUI({ tr("title", input$lang) })
    output$sidebar_title_ui <- renderUI({ tr("ctrl_panel", input$lang) })
    output$daq_monitor_lbl <- renderUI({ tr("daq_monitor", input$lang) })
    output$ucrit_meta_lbl <- renderUI({ tr("ucrit_meta", input$lang) })
    output$preview_filename_lbl <- renderUI({ tr("preview_filename", input$lang) })
    output$export_lbl <- renderUI({ tr("export", input$lang) })
    output$hw_config_lbl <- renderUI({ tr("hw_config", input$lang) })
    output$view_config_lbl <- renderUI({ tr("view_config", input$lang) })
    output$dsp_config_lbl <- renderUI({ tr("dsp_config", input$lang) })
    output$status_monitor_lbl <- renderUI({ tr("status_monitor", input$lang) })
    output$status_record_lbl <- renderUI({ tr("status_record", input$lang) })
    output$status_freq_lbl <- renderUI({ tr("status_freq", input$lang) })
    output$samples_buffer_lbl <- renderUI({ tr("samples_buffer", input$lang) })
    output$dur_rec_lbl <- renderUI({ tr("dur_rec", input$lang) })
    
    output$analysis_offline_lbl <- renderUI({ tr("analysis_offline", input$lang) })
    output$dsp_config_off_lbl <- renderUI({ tr("dsp_config", input$lang) })
    output$metrics_header_lbl <- renderUI({ tr("metrics_header", input$lang) })
    
    output$manual_ui <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (lang == "it") {
            tagList(
                div(
                    style = "max-height: calc(100vh - 165px); overflow-y: auto; padding-right: 15px;",
                    h4("Manuale d'Uso Dettagliato del Sistema EMG", class = "text-success"),
                    p("Questo software è progettato per l'acquisizione in tempo reale, il filtraggio digitale e l'analisi offline di segnali elettromiografici (EMG) biologici, specificamente ottimizzato per studi di fisiologia animale e protocolli di nuoto critico (Ucrit) su pesci."),
                    
                    h5("1. Introduzione Fisiologica e Scopo dell'App", class = "text-info mt-3"),
                    p("L'elettromiografia (EMG) rileva l'attività elettrica generata dalla depolarizzazione delle membrane delle fibre muscolari durante la contrazione. Nelle prove Ucrit condotte in tunnel di nuoto (es. respirometri), il pesce nuota contro corrente a velocità crescenti per step temporali. Il monitoraggio EMG di due o più canali consente di:"),
                    tags$ul(
                        tags$li(strong("Identificare le fibre muscolari attive:"), " Distinguere tra il reclutamento delle fibre rosse (aerobiche, deputate al nuoto lento e costante) e delle fibre bianche (anaerobiche, deputate a scatti veloci e nuoto vigoroso oltre la soglia critica)."),
                        tags$li(strong("Rilevare la fatica:"), " Individuare il momento esatto in cui il pesce esaurisce il metabolismo aerobico e recluta massivamente le fibre bianche per mantenere lo step di velocità."),
                        tags$li(strong("Calcolare lo sforzo totale:"), " Stimare il lavoro muscolare complessivo espresso tramite l'indice iEMG (EMG integrato).")
                    ),
                    
                    h5("2. Architettura Hardware e Acquisizione", class = "text-info mt-3"),
                    p("L'applicazione Shiny (interfaccia utente R) comunica in background con un daemon Python che gestisce direttamente la scheda di acquisizione ", strong("National Instruments NI USB-6009"), " tramite la libreria ufficiale NI-DAQmx."),
                    tags$ul(
                        tags$li(strong("Configurazione RSE:"), " L'acquisizione dei canali analogici (fino a 8 ingressi, da ai0 a ai7) avviene in modalità Referenced Single-Ended (RSE), ovvero riferita a un potenziale di terra comune (GND)."),
                        tags$li(strong("Frequenza di Campionamento:"), " Selezionabile tra 500, 1000 (predefinita) o 2000 Hz per soddisfare il teorema del campionamento di Nyquist-Shannon in relazione alle bande dell'EMG biologico.")
                    ),
                    
                    h5("3. Calibrazione e Compensazione dell'Offset", class = "text-info mt-3"),
                    p("La scheda NI USB-6009 in configurazione RSE presenta tipicamente una tensione residua di fondo (DC Offset) pari a circa +1.4V, indotta dall'elettronica interna e dal circuito biologico di bianco. Per correggere questo valore:"),
                    tags$ol(
                        tags$ol(
                            tags$li("Avviare il monitoraggio cliccando su ", strong("Avvia"), " (il LED di stato diventerà verde ed evidenzierà 'ATTIVO')."),
                            tags$li("A pesce fermo (condizione di riposo/bianco), osservare la tensione media del segnale."),
                            tags$li("Cliccare sul pulsante ", strong("Azzera da segnale live"), " sotto la scheda del canale corrispondente per calibrare: l'app calcolerà la media in tempo reale e la imposterà come Offset, sottraendola istantaneamente dal grafico per centrarlo sullo zero.")
                        )
                    ),
                    
                    h5("4. Registrazione e Esportazione Dati", class = "text-info mt-3"),
                    p("Il sistema garantisce che la registrazione contenga esclusivamente i dati acquisiti tra l'inizio e la fine dell'esperimento, evitando file pesanti e non pertinenti:"),
                    tags$ul(
                        tags$li(strong("Metadati Ucrit (Nome File):"), " Il nome del file finale viene autogenerato in modo normalizzato combinando i campi: Specie, Individuo (ID), Taglia, Data e Step Velocità (es. ", code("EMG_20260624_sp_ind1_20cm_step1.csv"), "). Ciò previene sovrascritture accidentali."),
                        tags$li(strong("Formato di Esportazione:"), " Tutti i dati vengono salvati usando il separatore di colonna punto e virgola (", code(";"), ") e il separatore decimale punto (", code("."), "). Cliccando su 'Ferma' (Registrazione), il file CSV viene consolidato e ne viene generata una copia binaria RDS ad alta velocità di caricamento."),
                        tags$li(strong("Struttura dei file:"), " Il file salvato contiene una colonna temporale relativa ", code("time_s"), " (che parte da 0.0s all'inizio della registrazione), una colonna ", code("clock_time"), " in formato ora civile precisa (HH:MM:SS.FFF) e le colonne dei singoli canali con i valori di tensione calibrati.")
                    ),
                    
                    h5("5. Descrizione Dettagliata dei Filtri Digitali (DSP)", class = "text-info mt-3"),
                    p("I filtri digitali in tempo reale (DSP) puliscono il segnale da rumori e interferenze prima della visualizzazione e del calcolo metrico:"),
                    tags$ul(
                        tags$li(strong("Passa-Alto (High-Pass 5 Hz):"), " Rimuove la componente continua (offset DC) e le bassissime frequenze dovute ai movimenti macroscopici dell'animale o ai lenti spostamenti fisici dell'elettrodo (artefatti da movimento). Include un sistema di pre-padding del segnale di 0.2 secondi per eliminare qualsiasi transitorio di avvio del filtro a t=0."),
                        tags$li(strong("Notch 50 Hz:"), " Filtro a banda strettissima incentrato sui 50 Hz. Rimuove selettivamente il rumore di alternata indotto dalla rete elettrica civile (lampade, alimentatori, spine circostanti)."),
                        tags$li(strong("Passa-Basso (Low-Pass 450 Hz):"), " Attenua le frequenze superiori a 450 Hz, eliminando il rumore bianco elettronico dell'amplificatore e le radiofrequenze ambientali che non contengono segnale biologico utile.")
                    ),
                    
                    h5("6. Metriche e Analisi Post-Elaborazione (Offline)", class = "text-info mt-3"),
                    p("Nella seconda scheda, è possibile caricare una registrazione precedente (.csv o .rds) per analizzarla offline:"),
                    tags$ul(
                        tags$li(strong("Slider Temporale:"), " Consente di tagliare graficamente ed esaminare una precisa porzione di tempo dell'esperimento. I grafici e le tabelle si adatteranno istantaneamente."),
                        tags$li(strong("Valore Medio (V):"), " La tensione media registrata nel canale (utile per verificare offset residui)."),
                        tags$li(strong("Valore RMS (V):"), " Root Mean Square. Calcola l'ampiezza efficace del segnale elettrico, correlata con l'intensità della contrazione muscolare."),
                        tags$li(strong("Ampiezza di Picco (V):"), " Il valore massimo assoluto registrato, utile per identificare scatti improvvisi del pesce."),
                        tags$li(strong("iEMG (Volt-secondi, V·s):"), " L'EMG Integrato. Rappresenta l'area sottesa al segnale EMG rettificato nel tempo. È il parametro cumulativo fondamentale che misura l'energia elettrica complessiva spesa dal muscolo nella finestra considerata.")
                    ),
                    
                    h5("7. Requisiti di Sistema e Avvio (RStudio & Python)", class = "text-info mt-3"),
                    p("Per far funzionare correttamente l'intero sistema (interfaccia e acquisizione hardware), assicurarsi di soddisfare i seguenti requisiti:"),
                    tags$ul(
                        tags$li(strong("RStudio / R:"), " L'interfaccia utente è scritta in R Shiny. Prima di avviarla per la prima volta, installare i pacchetti necessari eseguendo in RStudio: ", code("install.packages(c('shiny', 'bslib', 'plotly', 'tidyverse'))"), ". Per avviare l'applicazione, aprire RStudio, impostare la directory di lavoro sulla cartella ", code("EMG"), " ed eseguire il comando ", code("shiny::runApp()"), "."),
                        tags$li(strong("Python:"), " L'acquisizione dei dati in tempo reale dalla scheda NI USB-6009 avviene tramite un daemon Python in background (", code("daq_emg_stream.py"), "). È necessario che Python 3 sia installato sul sistema e configurato nel PATH di Windows. Inoltre, devono essere installate le librerie Python per l'interfacciamento con la scheda NI e per il calcolo scientifico: ", code("pip install nidaqmx numpy"), "."),
                        tags$li(strong("Driver National Instruments:"), " La scheda NI USB-6009 richiede l'installazione dei driver ufficiali ", strong("NI-DAQmx"), " forniti da National Instruments sul computer host.")
                    )
                )
            )
        } else {
            tagList(
                div(
                    style = "max-height: calc(100vh - 165px); overflow-y: auto; padding-right: 15px;",
                    h4("Detailed EMG System User Manual", class = "text-success"),
                    p("This software is designed for real-time acquisition, digital filtering, and offline analysis of biological electromyographic (EMG) signals, specifically optimized for animal physiology and critical swimming speed (Ucrit) fish protocols."),
                    
                    h5("1. Physiological Background & Purpose", class = "text-info mt-3"),
                    p("Electromyography (EMG) detects the electrical activity generated by muscle fiber membrane depolarization during contraction. In Ucrit trials conducted inside swim tunnels (e.g. respirometers), the fish swims against a water current that increases in speed steps. EMG monitoring of two or more channels allows to:"),
                    tags$ul(
                        tags$li(strong("Identify active muscle fibers:"), " Distinguish between the recruitment of red fibers (aerobic, used for slow and steady swimming) and white fibers (anaerobic, recruited for burst swimming beyond the critical speed threshold)."),
                        tags$li(strong("Detect fatigue:"), " Find the exact moment when the fish exhausts its aerobic capacity and recruits white muscle fibers to keep pace with the current."),
                        tags$li(strong("Estimate muscle work:"), " Quantify overall muscle effort via the Integrated EMG (iEMG) index.")
                    ),
                    
                    h5("2. Hardware Architecture & Acquisition", class = "text-info mt-3"),
                    p("The R Shiny front-end UI communicates in the background with a Python daemon that directly interfaces with the ", strong("National Instruments NI USB-6009"), " DAQ card using the official NI-DAQmx driver."),
                    tags$ul(
                        tags$li(strong("RSE Configuration:"), " Analog channels (up to 8, from ai0 to ai7) are acquired in Referenced Single-Ended (RSE) mode, relative to a common ground potential (GND)."),
                        tags$li(strong("Sampling Rate:"), " Choose between 500, 1000 (default), or 2000 Hz to satisfy the Nyquist-Shannon theorem relative to biological EMG frequency bands.")
                    ),
                    
                    h5("3. Calibration & Offset Compensation", class = "text-info mt-3"),
                    p("The NI USB-6009 DAQ card in RSE configuration usually exhibits a residual baseline voltage (DC Offset) of about +1.4V due to internal electronics. To calibrate:"),
                    tags$ol(
                        tags$li("Start monitoring by clicking ", strong("Start"), " (the status indicator turns green, showing 'ACTIVE')."),
                        tags$li("While the fish is resting (resting/baseline condition), observe the average signal voltage."),
                        tags$li("Click ", strong("Zero from live signal"), " under the corresponding channel card: the app calculates the average live voltage and subtracts it as an Offset, instantly centering the plot on zero.")
                    ),
                    
                    h5("4. Data Recording & Exporting", class = "text-info mt-3"),
                    p("The system ensures that the recording contains only the data acquired between the start and stop triggers, avoiding heavy and non-relevant files:"),
                    tags$ul(
                        tags$li(strong("Ucrit Metadata (File Name):"), " The output file name is automatically generated by combining Ucrit fields: Species, Fish ID, Size, Date, and Speed Step (e.g. ", code("EMG_20260624_sp_ind1_20cm_step1.csv"), ")."),
                        tags$li(strong("Export Format:"), " Data is written using a semicolon (", code(";"), ") column separator and a dot (", code("."), ") decimal separator. Stopping the recording consolidates the CSV file and generates an RDS binary copy for high-speed loading."),
                        tags$li(strong("File Structure:"), " The saved file contains a relative time column ", code("time_s"), " (starting from 0.0s at recording start), a ", code("clock_time"), " column in HH:MM:SS.FFF format, and the voltage values for each active channel.")
                    ),
                    
                    h5("5. Detailed DSP Filter Descriptions", class = "text-info mt-3"),
                    p("Real-time digital signal processing (DSP) filters clean the signal from ambient noises and interferences before plotting and calculations:"),
                    tags$ul(
                        tags$li(strong("High-Pass Filter (5 Hz):"), " Removes the DC offset component and low frequencies caused by gross animal movements or electrode wire sway (motion artifacts). It includes a 0.2-second pre-padding system to completely eliminate any filter startup transient at t=0."),
                        tags$li(strong("Notch Filter (50 Hz):"), " A narrow-band filter centered at 50 Hz. It selectively removes electrical AC power line interference (hum from nearby lights, plugs, and adapters)."),
                        tags$li(strong("Low-Pass Filter (450 Hz):"), " Attenuates frequencies above 450 Hz, removing white electronic noise from amplifiers and ambient radiofrequencies that carry no physiological information.")
                    ),
                    
                    h5("6. Metrics & Offline Post-Processing", class = "text-info mt-3"),
                    p("In the second tab, you can load a previously recorded file (.csv or .rds) to analyze it offline:"),
                    tags$ul(
                        tags$li(strong("Time Range Slider:"), " Graphically crop and inspect a precise portion of the experiment. Charts and metrics table will adapt instantly."),
                        tags$li(strong("Mean Value (V):"), " The mean voltage recorded in the channel (useful to verify residual offsets)."),
                        tags$li(strong("RMS Value (V):"), " Root Mean Square. Calculates the effective amplitude of the electrical signal, which is directly correlated with muscle contraction intensity."),
                        tags$li(strong("Peak Amplitude (V):"), " The absolute maximum value recorded, useful to identify sudden burst movements."),
                        tags$li(strong("iEMG (Volt-seconds, V·s):"), " Integrated EMG. Represents the area under the rectified EMG signal over time. It is the fundamental cumulative parameter used to quantify the total electrical energy spent by the muscle in the selected window.")
                    ),
                    
                    h5("7. System Requirements & Execution (RStudio & Python)", class = "text-info mt-3"),
                    p("To ensure the proper functioning of the acquisition interface and hardware streaming, verify the following prerequisites:"),
                    tags$ul(
                        tags$li(strong("RStudio / R:"), " The graphical interface is built with R Shiny. Before running the application for the first time, install the required packages in RStudio: ", code("install.packages(c('shiny', 'bslib', 'plotly', 'tidyverse'))"), ". To launch the app, open RStudio, set the working directory to the ", code("EMG"), " folder, and execute ", code("shiny::runApp()"), "."),
                        tags$li(strong("Python:"), " Real-time streaming from the NI USB-6009 card is powered by a background Python daemon (", code("daq_emg_stream.py"), "). Python 3 must be installed on your system and added to the Windows PATH. Additionally, install the NI-DAQmx and NumPy libraries using: ", code("pip install nidaqmx numpy"), "."),
                        tags$li(strong("National Instruments Drivers:"), " The NI USB-6009 card requires the official ", strong("NI-DAQmx"), " drivers installed on the host computer.")
                    )
                )
            )
        }
    })
    
    output$credits_ui <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (lang == "it") {
            tagList(
                h4("Crediti & Contatti", class = "text-warning"),
                hr(),
                p(strong("Autore Scientifico:"), br(), "Walter Zupa"),
                p(strong("Email di Contatto:"), br(), tags$a(href = "mailto:zupa@fondazionecoispa.org", "zupa@fondazionecoispa.org")),
                p(strong("Ente di Affiliazione:"), br(), "Fondazione COISPA ETS"),
                p(strong("Versione Software:"), br(), "0.1.0")
            )
        } else {
            tagList(
                h4("Credits & Contacts", class = "text-warning"),
                hr(),
                p(strong("Scientific Author:"), br(), "Walter Zupa"),
                p(strong("Contact Email:"), br(), tags$a(href = "mailto:zupa@fondazionecoispa.org", "zupa@fondazionecoispa.org")),
                p(strong("Affiliation:"), br(), "Fondazione COISPA ETS"),
                p(strong("Software Version:"), br(), "0.1.0")
            )
        }
    })
    
    # Dynamic translation observer for inputs
    observeEvent(input$lang, {
        lang <- input$lang
        
        session$sendCustomMessage("update_tab_titles", list(
            tab_live = tr("tab_live", lang),
            tab_offline = tr("tab_offline", lang),
            tab_info = tr("tab_info", lang)
        ))
        
        updateTextInput(session, "meta_specie", label = tr("specie", lang))
        updateTextInput(session, "meta_individuo", label = tr("individuo", lang))
        updateNumericInput(session, "meta_taglia", label = tr("taglia", lang))
        updateDateInput(session, "meta_giorno", label = tr("giorno", lang))
        updateTextInput(session, "meta_step", label = tr("step", lang))
        
        updateActionButton(session, "btn_start_monitor", label = tr("btn_start", lang))
        updateActionButton(session, "btn_stop_monitor", label = tr("btn_stop", lang))
        updateActionButton(session, "btn_start_record", label = tr("btn_record", lang))
        updateActionButton(session, "btn_stop_record", label = tr("btn_stop_rec", lang))
        
        updateCheckboxGroupInput(session, "channels", label = tr("channels_acq", lang))
        updateSelectInput(session, "sample_rate", label = tr("sample_rate", lang))
        updateSelectInput(session, "window_s", label = tr("time_window", lang))
        updateCheckboxInput(session, "downsample", label = tr("downsample", lang))
        updateCheckboxInput(session, "fix_yaxis", label = tr("fix_yaxis", lang))
        updateNumericInput(session, "yaxis_min", label = tr("yaxis_min", lang))
        updateNumericInput(session, "yaxis_max", label = tr("yaxis_max", lang))
        
        updateCheckboxInput(session, "enable_filters", label = tr("enable_filters", lang))
        updateCheckboxInput(session, "filter_hp", label = tr("rm_offset", lang))
        updateCheckboxInput(session, "filter_lp", label = tr("lp_filter", lang))
        updateCheckboxInput(session, "filter_notch", label = tr("notch_filter", lang))
        updateSelectInput(session, "conditioning", label = tr("conditioning", lang))
        updateNumericInput(session, "rms_window_ms", label = tr("rms_window", lang))
        
        updateNumericInput(session, "offline_fs", label = tr("sample_rate", lang))
        updateCheckboxInput(session, "off_enable_filters", label = tr("enable_filters", lang))
        updateCheckboxInput(session, "off_filter_hp", label = tr("rm_offset", lang))
        updateCheckboxInput(session, "off_filter_lp", label = tr("lp_filter", lang))
        updateCheckboxInput(session, "off_filter_notch", label = tr("notch_filter", lang))
        updateSelectInput(session, "off_conditioning", label = tr("conditioning", lang))
        updateNumericInput(session, "off_rms_window_ms", label = tr("rms_window", lang))
        updateCheckboxInput(session, "off_fix_yaxis", label = tr("fix_yaxis", lang))
        updateNumericInput(session, "off_yaxis_min", label = tr("yaxis_min", lang))
        updateNumericInput(session, "off_yaxis_max", label = tr("yaxis_max", lang))
    })
    
    # Generate UI custom configurations (Name, Color, Offset, Calibration) per active channel
    output$channel_name_inputs_ui <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        chans <- input$channels
        if (length(chans) == 0) return(NULL)
        
        default_colors <- c("#ff0000", "#0070ff", "#f39c12", "#00bc8c", "#9b59b6", "#1abc9c", "#d35400", "#34495e")
        
        inputs <- lapply(seq_along(chans), function(i) {
            chan <- chans[i]
            chan_clean <- gsub("/", "_", chan)
            
            card(
                class = "p-2 mb-1",
                style = "border: 1px solid #444; background-color: #222;",
                p(strong(paste("Canale:", chan)), class = "mb-1 text-info", style = "font-size: 0.85rem;"),
                textInput(
                    inputId = paste0("custom_name_", chan_clean),
                    label = tr("chan_name", lang),
                    value = gsub(".*/", "", chan)
                ),
                fluidRow(
                    column(6, 
                           div(
                               class = "form-group shiny-input-container",
                               tags$label(tr("color", lang), `for` = paste0("color_", chan_clean), style = "font-size: 0.75rem;"),
                               tags$input(id = paste0("color_", chan_clean), type = "color", 
                                          value = default_colors[((i-1) %% length(default_colors)) + 1], 
                                          class = "form-control form-control-sm w-100", style = "height: 30px; padding: 0;")
                           )
                    ),
                    column(6,
                           numericInput(
                               inputId = paste0("offset_", chan_clean),
                               label = tr("offset_v", lang),
                               value = 0.0,
                               step = 0.01
                           )
                    )
                ),
                actionButton(
                    inputId = paste0("btn_calib_", chan_clean),
                    label = tr("zero_live", lang),
                    class = "btn btn-outline-info btn-xs w-100 mt-1",
                    style = "font-size: 0.7rem; padding: 2px 5px;"
                )
            )
        })
        do.call(tagList, inputs)
    })
    
    # Observe dynamic live calibration buttons
    observe({
        chans <- input$channels
        if (length(chans) == 0) return()
        
        for (chan in chans) {
            chan_clean <- gsub("/", "_", chan)
            btn_id <- paste0("btn_calib_", chan_clean)
            
            local({
                c_clean <- chan_clean
                b_id <- btn_id
                c_name <- gsub(".*/", "", chan)
                
                observeEvent(input[[b_id]], {
                    lang <- if (!is.null(input$lang)) input$lang else "it"
                    dat <- emg_data()
                    custom_name <- input[[paste0("custom_name_", c_clean)]]
                    if (is.null(custom_name) || trimws(custom_name) == "") {
                        col_to_check <- c_name
                    } else {
                        col_to_check <- clean_filename_part(custom_name)
                    }
                    
                    if (!is.null(dat) && col_to_check %in% names(dat)) {
                        mean_val <- mean(dat[[col_to_check]], na.rm = TRUE)
                        updateNumericInput(session, paste0("offset_", c_clean), value = round(mean_val, 4))
                        showNotification(paste(tr("calib_done", lang), col_to_check, ":", round(mean_val, 4), "V"), type = "message")
                    } else {
                        showNotification(tr("calib_err", lang), type = "error")
                    }
                }, ignoreInit = TRUE)
            })
        }
    })
    
    # Dynamic preview of the customized filename
    filename_preview <- reactive({
        specie <- clean_filename_part(input$meta_specie)
        individuo <- clean_filename_part(input$meta_individuo)
        taglia <- if(is.na(input$meta_taglia)) "NA" else paste0(input$meta_taglia, "cm")
        giorno <- format(input$meta_giorno, "%Y%m%d")
        step <- clean_filename_part(input$meta_step)
        
        paste0("EMG_", giorno, "_", specie, "_", individuo, "_", taglia, "_", step, ".csv")
    })
    
    output$filename_preview_text <- renderText({
        filename_preview()
    })
    
    # 1. Start/Stop Monitoraggio
    observeEvent(input$btn_start_monitor, {
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (monitoring_active()) return()
        if (length(input$channels) == 0) {
            showNotification(tr("err_channels", lang), type = "error")
            return()
        }
        
        writeLines("run", stop_file)
        if (file.exists(data_file)) try(file.remove(data_file), silent = TRUE)
        if (file.exists(record_control_file)) try(file.remove(record_control_file), silent = TRUE)
        
        # Collect custom channel names
        custom_names_vector <- sapply(input$channels, function(chan) {
            chan_clean <- gsub("/", "_", chan)
            val <- input[[paste0("custom_name_", chan_clean)]]
            if (is.null(val) || trimws(val) == "") {
                return(gsub(".*/", "", chan))
            }
            clean_filename_part(val)
        })
        channel_names_str <- paste(custom_names_vector, collapse = ",")
        
        channels_str <- paste(input$channels, collapse = ",")
        args <- c(
            shQuote(script_path, type = "cmd"),
            "--sample-rate", input$sample_rate,
            "--channels", shQuote(channels_str, type = "cmd"),
            "--channel-names", shQuote(channel_names_str, type = "cmd"),
            "--live-output", shQuote(data_file, type = "cmd"),
            "--stop-file", shQuote(stop_file, type = "cmd"),
            "--record-control", shQuote(record_control_file, type = "cmd")
        )
        
        log_file <- file.path(app_dir, "python_daq.log")
        if (file.exists(log_file)) try(file.remove(log_file), silent = TRUE)
        
        system2(
            py_exe,
            args = args,
            wait = FALSE,
            stdout = log_file,
            stderr = log_file
        )
        
        monitoring_active(TRUE)
        showNotification(tr("daq_success", lang), type = "message")
    })
    
    observeEvent(input$btn_stop_monitor, {
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (!monitoring_active()) return()
        
        if (recording_active()) {
            recording_active(FALSE)
            writeLines("stop", record_control_file)
            showNotification(tr("rec_stopped_warn", lang), type = "warning")
        }
        
        writeLines("stop", stop_file)
        monitoring_active(FALSE)
        showNotification(tr("daq_stopped", lang), type = "warning")
    })
    
    # 2. Start/Stop Registrazione
    observeEvent(input$btn_start_record, {
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (!monitoring_active()) {
            showNotification(tr("err_monitor_first", lang), type = "error")
            return()
        }
        if (recording_active()) return()
        
        filename_custom <- filename_preview()
        filename_abs <- file.path(app_dir, filename_custom)
        
        # Se il file esiste già, aggiungi _01, _02, ecc.
        if (file.exists(filename_abs)) {
            filename_base <- gsub("\\.csv$", "", filename_custom)
            counter <- 1
            while (TRUE) {
                suffix <- sprintf("_%02d", counter)
                temp_filename <- paste0(filename_base, suffix, ".csv")
                temp_abs <- file.path(app_dir, temp_filename)
                if (!file.exists(temp_abs)) {
                    filename_custom <- temp_filename
                    filename_abs <- temp_abs
                    break
                }
                counter <- counter + 1
            }
        }
        
        active_record_file(filename_abs)
        record_start_time(Sys.time())
        
        writeLines(filename_abs, record_control_file)
        
        recording_active(TRUE)
        showNotification(paste(tr("rec_started", lang), filename_custom), type = "warning")
    })
    
    observeEvent(input$btn_stop_record, {
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (!recording_active()) return()
        
        writeLines("stop", record_control_file)
        recording_active(FALSE)
        showNotification(tr("rec_saved", lang), type = "message")
        
        csv_path <- active_record_file()
        if (!is.null(csv_path) && file.exists(csv_path)) {
            try({
                Sys.sleep(0.5)
                # Read with semicolon delimiter
                df <- read_delim(csv_path, delim = ";", show_col_types = FALSE)
                rds_path <- gsub("\\.csv$", ".rds", csv_path)
                saveRDS(df, rds_path)
            }, silent = TRUE)
        }
    })
    
    # Fast reactive file reader (Semicolon Delimited)
    emg_data <- reactiveFileReader(
        intervalMillis = 300,
        session = session,
        filePath = data_file,
        readFunc = function(file) {
            if (!file.exists(file)) return(NULL)
            tryCatch({
                read_delim(file, delim = ";", show_col_types = FALSE)
            }, error = function(e) NULL)
        }
    )
    
    processed_data <- reactive({
        dat <- emg_data()
        if (is.null(dat) || nrow(dat) < 10) return(NULL)
        
        fs <- as.numeric(input$sample_rate)
        cols <- setdiff(names(dat), c("time_s", "clock_time"))
        
        # 1. Apply Offset Calibration FIRST
        for (col in cols) {
            idx <- match(col, cols)
            if (!is.na(idx) && idx <= length(input$channels)) {
                chan_clean <- gsub("/", "_", input$channels[idx])
                offset_val <- input[[paste0("offset_", chan_clean)]]
                if (is.null(offset_val)) offset_val <- 0
                dat[[col]] <- dat[[col]] - offset_val
            }
        }
        
        # 2. Apply filters
        if (input$enable_filters) {
            for (col in cols) {
                y <- dat[[col]]
                if (input$filter_hp) y <- filter_highpass(y, fs = fs, fc = 5)
                if (input$filter_notch) y <- filter_notch(y, fs = fs, f0 = 50)
                if (input$filter_lp) y <- filter_lowpass(y, fs = fs, fc = 450)
                
                if (input$conditioning == "rectified") {
                    y <- abs(y)
                } else if (input$conditioning == "rms") {
                    y <- moving_rms(y, window_ms = input$rms_window_ms, fs = fs)
                }
                dat[[col]] <- y
            }
        } else {
            if (input$conditioning == "rectified") {
                for (col in cols) dat[[col]] <- abs(dat[[col]])
            } else if (input$conditioning == "rms") {
                for (col in cols) dat[[col]] <- moving_rms(dat[[col]], window_ms = input$rms_window_ms, fs = fs)
            }
        }
        
        max_t <- max(dat$time_s, na.rm = TRUE)
        window_size <- as.numeric(input$window_s)
        dat <- dat[dat$time_s >= max_t - window_size, ]
        
        if (input$downsample && nrow(dat) > 1000) {
            decimation_factor <- ceiling(nrow(dat) / 1000)
            dat <- dat[seq(1, nrow(dat), by = decimation_factor), ]
        }
        dat
    })
    
    # Status Displays
    output$status_monitor <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (monitoring_active()) {
            span(tr("status_active", lang), class = "text-success", style = "font-weight: bold;")
        } else {
            span(tr("status_inactive", lang), class = "text-danger", style = "font-weight: bold;")
        }
    })
    
    output$status_record <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        if (recording_active()) {
            span(tr("status_recording", lang), class = "text-warning", style = "font-weight: bold;")
        } else {
            span(tr("status_stopped", lang), class = "text-muted", style = "font-weight: bold;")
        }
    })
    
    output$active_sr <- renderText({
        paste(input$sample_rate, "Hz")
    })
    output$live_samples <- renderText({
        dat <- emg_data()
        if (is.null(dat)) "0" else format(nrow(dat), big.mark = ".")
    })
    output$record_duration <- renderText({
        if (!recording_active() || is.null(record_start_time())) {
            "00:00:00"
        } else {
            invalidateLater(1000, session)
            diff_secs <- as.numeric(difftime(Sys.time(), record_start_time(), units = "secs"))
            hours <- floor(diff_secs / 3600)
            mins <- floor((diff_secs %% 3600) / 60)
            secs <- floor(diff_secs %% 60)
            sprintf("%02d:%02d:%02d", hours, mins, secs)
        }
    })
    
    # Active recording info display in sidebar
    output$active_record_info_sidebar <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        f <- active_record_file()
        if (is.null(f)) {
            div(style = "font-size: 0.8rem; margin-top: 10px;", tr("no_recent_rec", lang))
        } else {
            status_text <- if (recording_active()) {
                span(tr("rec_in_progress", lang), class = "badge bg-warning")
            } else {
                span(tr("rec_completed", lang), class = "badge bg-success")
            }
            div(
                style = "font-size: 0.8rem; margin-top: 10px; border: 1px solid #444; padding: 5px; border-radius: 4px; background: #222;",
                strong("File:"), br(), code(basename(f)), br(),
                strong("Status:"), status_text
            )
        }
    })
    
    # Live Plots Rendering with custom colors
    output$emg_plot <- renderPlotly({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        dat <- processed_data()
        validate(
            need(!is.null(dat), tr("waiting_data", lang)),
            need(nrow(dat) > 5, tr("initial_samples", lang))
        )
        default_colors <- c("#ff0000", "#0070ff", "#f39c12", "#00bc8c", "#9b59b6", "#1abc9c", "#d35400", "#34495e")
        cols <- setdiff(names(dat), c("time_s", "clock_time"))
        plots <- lapply(cols, function(col) {
            idx <- match(col, cols)
            color_val <- if (!is.na(idx)) default_colors[((idx-1) %% length(default_colors)) + 1] else "#00bc8c"
            if (!is.na(idx) && idx <= length(input$channels)) {
                chan_clean <- gsub("/", "_", input$channels[idx])
                color_picker_val <- input[[paste0("color_", chan_clean)]]
                if (!is.null(color_picker_val)) color_val <- color_picker_val
            }
            
            yaxis_opts <- list(title = paste(col, "(V)"))
            if (isTRUE(input$fix_yaxis)) {
                ymin <- if (is.numeric(input$yaxis_min)) input$yaxis_min else -1
                ymax <- if (is.numeric(input$yaxis_max)) input$yaxis_max else 1
                yaxis_opts$range <- c(ymin, ymax)
            }
            
            plot_ly(dat, x = ~time_s, y = as.formula(paste0("~`", col, "`")), 
                    type = 'scatter', mode = 'lines',
                    line = list(width = 1.2, color = color_val), name = col) %>%
                layout(
                    yaxis = yaxis_opts,
                    xaxis = list(title = tr("tempo_s", lang))
                )
        })
        subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
            layout(
                hovermode = "x unified",
                legend = list(orientation = 'h', y = 1.15),
                margin = list(t = 35, b = 30, l = 50, r = 20)
            ) %>%
            config(displayModeBar = FALSE)
    })
    
    # Downloads
    output$download_csv <- downloadHandler(
        filename = function() {
            f <- active_record_file()
            if (is.null(f)) "emg_recording.csv" else basename(f)
        },
        content = function(file) {
            f <- active_record_file()
            validate(need(!is.null(f) && file.exists(f), "File di registrazione non trovato."))
            file.copy(f, file)
        }
    )
    output$download_rds <- downloadHandler(
        filename = function() {
            f <- active_record_file()
            if (is.null(f)) "emg_recording.rds" else gsub("\\.csv$", ".rds", basename(f))
        },
        content = function(file) {
            f <- active_record_file()
            validate(need(!is.null(f), "Nessuna registrazione recente."))
            rds_path <- gsub("\\.csv$", ".rds", f)
            validate(need(file.exists(rds_path), "File RDS non generato."))
            file.copy(rds_path, file)
        }
    )
    
    # -------------------------------------------------------------
    # POST-ELABORAZIONE / CARICAMENTO FILE
    # -------------------------------------------------------------
    loaded_raw_data <- reactive({
        file_info <- input$upload_file
        if (is.null(file_info)) return(NULL)
        
        tryCatch({
            ext <- tools::file_ext(file_info$datapath)
            if (ext == "csv") {
                read_delim(file_info$datapath, delim = ";", show_col_types = FALSE)
            } else if (ext == "rds") {
                readRDS(file_info$datapath)
            } else {
                showNotification("Formato file non supportato. Usa CSV o RDS.", type = "error")
                NULL
            }
        }, error = function(e) {
            showNotification(paste("Errore nel caricamento del file:", e$message), type = "error")
            NULL
        })
    })
    
    # Render channel selectors + color pickers + offset configuration per loaded channel
    output$offline_channel_selector <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        dat <- loaded_raw_data()
        if (is.null(dat)) return(p(tr("upload_prompt", lang), class = "text-muted"))
        
        cols <- setdiff(names(dat), c("time_s", "clock_time"))
        default_colors <- c("#ff0000", "#0070ff", "#f39c12", "#00bc8c", "#9b59b6", "#1abc9c", "#d35400", "#34495e")
        
        inputs <- lapply(seq_along(cols), function(i) {
            col <- cols[i]
            col_clean <- gsub("[^a-zA-Z0-9_-]", "_", col)
            
            card(
                class = "p-2 mb-1",
                style = "border: 1px solid #444; background-color: #222;",
                p(strong(col), class = "mb-1 text-info", style = "font-size: 0.85rem;"),
                checkboxInput(paste0("off_active_", col_clean), tr("include_analysis", lang), TRUE),
                fluidRow(
                    column(6,
                           div(
                               class = "form-group shiny-input-container",
                               tags$label(tr("color", lang), `for` = paste0("off_color_", col_clean), style = "font-size: 0.75rem;"),
                               tags$input(id = paste0("off_color_", col_clean), type = "color", 
                                          value = default_colors[((i-1) %% length(default_colors)) + 1], 
                                          class = "form-control form-control-sm w-100", style = "height: 30px; padding: 0;")
                           )
                    ),
                    column(6,
                           numericInput(
                               inputId = paste0("off_offset_", col_clean),
                               label = tr("offset_v", lang),
                               value = 0.0,
                               step = 0.01
                           )
                    )
                ),
                actionButton(
                    inputId = paste0("off_btn_calib_", col_clean),
                    label = tr("zero_off", lang),
                    class = "btn btn-outline-info btn-xs w-100 mt-1",
                    style = "font-size: 0.7rem; padding: 2px 5px;"
                )
            )
        })
        do.call(tagList, inputs)
    })
    
    # Observe dynamic offline calibration buttons
    observe({
        dat <- loaded_raw_data()
        if (is.null(dat)) return()
        cols <- setdiff(names(dat), c("time_s", "clock_time"))
        if (length(cols) == 0) return()
        
        for (col in cols) {
            col_clean <- gsub("[^a-zA-Z0-9_-]", "_", col)
            btn_id <- paste0("off_btn_calib_", col_clean)
            
            local({
                c_clean <- col_clean
                b_id <- btn_id
                c_name <- col
                
                observeEvent(input[[b_id]], {
                    lang <- if (!is.null(input$lang)) input$lang else "it"
                    raw_dat <- loaded_raw_data()
                    if (!is.null(raw_dat) && c_name %in% names(raw_dat)) {
                        # Slice time to get mean of current viewport
                        if (!is.null(input$offline_time_range)) {
                            raw_dat <- raw_dat %>% 
                                filter(time_s >= input$offline_time_range[1] & time_s <= input$offline_time_range[2])
                        }
                        mean_val <- mean(raw_dat[[c_name]], na.rm = TRUE)
                        updateNumericInput(session, paste0("off_offset_", c_clean), value = round(mean_val, 4))
                        showNotification(paste(tr("calib_off_done", lang), c_name, ":", round(mean_val, 4), "V"), type = "message")
                    }
                }, ignoreInit = TRUE)
            })
        }
    })
    
    # Dynamic time-range selection UI based on loaded file duration
    output$offline_time_range_ui <- renderUI({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        dat <- loaded_raw_data()
        if (is.null(dat)) return(NULL)
        
        t_min <- min(dat$time_s, na.rm = TRUE)
        t_max <- max(dat$time_s, na.rm = TRUE)
        
        sliderInput("offline_time_range", tr("time_range_off", lang),
                    min = t_min, max = t_max, value = c(t_min, t_max), step = 0.1)
    })
    
    # Filter loaded data + time segment slice
    processed_offline_data <- reactive({
        dat <- loaded_raw_data()
        if (is.null(dat)) return(NULL)
        
        # Get active channels in offline mode
        all_cols <- setdiff(names(dat), c("time_s", "clock_time"))
        active_cols <- c()
        for (col in all_cols) {
            col_clean <- gsub("[^a-zA-Z0-9_-]", "_", col)
            is_active <- input[[paste0("off_active_", col_clean)]]
            if (is.null(is_active) || is_active) {
                active_cols <- c(active_cols, col)
            }
        }
        if (length(active_cols) == 0) return(NULL)
        
        # Keep only time_s, clock_time (if present), and selected channels
        cols_to_keep <- intersect(c("time_s", "clock_time"), names(dat))
        dat <- dat[, c(cols_to_keep, active_cols), drop = FALSE]
        
        fs <- as.numeric(input$offline_fs)
        
        # 1. Apply Offset Calibration FIRST
        for (col in active_cols) {
            col_clean <- gsub("[^a-zA-Z0-9_-]", "_", col)
            offset_val <- input[[paste0("off_offset_", col_clean)]]
            if (is.null(offset_val)) offset_val <- 0
            dat[[col]] <- dat[[col]] - offset_val
        }
        
        # 2. Apply filters on the FULL dataset first (prevents startup transient artifact after slicing)
        if (input$off_enable_filters) {
            for (col in active_cols) {
                y <- dat[[col]]
                if (input$off_filter_hp) y <- filter_highpass(y, fs = fs, fc = 5)
                if (input$off_filter_notch) y <- filter_notch(y, fs = fs, f0 = 50)
                if (input$off_filter_lp) y <- filter_lowpass(y, fs = fs, fc = 450)
                
                if (input$off_conditioning == "rectified") {
                    y <- abs(y)
                } else if (input$off_conditioning == "rms") {
                    y <- moving_rms(y, window_ms = input$off_rms_window_ms, fs = fs)
                }
                dat[[col]] <- y
            }
        } else {
            if (input$off_conditioning == "rectified") {
                for (col in active_cols) dat[[col]] <- abs(dat[[col]])
            } else if (input$off_conditioning == "rms") {
                for (col in active_cols) dat[[col]] <- moving_rms(dat[[col]], window_ms = input$off_rms_window_ms, fs = fs)
            }
        }
        
        # 3. Slice the time segment AFTER filtering has completed
        if (!is.null(input$offline_time_range)) {
            dat <- dat %>% 
                filter(time_s >= input$offline_time_range[1] & time_s <= input$offline_time_range[2])
        }
        
        if (nrow(dat) == 0) return(NULL)
        
        dat
    })
    
    # Compute offline metrics table for the selected portion
    output$offline_metrics_table <- renderTable({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        dat <- processed_offline_data()
        if (is.null(dat) || nrow(dat) < 5) return(NULL)
        
        # Exclude time columns to find actual channels
        cols <- setdiff(names(dat), c("time_s", "clock_time"))
        fs <- as.numeric(input$offline_fs)
        
        metrics <- lapply(cols, function(col) {
            y <- dat[[col]]
            
            mean_val <- mean(y, na.rm = TRUE)
            rms_val <- sqrt(mean(y^2, na.rm = TRUE))
            peak_val <- max(abs(y), na.rm = TRUE)
            iemg_val <- sum(abs(y), na.rm = TRUE) / fs
            
            data.frame(
                Canale = col,
                `Valore Medio (V)` = mean_val,
                `Valore RMS (V)` = rms_val,
                `Ampiezza di Picco (V)` = peak_val,
                `iEMG (V·s)` = iemg_val,
                check.names = FALSE
            )
        })
        
        res <- do.call(rbind, metrics)
        names(res) <- c(
            tr("canale", lang),
            tr("val_media", lang),
            tr("val_rms", lang),
            tr("amp_picco", lang),
            tr("iemg", lang)
        )
        res
    }, digits = 4, striped = TRUE, hover = TRUE, bordered = TRUE)
    
    # Offline plot showing selected slice with custom line colors
    output$offline_plot <- renderPlotly({
        lang <- if (!is.null(input$lang)) input$lang else "it"
        dat <- processed_offline_data()
        validate(
            need(!is.null(dat), tr("upload_first", lang))
        )
        
        default_colors <- c("#ff0000", "#0070ff", "#f39c12", "#00bc8c", "#9b59b6", "#1abc9c", "#d35400", "#34495e")
        cols <- setdiff(names(dat), c("time_s", "clock_time"))
        plots <- lapply(seq_along(cols), function(i) {
            col <- cols[i]
            col_clean <- gsub("[^a-zA-Z0-9_-]", "_", col)
            color_val <- default_colors[((i-1) %% length(default_colors)) + 1]
            color_picker_val <- input[[paste0("off_color_", col_clean)]]
            if (!is.null(color_picker_val)) color_val <- color_picker_val
            
            yaxis_opts <- list(title = paste(col, "(V)"))
            if (isTRUE(input$off_fix_yaxis)) {
                ymin <- if (is.numeric(input$off_yaxis_min)) input$off_yaxis_min else -1
                ymax <- if (is.numeric(input$off_yaxis_max)) input$off_yaxis_max else 1
                yaxis_opts$range <- c(ymin, ymax)
            }
            
            plot_ly(dat, x = ~time_s, y = as.formula(paste0("~`", col, "`")), 
                    type = 'scatter', mode = 'lines',
                    line = list(width = 1.2, color = color_val), name = col) %>%
                layout(
                    yaxis = yaxis_opts,
                    xaxis = list(title = tr("tempo_s", lang))
                )
        })
        subplot(plots, nrows = length(plots), shareX = TRUE, titleY = TRUE) %>%
            layout(
                xaxis = list(title = tr("tempo_s", lang), range = c(input$offline_time_range[1], input$offline_time_range[2])),
                hovermode = "x unified",
                legend = list(orientation = 'h', y = 1.15),
                margin = list(t = 35, b = 30, l = 50, r = 20)
            )
    })
    
    # Kill python daemon on session end/app closing
    session$onSessionEnded(function() {
        writeLines("stop", stop_file)
        if (file.exists(record_control_file)) try(file.remove(record_control_file), silent = TRUE)
    })
}

shinyApp(ui, server)