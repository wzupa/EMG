# Real-Time EMG Acquisition & Analysis System (v0.1.0)

Questo repository contiene un sistema integrato ad alte prestazioni per l'acquisizione in tempo reale, il filtraggio digitale e l'analisi offline di segnali elettromiografici (EMG) biologici. Il software è specificamente ottimizzato per studi di fisiologia animale e protocolli di nuoto critico (**Ucrit**) su pesci.

Sviluppato per **Fondazione COISPA ETS**.

---

## 🚀 Architettura del Sistema

Il sistema adotta un'architettura ibrida per garantire massima reattività dell'interfaccia utente ed efficienza di campionamento dell'hardware:
* **Frontend (R Shiny)**: Interfaccia utente interattiva basata su `bslib` (in modalità scura per laboratori a bassa luminosità), che gestisce la visualizzazione dei grafici multi-canale sincronizzati, l'elaborazione dei filtri digitali e l'esportazione dei metadati.
* **Backend (Python)**: Un demone di acquisizione in background (`daq_emg_stream.py`) che interagisce a basso livello con la scheda **National Instruments NI USB-6009** sfruttando l'API ufficiale NI-DAQmx per garantire lo streaming continuo senza colli di bottiglia o blocchi di memoria.

---

## ✨ Caratteristiche Principali

* **Monitoraggio Multicanale in Tempo Reale**: Visualizzazione simultanea e allineata verticalmente di fino a 8 canali analogici (da `ai0` a `ai7`) tramite grafici interattivi `plotly`.
* **Disegno Ottimizzato**: Algoritmo integrato di downsampling (decimazione) dinamica per consentire la visualizzazione fluida di ampie finestre temporali (es. 60 secondi) senza rallentamenti, limitando i punti tracciati a schermo.
* **Elaborazione Digitale del Segnale (DSP)**:
  * **Rimozoine Offset (High-Pass 5 Hz)**: Rimuove l'offset elettrico DC e gli artefatti da movimento. Include il pre-padding dei dati per eliminare i transitori transitori di attivazione a $t=0$.
  * **Filtro Notch (50 Hz)**: Elimina selettivamente le interferenze elettriche di rete.
  * **Passa-Basso (Low-Pass 450 Hz)**: Riduce il rumore bianco elettronico ad alta frequenza.
  * **Condizionamento**: Supporto per segnale rettificato (onda intera) e calcolo dell'inviluppo dinamico tramite **RMS mobile** (finestra regolabile).
* **Compensazione dell'Offset DC**: Pulsante "Azzera da segnale live" per sottrarre dinamicamente l'offset residuo a pesce fermo.
* **Salvataggio Antiperdita e Sequenziale**:
  * Scrittura indipendente e atomica dei dati per evitare lock di scrittura.
  * Generazione normalizzata del nome del file a partire dai metadati Ucrit (Specie, ID, Taglia, Step).
  * Controllo dei duplicati: se il file esiste già su disco, viene accodato automaticamente un suffisso numerico progressivo (`_01`, `_02`, ecc.).
* **Bilinguismo Nativo**: Traduzione istantanea (Italiano/Inglese) dell'intera interfaccia senza ricaricare i pannelli e senza perdita di stato dei grafici attivi.
* **Analisi Offline Integrata**: Caricamento di registrazioni storiche in formato `.csv` o `.rds` con calcolo delle metriche chiave (Media, RMS, Picco, iEMG) sulla finestra temporale selezionata.

---

## 🛠️ Requisiti di Sistema e Installazione

### 1. Driver Hardware
Per interfacciarsi con la scheda di acquisizione NI USB-6009, è necessario installare i driver ufficiali di National Instruments:
* Scaricare e installare [NI-DAQmx](https://www.ni.com/it-it/support/downloads/drivers/download.ni-daqmx.html).

### 2. Ambiente Python
Il demone di streaming richiede Python 3 (configurato nel PATH di sistema di Windows) e le seguenti librerie:
```bash
pip install nidaqmx numpy
```

### 3. Ambiente R / RStudio
L'interfaccia utente richiede R (consigliata versione $\ge$ 4.0) e i pacchetti elencati di seguito. Per installarli, eseguire in RStudio:
```R
install.packages(c("shiny", "bslib", "plotly", "tidyverse"))
```

---

## 🏃 Come Avviare l'Applicazione

1. Collegare la scheda NI USB-6009 al computer tramite porta USB (assicurarsi che venga rilevata dal sistema come `Dev1` tramite l'utility NI MAX).
2. Aprire RStudio.
3. Impostare la directory di lavoro sulla cartella contenente l'applicazione:
   ```R
   setwd("percorso/della/cartella/EMG")
   ```
4. Eseguire l'applicazione Shiny:
   ```R
   shiny::runApp()
   ```

---

## 📝 Struttura del File di Registrazione

I file generati al termine della registrazione (salvati sia in formato `.csv` che in formato compresso binario `.rds` ad alta velocità) presentano la seguente struttura tabellare:
* `time_s`: Tempo relativo all'avvio della registrazione (in secondi, partendo da `0.0`).
* `clock_time`: Ora civile precisa in formato `HH:MM:SS.FFF` ricavata dal server di clock di sistema.
* Colonne Canali (es. `ai0`, `ai1`): Valori di tensione acquisiti (espressi in Volt), già compensati dall'offset impostato in calibrazione.

---

## 👥 Crediti e Contatti

* **Autore Scientifico**: Walter Zupa ([zupa@fondazionecoispa.org](mailto:zupa@fondazionecoispa.org))
* **Sviluppo & Affiliazione**: [Fondazione COISPA ETS](https://www.coispa.it)
