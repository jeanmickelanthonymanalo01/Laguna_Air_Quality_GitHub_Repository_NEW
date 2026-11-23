# Laguna Air Quality Analysis and Next-Day Forecasting

**Courses:** CSST101 (R Programming) and CSST102 (Basic Machine Learning)  
**Group:** ChunkyChucky  
**Institution:** Laguna State Polytechnic University  
**Project area:** Laguna, Philippines

## Project overview

This repository contains the R-based statistical analysis of available air-quality estimates for Laguna (CSST101) and a Python/scikit-learn notebook that evaluates next-day air-quality classification (CSST102). It also includes the R Shiny dashboard source, the data files needed by the analysis and notebook, exported results and charts, and weekly progress forms.

The air-quality source values are OpenWeatherMap-derived estimates obtained through the Kaggle *Philippine Cities Air Quality Index Data* dataset. They are **not reference-grade measurements from local monitoring stations**. Biñan, Cabuyao, and Calamba have nearly identical PM2.5 series in the analyzed data, so the work treats them as one regional series rather than independent city measurements.

## Repository structure

```text
.
├── CSST101/
│   ├── Laguna_AirQuality_Analysis.Rmd       # Main R Markdown analysis
│   ├── Laguna_AirQuality_Analysis.docx      # Current Word report (updated after fresh-session rerun)
│   ├── Laguna_AirQuality_Analysis_Nov22_2026.pdf # Current PDF report
│   ├── csst101_week*.R                      # Weekly R scripts
│   ├── laguna_dashboard_app.R               # R Shiny dashboard source
│   ├── *.csv                                # Analysis inputs, cleaned data, results
│   ├── chart*.png                           # Exported analysis charts
│   └── archive/                             # Earlier Oct 27 report copies
├── CSST102/
│   ├── CSST102_Air_Quality_Prediction.ipynb # Executed Colab/Jupyter notebook
│   ├── CSST102_Report_Manalo.docx            # Final ML report
│   ├── CSST102_Report_Manalo.pdf             # PDF export of the report
│   ├── *.csv                                # Notebook inputs and outputs
├── progress-evidence/
│   ├── Shared/                              # Initial project planning form
│   ├── CSST101/                             # CSST101 weekly progress forms and draft
│   └── CSST102/                             # CSST102 weekly progress forms
├── supporting-documents/
│   └── Instructor_QA_CSST101_CSST102.docx
├── requirements.txt
└── .gitignore
```

## CSST101: run the R analysis

1. Install R and RStudio.
2. Open the `CSST101` folder as the working folder (or set the R working directory to it).
3. Install the packages once:

   ```r
   install.packages(c("tidyverse", "lubridate", "jsonlite", "knitr", "rmarkdown"))
   ```
4. Open `Laguna_AirQuality_Analysis.Rmd` and use **Knit** to generate the report.
5. The R Markdown file reads `laguna_air_quality_2023-2026.csv`. It uses `weather_laguna.csv` when that file is present; otherwise, it attempts to retrieve weather from Open-Meteo (internet required).

### Run the R Shiny dashboard

The dashboard expects these files in the same working folder: `laguna_hourly_clean.csv`, `weather_laguna.csv`, and `forecast_next_day.csv` (the forecast file is optional for the forecast card). Install the dashboard packages:

```r
install.packages(c("shiny", "plotly", "leaflet", "tidyverse", "lubridate"))
```

Then open `laguna_dashboard_app.R` in RStudio and click **Run App**.

## CSST102: run the machine-learning notebook

1. Open `CSST102/CSST102_Air_Quality_Prediction.ipynb` in Google Colab or Jupyter.
2. Keep `laguna_hourly_clean.csv` and `weather_laguna.csv` in the notebook's working folder. In Google Colab, if they are not already present, the notebook asks you to upload both files.
3. Run the notebook from top to bottom (**Runtime → Run all** in Colab).
4. The notebook uses pandas, NumPy, Matplotlib, and scikit-learn. See `requirements.txt` for the Python package list.
5. The notebook writes `model_results.csv`, `predictions_test.csv`, and `forecast_next_day.csv`.

## Key reported CSST102 results

The submitted notebook/report evaluate the models on a chronological test period. The persistence baseline (tomorrow's class = today's class) achieved about **74.2% accuracy** and **0.666 macro-F1**. The Random Forest using pollution and calendar features achieved about **71.3% accuracy**, and the Random Forest with weather features achieved about **68.4%**. The tested machine-learning models did not outperform persistence, and they struggled to identify the minority “Moderate or worse” class. This limitation is reported rather than hidden.

## Data and reproducibility notes

- The prepared input files used by the submitted R Markdown and Python notebook are included, so those analyses can be rerun without reconstructing the full source download.
- The uploaded archive also contained only two monthly all-city source extracts (January 2025 and January 2026), not a complete raw monthly archive for the full study period. They are not included here because they are not sufficient to reconstruct the full prepared Laguna dataset and are not required by the included analysis files.
- The source dataset is identified in the reports as CC0 public-domain data from Kaggle; Open-Meteo weather data and the software libraries are cited in the reports.
- The project code's separate license was not specified in the supplied files; no new code license has been assigned in this repository.

## Progress evidence and submission status

- Weekly progress forms for CSST101 Weeks 1–7 and CSST102 Weeks 1–7 are included under `progress-evidence/`.
- CSST101 fresh-session R Markdown rerun was completed on November 23, 2026. Supporting screenshots are stored in `progress-evidence/Fresh-Session-Rerun/evidence/`.
- CSST101 Week 7 records the fresh-session rerun as completed; final package upload remains pending until it is submitted to the required destination.
- CSST102 Week 7 keeps the demonstration and explanation tasks marked In Progress because the supplied form documents a plan and scheduled practice, not completed presentation evidence.

## Suggested citation

See the references in the CSST101 R Markdown report and CSST102 final report for the Kaggle air-quality dataset, OpenWeatherMap, Open-Meteo, scikit-learn, and related project context.
