# DataGuard — Automated Data Quality and Drift Detection

DataGuard is an R-based data validation project that detects data-quality issues and changes between reference and new datasets.

## Features

- Automated data-quality scorecard
- Missing-value detection
- Duplicate-row detection
- Schema validation
- Numeric range validation
- Statistical data-drift detection
- Interactive Shiny dashboard
- Downloadable quality and drift reports

## Technologies Used

- R
- RStudio
- Shiny
- ggplot2
- CSV datasets
- Statistical hypothesis testing

## Project Structure

```text
DataGuard/
├── app.R
├── dataguard.R
├── requirements.R
├── README.md
├── .gitignore
├── data/
│   ├── reference.csv
│   └── new_data.csv
├── reports/
└── plots/
```

## Installation

1. Install R and RStudio.
2. Download or clone this repository.
3. Open the DataGuard project directory in RStudio.
4. Run the following command:

```r
source("requirements.R")
```

## Run the Web Dashboard

Open `app.R` in RStudio and click **Run App**.

Upload a reference CSV file and a new CSV file, then click **Validate Data**.

## Run the Core Validation Script

Open `dataguard.R` and click **Source**.

The script creates sample data when needed, performs validation and drift checks, and generates reports in the `reports/` and `plots/` folders.

## How It Works

DataGuard compares reference data with incoming data, applies configurable validation rules, identifies potential quality problems, and summarizes the results through reports and a dashboard.

## Important Note

DataGuard is an educational and portfolio project. Its quality score and drift thresholds are illustrative and should be calibrated to the dataset and use case.

## Future Improvements

- Configurable schema and business rules
- Advanced drift metrics such as Population Stability Index (PSI)
- Improved validation test coverage
- Historical monitoring and audit logs
- Public deployment of the Shiny dashboard

## Author

Developed as an R programming and data science portfolio project.
