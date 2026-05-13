# PNAD Exercise: Alimony Rights and Young Women's Education in Brazil

This repository contains a compact empirical code sample inspired by Rangel (2006), which studies Brazil's expansion of alimony rights for women in stable unions. The exercise focuses on young women aged 15-24 and asks whether eligibility for alimony rights is associated with changes in years of education.

The project is intended as a reproducible sample of my empirical workflow: preparing survey microdata, defining treatment and control groups, checking covariate balance with propensity score matching, and estimating weighted OLS specifications.

## Repository Structure

- `code/data.R`: prepares the filtered PNAD sample and summary statistics.
- `code/Sample_Project.Rmd`: runs matching, creates balance plots, estimates regressions, and generates the report.
- `data/`: contains the filtered analysis files used by the R Markdown report.
- `match-tables/`: stores the matching summary tables by year.
- `Sample_Project.pdf`: rendered version of the report.

## Data and Design

- **Data:** PNAD microdata for 1992, 1993, and 1995. The full PNAD files are not redistributed, but the filtered analysis files are included.
- **Treatment group:** women aged 15-24 in non-civil unions, either consensual or religious-only, who are household heads or spouses.
- **Control group:** similar women in legally recognized unions, either civil or civil plus religious.
- **Method:** propensity score matching followed by weighted OLS specifications with progressively richer controls and fixed effects.

## Data Paths

The code uses the following local paths:

- `data/Pnadpnad_filtered_9295.rds`: intermediate filtered PNAD file.
- `data/final_filtered_9295.rds`: final analysis file used by `Sample_Project.Rmd`.
- `data/summary.rds`: weighted summary statistics used in the report.
- `data/pnad_raw_9295.rds`: optional raw PNAD file, not included in the repository.

If the raw PNAD file is unavailable, `code/data.R` can still rebuild the final analysis file from `data/Pnadpnad_filtered_9295.rds`.

## Reproducing the Report

From the `PNAD-exercise` folder or its `code/` subfolder, render:

```r
rmarkdown::render("code/Sample_Project.Rmd")
```

To recreate the filtered analysis data from the raw PNAD file, load the raw PNAD data into an object named `data` and run:

```r
source("code/data.R")
```

Alternatively, place a local raw file named `pnad_raw_9295.rds` in the `data/` folder before sourcing `code/data.R`. The script falls back to the included filtered file when the raw file is not present.

## Citation

Rangel, M. A. (2006). Alimony Rights and Intrahousehold Allocation of Resources: Evidence from Brazil. *The Economic Journal*, 116(513), 627-658.
