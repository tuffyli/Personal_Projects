# Data

This folder contains the filtered data files used by the code sample.

- `Pnadpnad_filtered_9295.rds`: intermediate filtered PNAD file with selected variables.
- `final_filtered_9295.rds`: final analysis dataset used by `code/Sample_Project.Rmd`.
- `summary.rds`: weighted descriptive statistics displayed in the report.
- `pnad_raw_9295.rds`: optional raw PNAD input. This file is not included in the repository.

To rebuild from raw PNAD microdata, either place `pnad_raw_9295.rds` in this folder or load the raw data into an R data frame named `data` before running `source("code/data.R")`.
