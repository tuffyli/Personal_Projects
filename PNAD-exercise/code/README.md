# Code

This folder contains the scripts used to construct the analysis dataset and produce the sample project.

- `data.R` prepares the filtered PNAD sample, defines treatment and control groups, creates analysis variables, and writes the derived `.rds` files to `data/`.
- `Sample_Project.Rmd` reads the prepared data, runs propensity score matching, estimates the regressions, and produces the PDF/HTML/Word outputs.

The scripts resolve paths relative to the `PNAD-exercise` folder, so they can be run from either the project root or the `code/` subfolder without editing machine-specific file paths.
