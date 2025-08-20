# dolphin-classification

This repository contains code for classifying dolphin species using deep learning techniques and supports the related publication on improving acoustic classification of two closely related species, the Atlantic bottlenose and common dolphin.

The main files are:

- `code/dataprocess.R`: Prepares the datasets for model training and evaluation.
- `report_species.qmd`: Generates the report for the species classification task. 
Set `echo: true` in YAML header and recompile to see the underlying code along the outputs in the resulting file `report_species.pdf`. 
In the section "General parameters", set `rerun_CV <- TRUE` to rerun the cross-validation and generate new results (the cross-validation codes are in the folder `code/`); it is set to `FALSE` by default to use the precomputed results.
Similarly, set `rerun_pdp <- TRUE` to update partial dependence plots, which are set to `FALSE` by default.
