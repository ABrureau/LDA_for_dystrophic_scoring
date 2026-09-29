# CAPN3 rat study: LDA analysis of histological and functional variables

Author: Anthony Brureau

## Overview

This R script compares 17 predefined variable panels using linear discriminant analysis (LDA). It trains classifiers on placebo-treated WT and Ko samples, computes leave-one-out cross-validation (LOOCV) classification metrics, and applies the fitted models to all samples retained by the sex, muscle and age filters.

Outputs include LDA scores, comparisons against the Ko group, ROC curves, model rankings and figures. This README describes the supplied script as implemented; it does not certify statistical validation. The input workbook was not supplied with the script, and execution against the study data has not been verified for this documentation.

## 1. Files to place in the analysis folder

| File | Purpose |
| --- | --- |
| `analysis_LDA.R` | Save the supplied R code under this name, or use your existing script name. |
| Your Excel workbook (`.xlsx`) | Study measurements and sample metadata. |
| `README.md` | These instructions. |

Place the script and workbook in the same folder. The current script also writes its outputs directly into this folder. No RStudio project (`.Rproj`) is required.

The workbook filename, public data link, article reference, code DOI and license have not yet been provided. Add them before depositing the final publication version. The analysis cannot be reproduced from the script alone without the input workbook or access to equivalent source data.

## 2. Requirements

- R; RStudio is convenient but optional.
- An internet connection and permission to install R packages for the first run if dependencies are missing.
- The `xlsx` package requires a working Java/rJava setup compatible with the R installation.

The script loads the following packages and automatically installs missing packages and their dependencies:

```r
tidyverse, readxl, xlsx, ggplot2, DescTools, MASS, caret,
pROC, ggrepel, patchwork, scales, purrr, pheatmap
```

Exact tested R, package, Java and operating-system versions were not supplied. Automatic installation does not pin package versions. Record the environment used for the published results by running the following after the analysis:

```r
writeLines(capture.output(sessionInfo()),
           file.path(wd, "sessionInfo.txt"))
```

Include `sessionInfo.txt` in the final deposit. Runtime and memory requirements have not been measured.

## 3. Input workbook structure

Only the **first worksheet** is read. Column names and text values are case-sensitive. Each row should represent one analyzed sample; the script does not check animal identifiers or account for repeated measurements from the same animal.

### Required column positions

The script selects columns partly by position. Preserve the following layout:

| Position | Expected content |
| --- | --- |
| 1 | `group`: WT, Ko or a treatment-group label. |
| 2 | `Protocol`: study identifier, such as `20-184` or `20-066`. |
| 3–6 | Other metadata; this block can hold the filtering columns below. These columns are discarded after filtering. |
| 7 | `treatment`: includes the exact value `placebo` for training samples. |
| 8 onward | Numeric measurement columns only; every column in this range is passed to `scale()`. |

The workbook must contain the named columns `Sex`, `muscle` and `age` for filtering. These should be placed in the metadata block, not among the numeric measurements. Do not add text identifiers in columns 8 onward.

Two measurement columns are renamed automatically:

| Original Excel heading | Name used in the analysis |
| --- | --- |
| `HPS % conjonctif` | `Conj` |
| `HPS % inflammation` | `Inf` |

The union of measurement variables used by the panels is:

```text
Mhcd
centro
IgG-M1
Conj
Inf
sPt
sPOCSA
HRT twitch
TTP tetanos
TTP twitch
```

A panel is skipped if one of its required variables is absent. Measurement definitions and units should be supplied with the workbook; they cannot be established from column names alone.

## 4. Configure and run

1. Open the script in RStudio.
2. Edit the `PARAMETERS - CHANGE HERE` section.
3. Set `wd` to the actual folder on your computer and replace the placeholder workbook filename.
4. Check the filters against the exact values in your workbook.
5. Run the full script from the beginning, preferably in a fresh R session (the **Source** button in RStudio).

Example configuration matching the supplied script:

```r
wd <- "E:/CAPN3_LDA_publication"
file <- file.path(wd, "YOUR_ACTUAL_WORKBOOK.xlsx")
setwd(wd)

md.sex <- "Female"
md.muscle <- "pso"
md.age <- "6 months"

positive_class <- "WT"
negative_class <- "Ko"
selected_model <- "histo_minus_Igg"
primary_metric <- "AUROC"
```

Use forward slashes in Windows paths. The folder must already exist.

`Female`, `pso` and `6 months` are exact text filters, not automatic categories. For example, `male` and `Male` are different strings. Analyze other subsets by changing these settings and rerunning the script.

`selected_model` highlights a panel and determines which panel receives the LDA score bar plot; it does not restrict the analysis to that panel or automatically choose the top-ranked panel.

`primary_metric` controls ranking and the performance-versus-panel-size plot. Accepted values are `Accuracy`, `Sensitivity`, `Specificity`, `BalancedAccuracy` and `AUROC`.

## 5. Variable panels

For compactness, H denotes the four variables `Mhcd`, `centro`, `Conj` and `Inf`.

| Panel name | Variables |
| --- | --- |
| `Histo.var` | H + `IgG-M1` |
| `Histo.Spt.var` | H + `IgG-M1` + `sPt` |
| `Histo.Sp0CSA.var` | H + `IgG-M1` + `sPOCSA` |
| `Histo+FctL` | H + `IgG-M1` + `sPOCSA` + `sPt` |
| `histo_minus_Igg` | H |
| `histo_HRT_SP0CSA` | H + `sPOCSA` + `HRT twitch` |
| `histo_HRT_Spt` | H + `sPt` + `HRT twitch` |
| `histo_TTPTT_Spt` | H + `sPt` + `TTP tetanos` |
| `histo_TTPTT_sPOCSA` | H + `sPOCSA` + `TTP tetanos` |
| `histo_TTP_Spt` | H + `sPt` + `TTP twitch` |
| `histo_TTP_sPOCSA` | H + `sPOCSA` + `TTP twitch` |
| `histo_all_fctL` | H + `sPOCSA` + `sPt` + `HRT twitch` + `TTP tetanos` + `TTP twitch` |
| `histo_TTP` | H + `TTP twitch` |
| `histo_HRT` | H + `HRT twitch` |
| `histo_TTPTT` | H + `TTP tetanos` |
| `Histo.Sp0CSA.noIgg` | H + `sPOCSA` |
| `Histo.Spt.noIgg` | H + `sPt` |

Keep the exact panel names: some use the digit `0` in their name, whereas the measurement column is spelled `sPOCSA` with the letter `O`.

## 6. Analysis implemented

1. Read the first worksheet and retain the selected sex, muscle and age.
2. Standardize all numeric measurement columns using `scale()` on the entire filtered dataset.
3. For each panel, replace missing numeric values with zero after scaling.
4. Select training samples with `Protocol` equal to `20-184` or `20-066` and `treatment` equal to `placebo`. Their groups must match the two specified classes.
5. Remove near-zero-variance predictors using `caret::nearZeroVar()` on the training set.
6. Fit an LDA model with `MASS::lda()`. No explicit priors are supplied; the default class-frequency priors are used.
7. Obtain LOOCV classifications and posterior probabilities with `lda(..., CV = TRUE)`.
8. Calculate classification metrics and ROC/AUROC from the WT posterior probabilities. With the default configuration, sensitivity refers to WT and specificity refers to Ko.
9. Apply the full fitted model to every retained sample to obtain the first discriminant score (`lda`). These scores are not the held-out LOOCV predictions.
10. Compare LDA scores between groups and the Ko reference using `DescTools::DunnettTest()`, restricted to protocol `20-184`.
11. Rank panels by the selected metric, then by ascending panel size and descending accuracy.

### Rescaled score

The exported `lda.prop.ctl` is computed exactly as follows:

```text
100 × (lda + abs(mean_Ko)) / (mean_WT + abs(mean_Ko))
```

The reference means use the WT and Ko groups from protocol `20-184` within the filtered data. It is not a posterior probability. Mapping the Ko mean to zero assumes a non-positive Ko mean; the expression is not a general sign-independent normalization. Values are not constrained to 0–100. Check score orientation, reference groups and the denominator before interpreting this field.

## 7. Outputs

Most filenames include the date (`YYYYMMDD`), sex, muscle and, for per-panel results, panel name.

| Filename prefix or name | Content |
| --- | --- |
| `Results_LDA_*.xlsx` | Retained metadata, standardized panel measurements, LDA scores and rescaled scores. |
| `Results_Dunnett_*.xlsx` | Comparisons against Ko for protocol 20-184, if successful. |
| `Results_Crossval_*.xlsx` | Per-panel classification metrics, AUROC, confidence limits, ROC p-value field and first LDA singular value. |
| `Results_ROC_coordinates_*.xlsx` | Specificity, sensitivity, false-positive rate and thresholds. |
| `ROC_*.png` | Individual ROC curves. |
| `Ranking_panels_LDA_ROC_*.csv` and `.xlsx` | Combined panel ranking. |
| `Heatmap_LDA_metrics_*.png` | Clustered performance heatmap. |
| `Panel_A_selection_performance_*.png` | Panel performance comparison. |
| `Panel_B_parsimony_vs_performance_*.png` | Performance versus panel size. |
| `Figure_panels_A_B_LDA_ROC_*.png` and `.pdf` | Combined panel figure. |
| `All_ROC_curves_LDA_*.png` | Combined ROC curves. |
| `LDA_score_barplot_*.png` | Mean LDA scores ± standard error for the selected panel. |
| `Accuracy_Sensitivity_Specificity_Histo_noIgG_from_existing_ranking.png` and `.pdf` | Additional comparison of three histology panels without IgG. |

The final additional plotting block selects the most recently modified `Ranking_panels_LDA_ROC_*.xlsx` or `.csv` file in `wd`, not necessarily a file matching the current filters. It compares `histo_minus_Igg`, `Histo.Spt.noIgg` and `Histo.Sp0CSA.noIgg`.

**Use a separate folder for each analysis subset or archive results before rerunning.** Age is absent from output filenames. Runs with the same date, sex and muscle can overwrite files even when age differs. The two additional-plot filenames are fixed and are overwritten on rerun.

The bar plot uses hard-coded group labels: WT, Ko, and four `C3-KO + GNT0008 [...]` groups at 1.1E13, 2.7E13, 5.4E13 and 1.6E14 vg/kg. Update the `compo` vector if your workbook uses different labels; unmatched labels become missing in this plot.

## 8. Interpretation and current limitations

- Scaling and missing-value replacement occur before cross-validation and use the filtered dataset, including samples outside the training subset. Near-zero-variance filtering also occurs before LOOCV. This is not a fully fold-contained preprocessing workflow. Zero replacement on the standardized scale generally represents mean imputation for nonconstant variables; constant variables require separate attention.
- Panel ranking and assessment use the same dataset. The best panel's performance is not an independent validation of panel selection.
- Reported panel sizes count the initially specified variables, including any later removed by near-zero-variance filtering.
- AUROC confidence intervals are calculated by the DeLong procedure on LOOCV predictions. The script does not explicitly account for dependence induced by overlapping training folds.
- The code attempts `roc.test(roc_obj, auc = 0.5, ...)` and falls back to a one-sided Wilcoxon rank-sum test on posterior scores if that call errors. The exported `ROC_pvalue` must therefore not be described unconditionally as a DeLong test against AUROC = 0.5. Review this implementation and its inference before publication; no across-panel p-value adjustment is implemented.
- Dunnett comparisons use scores from the full fitted model, including training samples, and do not account for model fitting or panel selection uncertainty.
- Performance figures restrict axes to approximately 0.5–1; lower values may be omitted from the figures even though they remain in the tables.
- Animal identifiers are not preserved by the metadata selection unless the script is modified. Keep the source workbook and row correspondence for traceability.
- Fitted models, model coefficients, per-sample LOOCV predictions and confusion matrices are not exported by the current script.

## 9. Troubleshooting

| Issue | What to check |
| --- | --- |
| Workbook not found | Confirm `wd`, the exact filename and the `.xlsx` extension. |
| Error in `scale()` | Columns 8 onward must be numeric; remove misplaced metadata or correct Excel cell types. The helper `to_num()` is defined but is not applied before scaling. |
| No model fitted | Check filter spelling, placebo samples in both classes, protocol labels, column order and missing predictors. |
| Missing-variable warning | Match measurement headings exactly, including spaces, punctuation and `sPOCSA`. |
| `xlsx`/`rJava` fails to load | Check the Java installation and compatibility with R. |
| LDA fitting fails | Inspect class sample sizes, within-class variation, constant predictors and collinearity. |
| Heatmap clustering fails | Inspect missing/non-finite metrics and whether enough valid models remain. |
| Unexpected final three-panel figure | Check which ranking file was most recently modified in `wd`. |

## 10. Publication information to complete

Before depositing the final version, supply:

- The exact script and input workbook filenames, and a public data link if data are hosted separately.
- The manuscript title, authors and reference/DOI when available.
- The code version and archive DOI when assigned.
- The tested software environment (`sessionInfo.txt`).
- A data dictionary giving measurement definitions and units.
- A code license and contact information.

No license, DOI, validated runtime or tested software version is implied by this README.
