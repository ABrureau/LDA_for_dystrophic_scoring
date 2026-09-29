#################################################################################################################################################################
# Description:
# This script tests different sets of variables to create Linear Discriminant Analysis models.
# Input: Excel file with raw data from studies 20-184 and 20-066.
# Output:
# - LDA scores for all animals
# - Dunnett's multiple comparison test
# - LOOCV classification metrics: sensitivity, specificity, accuracy, balanced accuracy
# - ROC curves based on LOOCV posterior probabilities
# - AUROC, AUROC 95% CI and p-value testing H0: AUROC = 0.5
# - Ranking table and panel selection figures
#
# Author: Anthony Brureau
# Updated version: LDA + LOOCV + ROC/AUROC/p-value
#################################################################################################################################################################

############################################################
# PACKAGES
############################################################

required_packages <- c(
  "tidyverse",
  "readxl",
  "xlsx",
  "ggplot2",
  "DescTools",
  "MASS",
  "caret",
  "pROC",
  "ggrepel",
  "patchwork",
  "scales",
  "purrr",
  "pheatmap"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}

invisible(lapply(required_packages, install_if_missing))

############################################################
# PARAMETERS - CHANGE HERE
############################################################


wd <- getwd()


file <- file.path(wd, "data", "mesures.xlsx")

output_dir <- file.path(wd, "results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

md.sex <- "Female" # "male"
md.muscle <- "pso" # "sol"
md.age <- "6 months"

positive_class <- "WT"
negative_class <- "Ko"

selected_model <- "histo_minus_Igg"

# Choix possibles:
# "Accuracy", "Sensitivity", "Specificity", "BalancedAccuracy", "AUROC"
primary_metric <- "AUROC"

date_tag <- format(Sys.time(), "%Y%m%d")

setwd(wd)

############################################################
# LOAD DATA
############################################################

the_sheet <- excel_sheets(file)[1]

md <- read_excel(
  path = file,
  sheet = the_sheet
)

colnames(md)[which(colnames(md) == "HPS % conjonctif")] <- "Conj"
colnames(md)[which(colnames(md) == "HPS % inflammation")] <- "Inf"

md <- md %>%
  filter(
    Sex == md.sex,
    muscle == md.muscle,
    age == md.age
  )

############################################################
# DEFINE VARIABLE PANELS
############################################################

Histo.var <- c("Mhcd", "centro", "IgG-M1", "Conj", "Inf")

Histo.Spt.var <- c("sPt", "Mhcd", "centro", "IgG-M1", "Conj", "Inf")

Histo.Sp0CSA.var <- c("sPOCSA", "Mhcd", "centro", "IgG-M1", "Conj", "Inf")

Histo.FctL <- c("sPOCSA", "sPt", "Mhcd", "centro", "IgG-M1", "Conj", "Inf")

histo_minus_Igg <- c("Mhcd", "centro", "Conj", "Inf")

histo_HRT_SP0CSA <- c("Mhcd", "centro", "Conj", "Inf", "sPOCSA", "HRT twitch")

histo_HRT_Spt <- c("Mhcd", "centro", "Conj", "Inf", "sPt", "HRT twitch")

histo_TTPTT_Spt <- c("Mhcd", "centro", "Conj", "Inf", "sPt", "TTP tetanos")

histo_TTPTT_sPOCSA <- c("Mhcd", "centro", "Conj", "Inf", "sPOCSA", "TTP tetanos")

histo_TTP_sPOCSA <- c("Mhcd", "centro", "Conj", "Inf", "sPOCSA", "TTP twitch")

histo_TTP_Spt <- c("Mhcd", "centro", "Conj", "Inf", "sPt", "TTP twitch")

histo_all_fctL <- c(
  "Mhcd",
  "centro",
  "Conj",
  "Inf",
  "sPOCSA",
  "sPt",
  "HRT twitch",
  "TTP tetanos",
  "TTP twitch"
)

histo_TTP <- c("Mhcd", "centro", "Conj", "Inf", "TTP twitch")

histo_HRT <- c("Mhcd", "centro", "Conj", "Inf", "HRT twitch")

histo_TTPTT <- c("Mhcd", "centro", "Conj", "Inf", "TTP tetanos")

Histo.Sp0CSA.noIgg <- c("sPOCSA", "Mhcd", "centro", "Conj", "Inf")

Histo.Spt.noIgg <- c("sPt", "Mhcd", "centro", "Conj", "Inf")

List.variables <- list(
  Histo.var,
  Histo.Spt.var,
  Histo.Sp0CSA.var,
  Histo.FctL,
  histo_minus_Igg,
  histo_HRT_SP0CSA,
  histo_HRT_Spt,
  histo_TTPTT_Spt,
  histo_TTPTT_sPOCSA,
  histo_TTP_Spt,
  histo_TTP_sPOCSA,
  histo_all_fctL,
  histo_TTP,
  histo_HRT,
  histo_TTPTT,
  Histo.Sp0CSA.noIgg,
  Histo.Spt.noIgg
)

names(List.variables) <- c(
  "Histo.var",
  "Histo.Spt.var",
  "Histo.Sp0CSA.var",
  "Histo+FctL",
  "histo_minus_Igg",
  "histo_HRT_SP0CSA",
  "histo_HRT_Spt",
  "histo_TTPTT_Spt",
  "histo_TTPTT_sPOCSA",
  "histo_TTP_Spt",
  "histo_TTP_sPOCSA",
  "histo_all_fctL",
  "histo_TTP",
  "histo_HRT",
  "histo_TTPTT",
  "Histo.Sp0CSA.noIgg",
  "Histo.Spt.noIgg"
)

panel_size_df <- tibble(
  model = names(List.variables),
  n_variables = as.numeric(lengths(List.variables))
)

############################################################
# SCALE DATA
############################################################

var_of_md <- colnames(md)
var_of_md <- var_of_md[8:length(var_of_md)]

md_scaled_variables <- as.data.frame(scale(md[, var_of_md]))

md <- cbind(
  md[, c(1, 2, 7)],
  md_scaled_variables
)

md <- as.data.frame(md)

############################################################
# USEFUL FUNCTIONS
############################################################

replace_na_numeric_by_zero <- function(x) {
  x <- as.data.frame(x)
  numeric_cols <- sapply(x, is.numeric)
  
  x[numeric_cols] <- lapply(
    x[numeric_cols],
    function(y) {
      y[is.na(y)] <- 0
      y
    }
  )
  
  return(x)
}

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, ",", ".")
  as.numeric(x)
}

pvalue_label <- function(p) {
  if (is.na(p)) return("NA")
  return(formatC(p, format = "e", digits = 3))
}


############################################################
# MAIN LOOP: LDA + LOOCV + ROC
############################################################

all_model_metric <- list()
all_roc_coordinates <- list()
selected_md_var <- NULL

for (i in seq_along(List.variables)) {
  
  a <- i
  variables <- List.variables[[a]]
  model_name <- names(List.variables)[a]
  
  message("------------------------------------------------------------")
  message("Running model: ", model_name)
  message("Variables: ", paste(variables, collapse = ", "))
  
  missing_variables <- variables[!variables %in% colnames(md)]
  
  if (length(missing_variables) > 0) {
    warning(
      "Model skipped because variables are missing: ",
      paste(missing_variables, collapse = ", ")
    )
    next
  }
  
  ############################################################
  # PREPARE DATA FOR THIS MODEL
  ############################################################
  
  md.var <- cbind(
    md[, c(1, 2, 3)],
    md[, variables, drop = FALSE]
  )
  
  md.var <- replace_na_numeric_by_zero(md.var)
  
  training <- md.var %>%
    filter(
      Protocol %in% c("20-184", "20-066"),
      treatment == "placebo"
    )
  
  colnames(training)[1] <- "training_group"
  
  training <- training[, -c(2, 3)]
  
  training$training_group <- factor(
    training$training_group,
    levels = c(negative_class, positive_class)
  )
  
  if (any(is.na(training$training_group))) {
    warning(
      "Model skipped because some training_group values are not ",
      negative_class,
      " or ",
      positive_class,
      "."
    )
    next
  }
  
  if (length(unique(training$training_group)) < 2) {
    warning("Model skipped because only one class is present in training.")
    next
  }
  
  predictors <- setdiff(colnames(training), "training_group")
  
  nzv <- caret::nearZeroVar(training[, predictors, drop = FALSE])
  
  if (length(nzv) > 0) {
    removed_predictors <- predictors[nzv]
    warning(
      "Near-zero variance predictors removed for ",
      model_name,
      ": ",
      paste(removed_predictors, collapse = ", ")
    )
    training <- training[, !colnames(training) %in% removed_predictors, drop = FALSE]
    variables_for_prediction <- variables[!variables %in% removed_predictors]
  } else {
    variables_for_prediction <- variables
  }
  
  if (length(variables_for_prediction) == 0) {
    warning("Model skipped because no predictor remains after near-zero variance filtering.")
    next
  }
  
  ############################################################
  # TRAIN LDA MODEL
  ############################################################
  
  linear <- tryCatch(
    {
      lda(training_group ~ ., data = training)
    },
    error = function(e) {
      warning("LDA failed for ", model_name, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(linear)) next
  
  ############################################################
  # LOOCV LDA
  ############################################################
  
  linearCV <- tryCatch(
    {
      lda(training_group ~ ., data = training, CV = TRUE)
    },
    error = function(e) {
      warning("LOOCV failed for ", model_name, ": ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(linearCV)) next
  
  predicted_class <- factor(
    linearCV$class,
    levels = levels(training$training_group)
  )
  
  observed_class <- factor(
    training$training_group,
    levels = levels(training$training_group)
  )
  
  Conf <- confusionMatrix(
    data = predicted_class,
    reference = observed_class,
    positive = positive_class
  )
  
  Sing.value <- linear$svd[1]
  
  ############################################################
  # ROC / AUROC / P-VALUE FROM LOOCV POSTERIOR PROBABILITIES
  ############################################################
  
  if (!positive_class %in% colnames(linearCV$posterior)) {
    
    warning(
      "ROC not calculated for ",
      model_name,
      ": posterior probability for ",
      positive_class,
      " not found."
    )
    
    AUROC <- NA
    AUROC_CI_low <- NA
    AUROC_CI_high <- NA
    ROC_pvalue <- NA
    roc_obj <- NULL
    
  } else {
    
    posterior_score <- linearCV$posterior[, positive_class]
    
    roc_obj <- roc(
      response = training$training_group,
      predictor = posterior_score,
      levels = c(negative_class, positive_class),
      direction = "<",
      quiet = TRUE
    )
    
    AUROC <- as.numeric(auc(roc_obj))
    
    AUROC_CI <- tryCatch(
      {
        ci.auc(roc_obj, method = "delong")
      },
      error = function(e) {
        c(NA, NA, NA)
      }
    )
    
    AUROC_CI_low <- as.numeric(AUROC_CI[1])
    AUROC_CI_high <- as.numeric(AUROC_CI[3])
    
    ROC_pvalue <- tryCatch(
      {
        as.numeric(
          roc.test(
            roc_obj,
            auc = 0.5,
            method = "delong",
            alternative = "greater"
          )$p.value
        )
      },
      error = function(e) {
        wilcox.test(
          posterior_score[training$training_group == positive_class],
          posterior_score[training$training_group == negative_class],
          alternative = "greater",
          exact = FALSE
        )$p.value
      }
    )
    
    ############################################################
    # SAVE ROC COORDINATES
    ############################################################
    
    roc_coord <- data.frame(
      model = model_name,
      specificity = roc_obj$specificities,
      sensitivity = roc_obj$sensitivities,
      FPR = 1 - roc_obj$specificities,
      threshold = roc_obj$thresholds
    )
    
    all_roc_coordinates[[model_name]] <- roc_coord
    
    write.xlsx(
      x = roc_coord,
      file = paste0(
        "Results_ROC_coordinates_",
        date_tag,
        "_",
        md.sex,
        "_",
        md.muscle,
        "_",
        model_name,
        ".xlsx"
      ),
      sheetName = "ROC",
      append = FALSE
    )
    
    ############################################################
    # SAVE INDIVIDUAL ROC PLOT
    ############################################################
    
    roc_plot <- ggroc(roc_obj, legacy.axes = TRUE) +
      geom_abline(
        intercept = 0,
        slope = 1,
        linetype = "dashed",
        color = "grey50"
      ) +
      theme_bw(base_size = 12) +
      labs(
        title = paste0("ROC curve - ", model_name),
        subtitle = paste0(
          "LOOCV AUROC = ",
          round(AUROC, 3),
          " [",
          round(AUROC_CI_low, 3),
          "-",
          round(AUROC_CI_high, 3),
          "]",
          "; p = ",
          pvalue_label(ROC_pvalue)
        ),
        x = "1 - Specificity",
        y = "Sensitivity"
      )
    
    ggsave(
      filename = paste0(
        "ROC_",
        date_tag,
        "_",
        md.sex,
        "_",
        md.muscle,
        "_",
        model_name,
        ".png"
      ),
      plot = roc_plot,
      width = 6,
      height = 5,
      dpi = 300
    )
  }
  
  ############################################################
  # PREDICT LDA SCORE FOR ALL ANALYZED SAMPLES
  ############################################################
  
  p <- predict(
    linear,
    md.var[, variables_for_prediction, drop = FALSE]
  )
  
  md.var$lda <- p$x[, 1]
  
  WT_lda_mean <- md.var %>%
    filter(
      Protocol == "20-184",
      group == positive_class
    ) %>%
    summarise(mean_lda = mean(lda, na.rm = TRUE)) %>%
    pull(mean_lda)
  
  Ko_lda_mean <- md.var %>%
    filter(
      Protocol == "20-184",
      group == negative_class
    ) %>%
    summarise(mean_lda = mean(lda, na.rm = TRUE)) %>%
    pull(mean_lda)
  
  md.var$lda.prop.ctl <- (
    (md.var$lda + sqrt(Ko_lda_mean^2)) * 100
  ) / (
    WT_lda_mean + sqrt(Ko_lda_mean^2)
  )
  
  write.xlsx(
    x = md.var,
    file = paste0(
      "Results_LDA_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      "_",
      model_name,
      ".xlsx"
    ),
    sheetName = "LDA",
    append = FALSE
  )
  
  if (model_name == selected_model) {
    selected_md_var <- md.var
  }
  
  ############################################################
  # DUNNETT TEST AGAINST KO
  ############################################################
  
  dtt <- md.var %>%
    filter(Protocol == "20-184")
  
  Dtt <- tryCatch(
    {
      DunnettTest(
        x = dtt$lda,
        g = dtt$group,
        control = negative_class
      )
    },
    error = function(e) {
      warning("Dunnett test failed for ", model_name, ": ", e$message)
      return(NULL)
    }
  )
  
  if (!is.null(Dtt)) {
    write.xlsx(
      x = Dtt[[negative_class]],
      file = paste0(
        "Results_Dunnett_",
        date_tag,
        "_",
        md.sex,
        "_",
        md.muscle,
        "_",
        model_name,
        ".xlsx"
      ),
      sheetName = "Dunnett",
      append = FALSE
    )
  }
  
  ############################################################
  # SAVE CROSS-VALIDATION + ROC METRICS
  ############################################################
  
  Cros.val <- data.frame(
    model = model_name,
    n_variables = length(variables),
    Sensitivity = as.numeric(Conf$byClass["Sensitivity"]),
    Specificity = as.numeric(Conf$byClass["Specificity"]),
    Accuracy = as.numeric(Conf$overall["Accuracy"]),
    BalancedAccuracy = as.numeric(Conf$byClass["Balanced Accuracy"]),
    AUROC = AUROC,
    AUROC_CI_low = AUROC_CI_low,
    AUROC_CI_high = AUROC_CI_high,
    ROC_pvalue = ROC_pvalue,
    singular = Sing.value,
    stringsAsFactors = FALSE
  )
  
  all_model_metric[[model_name]] <- Cros.val
  
  write.xlsx(
    x = Cros.val,
    file = paste0(
      "Results_Crossval_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      "_",
      model_name,
      ".xlsx"
    ),
    sheetName = "Cros-val",
    append = FALSE
  )
}

############################################################
# BIND ALL RESULTS
############################################################

if (length(all_model_metric) == 0) {
  stop("No LDA model was successfully fitted.")
}

plot_tbl <- bind_rows(all_model_metric) %>%
  left_join(panel_size_df, by = "model", suffix = c("", "_expected")) %>%
  mutate(
    n_variables = ifelse(
      is.na(n_variables),
      n_variables_expected,
      n_variables
    ),
    selected = model == selected_model
  ) %>%
  dplyr::select(-n_variables_expected)

if (!primary_metric %in% colnames(plot_tbl)) {
  stop("primary_metric must be one of: Accuracy, Sensitivity, Specificity, BalancedAccuracy, AUROC")
}

plot_tbl <- plot_tbl %>%
  arrange(
    desc(.data[[primary_metric]]),
    n_variables,
    desc(Accuracy)
  ) %>%
  mutate(
    model_label = paste0(model, " (n=", n_variables, ")"),
    model_label = factor(model_label, levels = rev(model_label))
  )

############################################################
# EXPORT RANKING TABLE
############################################################

ranking_tbl <- plot_tbl %>%
  arrange(
    desc(.data[[primary_metric]]),
    n_variables,
    desc(Accuracy)
  ) %>%
  dplyr::select(
    model,
    n_variables,
    AUROC,
    AUROC_CI_low,
    AUROC_CI_high,
    ROC_pvalue,
    Accuracy,
    Sensitivity,
    Specificity,
    BalancedAccuracy,
    singular
  )

write.csv(
  ranking_tbl,
  file = file.path(
    wd,
    paste0(
      "Ranking_panels_LDA_ROC_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".csv"
    )
  ),
  row.names = FALSE
)

write.xlsx(
  x = ranking_tbl,
  file = file.path(
    wd,
    paste0(
      "Ranking_panels_LDA_ROC_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".xlsx"
    )
  ),
  sheetName = "Ranking",
  append = FALSE
)

############################################################
# HEATMAP OF MODEL METRICS
############################################################

heatmap_df <- ranking_tbl %>%
  dplyr::select(
    model,
    AUROC,
    Accuracy,
    Sensitivity,
    Specificity,
    BalancedAccuracy
  ) %>%
  column_to_rownames("model")

png(
  filename = file.path(
    wd,
    paste0(
      "Heatmap_LDA_metrics_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".png"
    )
  ),
  width = 1800,
  height = 1400,
  res = 200
)

pheatmap(
  heatmap_df,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  display_numbers = TRUE,
  number_format = "%.3f",
  main = "LDA model performance metrics"
)

dev.off()

############################################################
# PANEL A: ACCURACY / SENSITIVITY / SPECIFICITY / AUROC
#La sensitivity/specificity LDA répondent à :
#  Avec la règle de classification automatique de la LDA, combien de WT et de Ko sont bien classés ?
# L’AUROC répond à :
# Est-ce que le score du modèle permet de séparer les WT des Ko, indépendamment du seuil choisi ?
# Les probabilités postérieures LOOCV sont les probabilités d’appartenir à Ko ou WT, prédites pour 
# chaque animal par un modèle LDA qui n’a pas vu cet animal pendant l’entraînement.
############################################################

panelA_df <- plot_tbl %>%
  dplyr::select(
    model,
    model_label,
    selected,
    Accuracy,
    Sensitivity,
    Specificity,
    AUROC
  ) %>%
  pivot_longer(
    cols = c(Accuracy, Sensitivity, Specificity, AUROC),
    names_to = "metric",
    values_to = "value"
  )

metric_colors <- c(
  "Accuracy" = "#1f77b4",
  "Sensitivity" = "#2ca02c",
  "Specificity" = "#ff7f0e",
  "AUROC" = "#9467bd"
)

pA <- ggplot(
  panelA_df,
  aes(
    x = value,
    y = model_label,
    color = metric
  )
) +
  geom_vline(
    xintercept = seq(0.50, 1.00, by = 0.05),
    color = "grey90",
    linewidth = 0.35
  ) +
  geom_point(
    position = position_dodge(width = 0.60),
    size = 3.2
  ) +
  geom_point(
    data = subset(panelA_df, selected),
    aes(
      x = value,
      y = model_label
    ),
    position = position_dodge(width = 0.60),
    shape = 21,
    size = 4.7,
    stroke = 1.2,
    fill = NA,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(label = number(value, accuracy = 0.001)),
    position = position_dodge(width = 0.60),
    hjust = -0.20,
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = metric_colors) +
  scale_x_continuous(
    limits = c(0.49, 1.05),
    breaks = seq(0.50, 1.00, by = 0.05)
  ) +
  labs(
    title = "Panel A - Classification performance",
    subtitle = paste("Panels ranked by", primary_metric),
    x = "Cross-validation performance",
    y = NULL,
    color = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )

############################################################
# PANEL B: PERFORMANCE VS NUMBER OF VARIABLES
############################################################

pB <- ggplot(
  plot_tbl,
  aes(
    x = n_variables,
    y = .data[[primary_metric]]
  )
) +
  geom_hline(
    yintercept = seq(0.50, 1.00, by = 0.05),
    color = "grey90",
    linewidth = 0.35
  ) +
  geom_point(
    aes(fill = selected),
    shape = 21,
    size = 4.5,
    color = "black",
    stroke = 0.7
  ) +
  geom_text_repel(
    aes(label = model),
    size = 3.2,
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf
  ) +
  scale_fill_manual(
    values = c(
      "TRUE" = "#d62728",
      "FALSE" = "grey75"
    )
  ) +
  scale_x_continuous(
    breaks = sort(unique(plot_tbl$n_variables))
  ) +
  scale_y_continuous(
    limits = c(0.50, 1.01),
    breaks = seq(0.50, 1.00, by = 0.05)
  ) +
  labs(
    title = "Panel B - Performance vs number of variables",
    subtitle = paste("Metric shown:", primary_metric),
    x = "Number of variables in the panel",
    y = primary_metric
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

############################################################
# FINAL PANEL FIGURE
############################################################

final_plot <- pA + pB + plot_layout(widths = c(1.7, 1))

print(final_plot)

ggsave(
  filename = file.path(
    wd,
    paste0(
      "Panel_A_selection_performance_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".png"
    )
  ),
  plot = pA,
  width = 11,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    wd,
    paste0(
      "Panel_B_parsimony_vs_performance_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".png"
    )
  ),
  plot = pB,
  width = 8,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(
    wd,
    paste0(
      "Figure_panels_A_B_LDA_ROC_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".png"
    )
  ),
  plot = final_plot,
  width = 17,
  height = 8,
  dpi = 300
)

ggsave(
  filename = file.path(
    wd,
    paste0(
      "Figure_panels_A_B_LDA_ROC_",
      date_tag,
      "_",
      md.sex,
      "_",
      md.muscle,
      ".pdf"
    )
  ),
  plot = final_plot,
  width = 17,
  height = 8
)

############################################################
# ALL ROC CURVES ON ONE FIGURE
############################################################

if (length(all_roc_coordinates) > 0) {
  
  all_roc_df <- bind_rows(all_roc_coordinates)
  
  all_roc_plot <- ggplot(
    all_roc_df,
    aes(
      x = FPR,
      y = sensitivity,
      color = model
    )
  ) +
    geom_line(linewidth = 0.9) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "dashed",
      color = "grey50"
    ) +
    theme_bw(base_size = 12) +
    labs(
      title = "ROC curves for all LDA panels",
      subtitle = "ROC curves based on LOOCV posterior probabilities",
      x = "1 - Specificity",
      y = "Sensitivity",
      color = "Model"
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold")
    )
  
  print(all_roc_plot)
  
  ggsave(
    filename = file.path(
      wd,
      paste0(
        "All_ROC_curves_LDA_",
        date_tag,
        "_",
        md.sex,
        "_",
        md.muscle,
        ".png"
      )
    ),
    plot = all_roc_plot,
    width = 10,
    height = 7,
    dpi = 300
  )
}

############################################################
# LDA SCORE BARPLOT FOR SELECTED MODEL
############################################################

if (!is.null(selected_md_var)) {
  
  compo <- c(
    "WT",
    "Ko",
    "C3-KO + GNT0008 [1.1E13 vg/kg]",
    "C3-KO + GNT0008 [2.7E13 vg/kg]",
    "C3-KO + GNT0008 [5.4E13 vg/kg]",
    "C3-KO + GNT0008 [1.6E14 vg/kg]"
  )
  
  selected_md_var$group <- factor(
    selected_md_var$group,
    levels = compo
  )
  
  lda_barplot <- ggplot(
    selected_md_var,
    aes(
      x = group,
      y = lda,
      fill = treatment
    )
  ) +
    stat_summary(
      fun = mean,
      geom = "col",
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      position = position_dodge(width = 0.8),
      width = 0.2
    ) +
    theme_bw(base_size = 12) +
    labs(
      title = paste0("LDA score - selected model: ", selected_model),
      x = "Group",
      y = "Mean LDA score ± SE",
      fill = "Treatment"
    ) +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      plot.title = element_text(face = "bold")
    )
  
  print(lda_barplot)
  
  ggsave(
    filename = file.path(
      wd,
      paste0(
        "LDA_score_barplot_",
        selected_model,
        "_",
        date_tag,
        "_",
        md.sex,
        "_",
        md.muscle,
        ".png"
      )
    ),
    plot = lda_barplot,
    width = 10,
    height = 6,
    dpi = 300
  )
}

############################################################
# PRINT FINAL RANKING
############################################################

############################################################
# PRINT FINAL RANKING
############################################################

ranking_tbl <- tibble::as_tibble(ranking_tbl)

print(
  ranking_tbl,
  n = Inf,
  width = Inf
)

message("Analysis completed.")
message(
  paste0(
    "Main ranking file saved as: Ranking_panels_LDA_ROC_",
    date_tag,
    "_",
    md.sex,
    "_",
    md.muscle,
    ".xlsx"
  )
)
message("ROC p-value tests H0: AUROC = 0.5.")

message("Analysis completed.")
message("Main ranking file saved as: Ranking_panels_LDA_ROC_", date_tag, "_", md.sex, "_", md.muscle, ".xlsx")
message("ROC p-value tests H0: AUROC = 0.5.")



###################################"

############################################################
# GRAPH FROM EXISTING RANKING FILE
# Accuracy / Sensitivity / Specificity
# Histo sans IgG only
############################################################

library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)

# Si wd existe déjà dans ton script, il sera utilisé.
# Sinon, on prend le dossier courant.
if (!exists("wd")) {
  wd <- getwd()
}

############################################################
# FIND EXISTING RANKING FILE
############################################################

ranking_files <- list.files(
  path = wd,
  pattern = "^Ranking_panels_LDA_ROC_.*\\.(xlsx|csv)$",
  full.names = TRUE
)

if (length(ranking_files) == 0) {
  stop("Aucun fichier Ranking_panels_LDA_ROC_*.xlsx ou *.csv trouvé dans wd.")
}

# Prend le fichier le plus récent
ranking_file <- ranking_files[which.max(file.info(ranking_files)$mtime)]

message("Fichier utilisé : ", basename(ranking_file))

############################################################
# READ RANKING FILE
############################################################

if (grepl("\\.xlsx$", ranking_file, ignore.case = TRUE)) {
  ranking_tbl <- readxl::read_excel(ranking_file)
} else {
  ranking_tbl <- read.csv(ranking_file, check.names = FALSE)
}

############################################################
# KEEP ONLY REQUIRED MODELS
############################################################

models_to_keep <- c(
  "histo_minus_Igg",
  "Histo.Spt.noIgg",
  "Histo.Sp0CSA.noIgg"
)

model_labels_clean <- c(
  "histo_minus_Igg" = "Histo sans IgG",
  "Histo.Spt.noIgg" = "Histo + sPt sans IgG",
  "Histo.Sp0CSA.noIgg" = "Histo + sPOCSA sans IgG"
)

missing_models <- setdiff(models_to_keep, ranking_tbl$model)

if (length(missing_models) > 0) {
  warning(
    "Modèles non trouvés dans le fichier ranking : ",
    paste(missing_models, collapse = ", "),
    "\nModèles disponibles : ",
    paste(unique(ranking_tbl$model), collapse = ", ")
  )
}

panelA_df <- ranking_tbl %>%
  filter(model %in% models_to_keep) %>%
  mutate(
    model_clean = recode(model, !!!model_labels_clean),
    model_clean = factor(
      model_clean,
      levels = rev(unname(model_labels_clean[models_to_keep]))
    )
  ) %>%
  dplyr::select(
    model,
    model_clean,
    Accuracy,
    Sensitivity,
    Specificity
  ) %>%
  mutate(
    across(
      c(Accuracy, Sensitivity, Specificity),
      as.numeric
    )
  ) %>%
  pivot_longer(
    cols = c(Accuracy, Sensitivity, Specificity),
    names_to = "metric",
    values_to = "value"
  )

if (nrow(panelA_df) == 0) {
  stop("Aucune donnée disponible pour les 3 modèles demandés.")
}

############################################################
# PLOT
############################################################

metric_colors <- c(
  "Accuracy" = "#1f77b4",
  "Sensitivity" = "#2ca02c",
  "Specificity" = "#ff7f0e"
)

pA <- ggplot(
  panelA_df,
  aes(
    x = value,
    y = model_clean,
    color = metric
  )
) +
  geom_vline(
    xintercept = seq(0.50, 1.00, by = 0.05),
    color = "grey90",
    linewidth = 0.35
  ) +
  geom_point(
    position = position_dodge(width = 0.55),
    size = 3.5
  ) +
  geom_text(
    aes(label = number(value, accuracy = 0.001)),
    position = position_dodge(width = 0.55),
    hjust = -0.20,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = metric_colors) +
  scale_x_continuous(
    limits = c(0.49, 1.05),
    breaks = seq(0.50, 1.00, by = 0.05)
  ) +
  labs(
    title = "Classification performance - Histology without IgG",
    subtitle = "Accuracy, sensitivity and specificity from LOOCV LDA",
    x = "Cross-validation performance",
    y = NULL,
    color = NULL
  ) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 11)
  )

print(pA)

############################################################
# SAVE
############################################################

ggsave(
  filename = file.path(
    wd,
    paste0(
      "Accuracy_Sensitivity_Specificity_Histo_noIgG_from_existing_ranking.png"
    )
  ),
  plot = pA,
  width = 9,
  height = 4.5,
  dpi = 300
)

ggsave(
  filename = file.path(
    wd,
    paste0(
      "Accuracy_Sensitivity_Specificity_Histo_noIgG_from_existing_ranking.pdf"
    )
  ),
  plot = pA,
  width = 9,
  height = 4.5
)