#
# Absolute risk analysis - treatment outcomes ----------------------------------
#


#
# Set paths --------------------------------------------------------------------
# 
# please set paths here

setwd(" ")

result_path      <- " "
data_path        <- " "
variableset_path <- " "

#
# Load packages ----------------------------------------------------------------
#


#
# Decision on analysis ---------------------------------------------------------
#

year_type           <- "3year"
#years               <- as.numeric(substr(year_type, 1, 1))
censoring_type      <- c("A","B")[1]
stratification_type <- c("age", "gender")[1]

population_type     <- c("not_elderly", "elderly")
outcome_interest    <- c("HbA1c", "weight")


#
# Make result table ------------------------------------------------------------
#

results_AR            <- data.frame(matrix(NA, ncol = 6, nrow = (length(outcome_interest)*4)))
colnames(results_AR)  <- c("outcome", "population", "treatment", "AR", "lower", "upper")
results_AR$outcome    <- rep(outcome_interest, each = 4)
results_AR$treatment  <- rep(c("DPP4", "SGLT2"), times = length(outcome_interest)*2)
results_AR$population <- rep(c("not_elderly", "not_elderly", "elderly", "elderly"), times = length(outcome_interest))

#
# Load data --------------------------------------------------------------------
#


load(paste0(data_path,"/study_cohort_",year_type,"_prepared.Rdata"))
data <- study_cohort_prepared

#
# Calculation of the absolute effect -------------------------------------------
#

n_DPP4_notelderly_hba1c  <- length(data$outcome_hba1c[data$treatment == "0" & data$elderly == 0]) - as.numeric(table(is.na(data$outcome_hba1c[data$treatment == "0" & data$elderly == 0]))["TRUE"])
n_DPP4_elderly_hba1c     <- length(data$outcome_hba1c[data$treatment == "0" & data$elderly == 1]) - as.numeric(table(is.na(data$outcome_hba1c[data$treatment == "0" & data$elderly == 1]))["TRUE"])

n_SGLT2_notelderly_hba1c <- length(data$outcome_hba1c[data$treatment == "1" & data$elderly == 0]) - as.numeric(table(is.na(data$outcome_hba1c[data$treatment == "1" & data$elderly == 0]))["TRUE"])
n_SGLT2_elderly_hba1c    <- length(data$outcome_hba1c[data$treatment == "1" & data$elderly == 1]) - as.numeric(table(is.na(data$outcome_hba1c[data$treatment == "1" & data$elderly == 1]))["TRUE"])

n_DPP4_notelderly_weight  <- length(data$outcome_weight[data$treatment == "0" & data$elderly == 0]) - as.numeric(table(is.na(data$outcome_weight[data$treatment == "0" & data$elderly == 0]))["TRUE"])
n_DPP4_elderly_weight     <- length(data$outcome_weight[data$treatment == "0" & data$elderly == 1]) - as.numeric(table(is.na(data$outcome_weight[data$treatment == "0" & data$elderly == 1]))["TRUE"])

n_SGLT2_notelderly_weight <- length(data$outcome_weight[data$treatment == "1" & data$elderly == 0]) - as.numeric(table(is.na(data$outcome_weight[data$treatment == "1" & data$elderly == 0]))["TRUE"])
n_SGLT2_elderly_weight    <- length(data$outcome_weight[data$treatment == "1" & data$elderly == 1]) - as.numeric(table(is.na(data$outcome_weight[data$treatment == "1" & data$elderly == 1]))["TRUE"])


## estimates

# HbA1c
results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  <- mean(data$outcome_hba1c[data$treatment == "0" & data$elderly == 0], na.rm = TRUE) # DPP4i, not elderly
results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      <- mean(data$outcome_hba1c[data$treatment == "0" & data$elderly == 1], na.rm = TRUE) # DPP4i, elderly
results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] <- mean(data$outcome_hba1c[data$treatment == "1" & data$elderly == 0], na.rm = TRUE) # SGLT2i, not elderly
results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     <- mean(data$outcome_hba1c[data$treatment == "1" & data$elderly == 1], na.rm = TRUE) # SGLT2i, elderly 


# weight
results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  <- mean(data$outcome_weight[data$treatment == "0" & data$elderly == 0], na.rm = TRUE) # DPP4i, not elderly
results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      <- mean(data$outcome_weight[data$treatment == "0" & data$elderly == 1], na.rm = TRUE) # DPP4i, elderly
results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] <- mean(data$outcome_weight[data$treatment == "1" & data$elderly == 0], na.rm = TRUE) # SGLT2i, not elderly
results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     <- mean(data$outcome_weight[data$treatment == "1" & data$elderly == 1], na.rm = TRUE) # SGLT2i, elderly 

## CI

# HbA1c 
results_AR$lower[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  - 1.96*(sd(data$outcome_hba1c[data$treatment == "0" & data$elderly == 0], na.rm = TRUE)/sqrt(n_DPP4_notelderly_hba1c))  # DPP4i, not elderly
results_AR$lower[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      - 1.96*(sd(data$outcome_hba1c[data$treatment == "0" & data$elderly == 1], na.rm = TRUE)/sqrt(n_DPP4_elderly_hba1c))     # DPP4i, elderly
results_AR$lower[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] - 1.96*(sd(data$outcome_hba1c[data$treatment == "1" & data$elderly == 0], na.rm = TRUE)/sqrt(n_SGLT2_notelderly_hba1c)) # SGLT2i, not elderly
results_AR$lower[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     - 1.96*(sd(data$outcome_hba1c[data$treatment == "1" & data$elderly == 1], na.rm = TRUE)/sqrt(n_SGLT2_elderly_hba1c))    # SGLT2i, elderly 

results_AR$upper[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  + 1.96*(sd(data$outcome_hba1c[data$treatment == "0" & data$elderly == 0], na.rm = TRUE)/sqrt(n_DPP4_notelderly_hba1c))  # DPP4i, not elderly
results_AR$upper[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      + 1.96*(sd(data$outcome_hba1c[data$treatment == "0" & data$elderly == 1], na.rm = TRUE)/sqrt(n_DPP4_elderly_hba1c))     # DPP4i, elderly
results_AR$upper[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] + 1.96*(sd(data$outcome_hba1c[data$treatment == "1" & data$elderly == 0], na.rm = TRUE)/sqrt(n_SGLT2_notelderly_hba1c)) # SGLT2i, not elderly
results_AR$upper[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     <- results_AR$AR[results_AR$outcome == "HbA1c" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     + 1.96*(sd(data$outcome_hba1c[data$treatment == "1" & data$elderly == 1], na.rm = TRUE)/sqrt(n_SGLT2_elderly_hba1c))    # SGLT2i, elderly 

# weight 
results_AR$lower[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  - 1.96*(sd(data$outcome_weight[data$treatment == "0" & data$elderly == 0], na.rm = TRUE)/sqrt(n_DPP4_notelderly_weight))  # DPP4i, not elderly
results_AR$lower[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      - 1.96*(sd(data$outcome_weight[data$treatment == "0" & data$elderly == 1], na.rm = TRUE)/sqrt(n_DPP4_elderly_weight))     # DPP4i, elderly
results_AR$lower[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] - 1.96*(sd(data$outcome_weight[data$treatment == "1" & data$elderly == 0], na.rm = TRUE)/sqrt(n_SGLT2_notelderly_weight)) # SGLT2i, not elderly
results_AR$lower[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     - 1.96*(sd(data$outcome_weight[data$treatment == "1" & data$elderly == 1], na.rm = TRUE)/sqrt(n_SGLT2_elderly_weight))    # SGLT2i, elderly 

results_AR$upper[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "not_elderly"]  + 1.96*(sd(data$outcome_weight[data$treatment == "0" & data$elderly == 0], na.rm = TRUE)/sqrt(n_DPP4_notelderly_weight))   # DPP4i, not elderly
results_AR$upper[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "DPP4" & results_AR$population == "elderly"]      + 1.96*(sd(data$outcome_weight[data$treatment == "0" & data$elderly == 1], na.rm = TRUE)/sqrt(n_DPP4_elderly_weight))      # DPP4i, elderly
results_AR$upper[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "not_elderly"] + 1.96*(sd(data$outcome_weight[data$treatment == "1" & data$elderly == 0], na.rm = TRUE)/sqrt(n_SGLT2_notelderly_weight))  # SGLT2i, not elderly
results_AR$upper[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     <- results_AR$AR[results_AR$outcome == "weight" & results_AR$treatment == "SGLT2" & results_AR$population == "elderly"]     + 1.96*(sd(data$outcome_weight[data$treatment == "1" & data$elderly == 1], na.rm = TRUE)/sqrt(n_SGLT2_elderly_weight))     # SGLT2i, elderly 



save(results_AR, file = paste0(result_path, "/results_AR_txoutcomes_", year_type, "_c", censoring_type,"_",stratification_type,"_stratification.Rdata"))










