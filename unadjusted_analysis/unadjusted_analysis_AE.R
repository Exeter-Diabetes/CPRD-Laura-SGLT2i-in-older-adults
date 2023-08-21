#
# Absolute risk analysis - adverse effects -------------------------------------
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

# install.packages("survival")
library(survival)

# install.packages("survminer")
library(survminer)


#
# Decision on analysis ---------------------------------------------------------
#

year_type           <- c("1year","3year")[2]
years               <- as.numeric(substr(year_type, 1, 1))
censoring_type      <- c("A","B")[1]
stratification_type <- c("age", "gender")[1]

if(stratification_type == "age"){
  
  population_type     <- c("not_elderly", "elderly")
  
  }else{
    
  population_type     <- c("study_population_female", "study_population_male")
  
  }

outcome_interest    <- c("GI", "OS", "MC", "VD", "UF", "falls", "llffalls", "amputation", "dka")


#
# Make result table ------------------------------------------------------------
#

results_AR            <- data.frame(matrix(NA, ncol = 6, nrow = (length(outcome_interest)*4)))
colnames(results_AR)  <- c("outcome", "population", "treatment", "AR", "lower", "upper")
results_AR$outcome    <- rep(outcome_interest, each = 4)
results_AR$treatment  <- rep(c("DPP4", "SGLT2"), times = length(outcome_interest)*2)

if(stratification_type == "age"){
  
  results_AR$population <- rep(c("not_elderly", "not_elderly", "elderly", "elderly"), times = length(outcome_interest))
  
}else{
    
  results_AR$population <- rep(c("male", "male", "female", "female"), times = length(outcome_interest))
  
  }


#
# Load data --------------------------------------------------------------------
#


load(paste0(data_path,"/study_cohort_",year_type,"_prepared.Rdata"))

for(o in 1:length(outcome_interest)){
  
  for(p in 1:length(population_type)){
    
    if(population_type[p] == "not_elderly"){
      
      data <- study_cohort_prepared[study_cohort_prepared$elderly == 0, ]
      
    }else if(population_type[p] == "elderly"){
      
      data <- study_cohort_prepared[study_cohort_prepared$elderly == 1, ]
      
    }else if(population_type[p] == "study_population_female"){
      
      data <- study_cohort_prepared[study_cohort_prepared$gender == 2, ]
      
    }else{
      
      data <- study_cohort_prepared[study_cohort_prepared$gender == 1, ]
      
    }
    
    
    
    source(paste0(variableset_path, "/variableset_", outcome_interest[o], ".R")) 
    
    
    time        <- followup_time   
    status      <- data[, Y] 
    
    Surv_object <- Surv(time = time, event = status)                            # make survival object
    
    Surv_model  <- survfit(Surv_object ~ drugclass, data = data, type = "kaplan-meier")
    
    
    # AR
    results_AR$AR[results_AR$population == population_type[p] & results_AR$outcome == outcome_interest[o] & results_AR$treatment == "DPP4" ]     <- 1-Surv_model$surv[Surv_model$time == 365*years][1]
    results_AR$AR[results_AR$population == population_type[p] & results_AR$outcome == outcome_interest[o] & results_AR$treatment == "SGLT2" ]    <- 1-Surv_model$surv[Surv_model$time == 365*years][2]
    
    # lower 
    results_AR$lower[results_AR$population == population_type[p] & results_AR$outcome == outcome_interest[o] & results_AR$treatment == "DPP4" ]  <- 1-Surv_model$upper[Surv_model$time == 365*years][1]
    results_AR$lower[results_AR$population == population_type[p] & results_AR$outcome == outcome_interest[o] & results_AR$treatment == "SGLT2" ] <- 1-Surv_model$upper[Surv_model$time == 365*years][2]
    
    # upper 
    results_AR$upper[results_AR$population == population_type[p] & results_AR$outcome == outcome_interest[o] & results_AR$treatment == "DPP4" ]  <- 1-Surv_model$lower[Surv_model$time == 365*years][1]
    results_AR$upper[results_AR$population == population_type[p] & results_AR$outcome == outcome_interest[o] & results_AR$treatment == "SGLT2" ] <- 1-Surv_model$lower[Surv_model$time == 365*years][2]
    
    
  }
  
  
}


save(results_AR, file = paste0(result_path, "/results_AR_", year_type, "_c", censoring_type,"_",stratification_type,"_stratification.Rdata"))










