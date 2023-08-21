#
## Instrumental variable method (ppIV previous patient) ------------------------
#


## load needed data ----

load(paste0(data_path,"/study_cohort_",year_type,"_prepared.Rdata"))

if(population_type == "study_population"){
  
  data <- study_cohort_prepared
  
  }else if(population_type == "not_elderly"){
    
    data <- study_cohort_prepared[study_cohort_prepared$elderly == 0, ]
    
    }else if(population_type == "elderly"){
      
      data <- study_cohort_prepared[study_cohort_prepared$elderly == 1, ]
      
      }else if(population_type == "study_population_female"){
        
        data <- study_cohort_prepared[study_cohort_prepared$gender == 2, ]
        
        }else{
          
          data <- study_cohort_prepared[study_cohort_prepared$gender == 1, ]
          
          }

rm(study_cohort_prepared)



## define variable set ----

source(paste0("variableset_", which_outcome,".R"))
Z                <- "IV_prevpatient"

## generate complete case dataset ----


if(outcome_variable_type == "binary"){
  
  variables_cc    <- c(all_variables, paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = ""), Z)
  data_cc         <- data[complete.cases(data[ , variables_cc]), ]
  
  ## define follow up time variable ----
  
  followup_time   <- data_cc[ , paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = "")]
  
  }else{
    
    variables_cc    <- c(all_variables, Z)
    data_cc         <- data[complete.cases(data[ , variables_cc]), ]
    
    }


## Estimation of the model (TSLS) ----


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  IV_prevpatient_model1_formula  <- as.formula(paste(X, " ~ ", paste0(c(all_W_notgender, Z), collapse =  " + ")))
  
  }else{
    
    IV_prevpatient_model1_formula  <- as.formula(paste(X, " ~ ", paste0(c(all_W, Z), collapse =  " + ")))
    
    }


IV_prevpatient_model1             <- glm(IV_prevpatient_model1_formula, family = binomial, data = data_cc)

data_cc$X_hat_IV_prevpatient      <- as.numeric(IV_prevpatient_model1$fitted.values)
X_hat                             <- "X_hat_IV_prevpatient"


if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  IV_prevpatient_model2_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W_notgender), collapse =  " + ")))
  
  }else{
    
    IV_prevpatient_model2_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X_hat, all_W), collapse =  " + ")))
    
    }


if(outcome_variable_type == "binary"){
  
  IV_prevpatient_model2           <- glm(IV_prevpatient_model2_formula, offset = log(followup_time[!followup_time == 0]), family = poisson(link = log), data = data_cc[!followup_time == 0, ])
  
  
}else{
  
  IV_prevpatient_model2           <- lm(IV_prevpatient_model2_formula, data = data_cc)

}


## Extraction of the results ----

assign(paste0("IV_prevpatient_",which_outcome,"_model_summary"), summ(IV_prevpatient_model2, confint = TRUE, digits = 4))
assign(paste0("IV_prevpatient_",which_outcome,"_model1"),        IV_prevpatient_model1)











