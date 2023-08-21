#
# Multivariable regression (unmatched) -----------------------------------------
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

## generate complete case dataset ----

if(outcome_variable_type == "binary"){
  
  variables_cc      <- c(all_variables, paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = ""))
  data_cc           <- data[complete.cases(data[ , variables_cc]), ]
  
  ## define follow up time variable ----
  
  followup_time     <- data_cc[ , paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = "")]
  
  }else{
    
    variables_cc    <- c(all_variables)
    data_cc         <- data[complete.cases(data[ , variables_cc]), ]
    
    
    }

## Estimation of the model ----

if(population_type == "study_population_female" | population_type == "study_population_male"){
 
  MVR_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X, all_W_notgender), collapse =  " + ")))
  
  }else{
    
    MVR_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X, all_W), collapse =  " + ")))
    
    }

if(outcome_variable_type == "binary"){
  
  MVR_model    <- glm(MVR_formula, offset = log(followup_time[!followup_time == 0]), family=poisson(link = log), data = data_cc[!followup_time == 0, ])

  }else{
    
    MVR_model    <- lm(MVR_formula, data = data_cc)
    
    }

## Extraction of the results ----

assign(paste0("MVR_",which_outcome,"_model_summary"), summ(MVR_model, confint = TRUE, digits = 4))



