#
## Propensity score matched multivariable regression --------------------------- 
#


## load needed data ----

matched_data <- get(load(paste0(data_path,"/matched_data_",which_outcome,"_",year_type,".Rdata")))

rm(list=paste0("matched_data_",which_outcome))


if(population_type == "study_population"){
  
  data <- matched_data
  
  }else if(population_type == "not_elderly"){
    
    data <- matched_data[matched_data$elderly == 0, ]
    
    }else if(population_type == "elderly"){
      
      data <- matched_data[matched_data$elderly == 1, ]
      
      }else if(population_type == "study_population_female"){
        
        data <- matched_data[matched_data$gender == 2, ]
        
        }else{
          
          data <- matched_data[matched_data$gender == 1, ]
          
          }

rm(matched_data)

## define variable set ----

source(paste0("variableset_", which_outcome,".R"))

## generate complete case dataset ----


if(outcome_variable_type == "binary"){
  
  variables_cc     <- c(all_variables, paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = ""))
  data_cc          <- data[complete.cases(data[ , variables_cc]), ]
  
  ## define follow up time variable ----
  
  followup_time    <- data_cc[ , paste("followup_days_", which_outcome, "_c"  , censoring_type, sep = "")]
  
  }else{
    
    variables_cc   <- c(all_variables)
    data_cc        <- data[complete.cases(data[ , variables_cc]), ]
    
    }


## Estimation of the model ----

if(population_type == "study_population_female" | population_type == "study_population_male"){
  
  PSM_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X, all_W_notgender), collapse =  " + ")))
  
  }else{
    
    PSM_formula  <- as.formula(paste(Y, " ~ ", paste0(c(X, all_W), collapse =  " + ")))
    
    }


if(outcome_variable_type == "binary"){
  
  PSM_model    <- glm(PSM_formula, offset = log(followup_time[!followup_time == 0]), family = poisson(link = log), data = data_cc[!followup_time == 0, ])
  
  
}else{
  
  PSM_model    <- lm(PSM_formula, data = data_cc)
  
}


## Extraction of the results ----

assign(paste0("PSM_",which_outcome,"_model_summary"), summ(PSM_model, confint = TRUE, digits = 4))



