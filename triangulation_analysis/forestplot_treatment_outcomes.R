#
# Forest plot (continuous outcomes) --------------------------------------------
#


#
# Packages ---------------------------------------------------------------------
#

# install.packages("Cairo")
library(Cairo)

#install.packages("forestplot")
library(forestplot)


#
# Set paths --------------------------------------------------------------------
#
# please set paths

setwd(" ")

result_path <- " "


#
# Decision on analysis ---------------------------------------------------------
#

year_type           <- c("1year","3year")[2]                                    # change year according to outcome follow up (sensitivity analysis)
censoring_type      <- c("A","B")[1]                                            # change according to censoring type (sensitivity analysis)
stratification_type <- c("age", "gender")[1]
outcome_name        <- c("HbA1c", "weight")[2]


if(stratification_type == "age"){
  
  population_type     <- c("not_elderly", "elderly")  
  
}else{
  
  population_type     <- c("female", "male")
  
}

if(outcome_name == "HbA1c"){
  
  xlab_outcome     <- "Difference in HbA1c (mmol/mol)" 
  
}else{
  
  xlab_outcome     <- "Difference in weight (kg)"
  
}




#
# Load data for the plot -------------------------------------------------------
#
# load results

load(paste0("/results_change_",year_type, "_c", censoring_type,"_",stratification_type,"_stratification.Rdata"))



#
# Prepare data frame for absolute risk forest plot -----------------------------
#


outcome_names     <- c(outcome_name,                         population_type[1], population_type[2])

estimate_plot     <- c(NA, results_change$causal_estimate[results_change$outcome == outcome_name])
lower_plot        <- c(NA, results_change$lower[results_change$outcome == outcome_name])
upper_plot        <- c(NA, results_change$upper[results_change$outcome == outcome_name])


text_col                 <- paste(as.character(round(estimate_plot, 1)),
                                  rep(" (", times = length(estimate_plot)),  
                                  as.character(round(lower_plot, 1)),  
                                  rep(", ", times = length(lower_plot)),
                                  as.character(round(upper_plot, 1)),  
                                  rep(")", times = length(upper_plot)), sep = "")  
text_col[text_col == "NA (NA, NA)"] <- ""  


data_plot         <- data.frame(cbind(outcome_names, estimate_plot, lower_plot, upper_plot, text_col))

colnames(data_plot) <- c("outcome_names", "mean", "lower", "upper", "text_col")

data_plot$mean      <- as.numeric(data_plot$mean)
data_plot$lower     <- as.numeric(data_plot$lower)
data_plot$upper     <- as.numeric(data_plot$upper)
data_plot$outcome_names[data_plot$outcome_names == "not_elderly"] <- "   <70 years"
data_plot$outcome_names[data_plot$outcome_names == "elderly"]     <- "   >70 years"

if(outcome_name == "weight"){
  
  data_plot[1,1] <- "Weight"
  
}else{
  
  data_plot[1,1] <- outcome_name
  
}



#
# Forest plot, part A for relative risk ----------------------------------------
#

Cairo(file = paste0(result_path,"/forestplot_IV_continuous_",outcome_name,"_",year_type,"_c", censoring_type,"_",stratification_type,"_stratification_RR_mainresults.png" ), 
      type = "png",
      units = "in", 
      width = 20,#10, 20
      height = 6, #8, 16
      pointsize = 12, 
      dpi = 72)

data_plot|>
forestplot(labeltext = c(outcome_names, text_col),
           xlog = F,
           ci.vertices = TRUE,
           ci.vertices.height = 0.15, # 0.05
           boxsize = 0.2, # 0.1
           zero = 0,  
           lwd.zero = 3,
           col = fpColors(box = c("blue"), lines = c("black"), zero = "black"), 
           fn.ci_norm = c("fpDrawCircleCI"), 
           xlab = xlab_outcome,
           txt_gp = fpTxtGp(cex = 2, xlab = gpar(cex = 2), ticks = gpar(cex = 2)),
           lty.ci = c(1),
           new_page = TRUE)|>
  fp_add_header(outcome_names = "Treatment outcome", text_col = "Estimate (CI 95%)")|> 
  fp_decorate_graph(graph.pos = 2)


dev.off()



























