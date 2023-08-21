# ------------------------------------------------------------------------------
# Plots of the triangulation analysis ------------------------------------------
# ------------------------------------------------------------------------------


#
# Set paths --------------------------------------------------------------------
#

setwd(" ")

result_path <- " "

result_path_plots <- " "

#
# Packages ---------------------------------------------------------------------
#

# install.packages("ggplot2")
library(ggplot2)

# install.packages("Cairo")
library(Cairo)


#
# Decision on analysis ---------------------------------------------------------
#


outcome_interest <- c("HbA1c", "weight")[1]  # change for which outcome plots are computed

year_type        <- c("1year","3year")[2]                                       # change year according to outcome follow up (sensitivity analysis)
censoring_type   <- c("A","B")[1]                                               # change according to censoring type (sensitivity analysis)

results_populations <- c("not_elderly", "elderly")
results_models      <- c("MVR", "PSM", "IV_prevpatient", "IV_Ertefaie")
treatment_name      <- c("treatment","treatment", "X_hat_IV_prevpatient", "X_hat_IV_Ertefaie")
results_outcomes    <- c("GI", "MC", "VD", "UF", "falls", "amputation", "DKA")


#
# Build empty results table ----------------------------------------------------
#


rownumber   <- length(results_populations)*length(results_models)
colnumber   <- 5

result_data           <- as.data.frame(matrix(NA, nrow = rownumber, ncol = colnumber))
colnames(result_data) <- c("cohort", "causal_estimate", "lower", "upper", "methods")
result_data$cohort    <- c(rep("not_elderly", times = length(results_models)) , rep("elderly", times = length(results_models)))
result_data$methods   <- rep(results_models, times = length(results_populations))


#
# Load the result data in results table ----------------------------------------
#


#p <- 2
#m <- 4

for(p in 1:length(results_populations)){
  
  for(m in 1:length(results_models)){
    
    subset_results <- get(load(paste0(result_path, "/",results_models[m], "_", outcome_interest, "_modelsummary_", year_type, "_c", censoring_type, "_",results_populations[p],".Rdata" )))
    
    
    #result_data$causal_estimate[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]] <- exp(subset_results$coeftable[treatment_name[m], "Est."])
    #result_data$lower[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- exp(subset_results$coeftable[treatment_name[m], "2.5%"])
    #result_data$upper[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- exp(subset_results$coeftable[treatment_name[m], "97.5%"])
    
    result_data$causal_estimate[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]] <- (subset_results$coeftable[treatment_name[m], "Est."])
    result_data$lower[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- (subset_results$coeftable[treatment_name[m], "2.5%"])
    result_data$upper[result_data$cohort == results_populations[p] & result_data$methods == results_models[m]]           <- (subset_results$coeftable[treatment_name[m], "97.5%"])
    
    
    rm(subset_results)
  }
  
}


result_data$methods <- c("MVR", "PSM", "IV prevpatient", "IV ePP", "MVR", "PSM", "IV prevpatient", "IV ePP")

result_data$methods <- factor(result_data$methods, level = rev(c("MVR", "PSM", "IV prevpatient", "IV ePP")))
result_data$cohort  <- factor(result_data$cohort, level =  (results_populations))




#
# Create result plot  ----------------------------------------------------------
#




p = ggplot(data=result_data,
           aes(x = methods,y = causal_estimate, ymin = lower, ymax = upper ))+
  geom_pointrange(aes(col=methods))+
  geom_hline(aes(fill=methods),yintercept =1, linetype=1)+
  xlab(' ')+ ylab("Relative risk")+
  geom_errorbar(aes(ymin=lower, ymax=upper,col=methods),width=0.5,cex=1)+ 
  facet_wrap(~cohort,strip.position="left",nrow=9,scales = "free_y") +
  theme(plot.title=element_text(size=16,face="bold"),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_text(size=16, face="bold"),
        axis.title=element_text(size=16,face="bold"),
        strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"), 
        legend.text=element_text(size=15),
        legend.title = element_text(size=16,face="bold"),
        legend.key.size = unit(1, 'cm'))+
        coord_flip() + theme_linedraw(base_size = 20) + ylim(0, 4) #ylim(0, 3) #c(0,10), (0, 15.8)
        #ylim(0, 4)
  

Cairo(file = paste0(result_path_plots,"/triangulation_binary_",year_type,"_c", censoring_type,"_",outcome_interest,".png" ), 
      type = "png",
      units = "in", 
      width = 14,#10, 20
      height = 4, #8, 16
      pointsize = 12, 
      dpi = 72)

 plot(p)

dev.off()


