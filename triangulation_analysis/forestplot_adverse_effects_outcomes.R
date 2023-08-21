#
# Forest plot (binary outcomes) ------------------------------------------------
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

if(stratification_type == "age"){
  
  population_type     <- c("not_elderly", "elderly")  
  
}else{
  
  population_type     <- c("female", "male")
  
}



#
# Load data for the plot -------------------------------------------------------
#
# set paths and load results

# n events
load(paste0("/results_n_events_",year_type,"_c",censoring_type,"_",stratification_type,"_stratification.Rdata"))

results_n_events       <- results_n_events[results_n_events$outcome%in%c("GI", "MC", "VD", "UF", "falls", "amputation", "dka"), ]

results_n_events_DPP4  <- results_n_events[results_n_events$treatment == "DPP4", ]
results_n_events_SGLT2 <- results_n_events[results_n_events$treatment == "SGLT2", ]


# incidence rates
load(paste0("/results_incidence_rates_",year_type,"_c",censoring_type,"_",stratification_type,"_stratification.Rdata"))

results_incidence_rates        <- results_incidence_rates[results_incidence_rates$outcome%in%c("GI", "MC", "VD", "UF", "falls", "amputation", "dka"), ]

results_incidence_rates_DPP4   <- results_incidence_rates[results_incidence_rates$treatment == "DPP4", ]
results_incidence_rates_SGLT2  <- results_incidence_rates[results_incidence_rates$treatment == "SGLT2", ]

# relative risk results

load(paste0("/results_RR_",year_type,"_c",censoring_type,"_",stratification_type,"_stratification.Rdata"))

results_RR <- results_RR[results_RR$outcome%in%c("GI", "MC", "VD", "UF", "falls", "amputation", "dka"), ]


#
# Prepare data frame for absolute risk forest plot -----------------------------
#


outcome_names           <- c("Genital infection",                         population_type[1], population_type[2], 
                             "Micturition control",                       population_type[1], population_type[2],
                             "Volume depletion",                          population_type[1], population_type[2],
                             "Urinary frequency",                         population_type[1], population_type[2],
                             "Falls",                                     population_type[1], population_type[2],
                             "Amputation",                                population_type[1], population_type[2],   
                             "DKA",                                       population_type[1], population_type[2])

n_events_DPP4_plot      <- c(NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "GI"], 
                             NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "MC"],
                             NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "VD"],
                             NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "UF"],
                             NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "falls"],
                             NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "amputation"],   
                             NA, results_n_events_DPP4$n_event[results_n_events_DPP4$outcome == "dka"])
  
  
n_events_SGLT2_plot     <- c(NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "GI"], 
                             NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "MC"],
                             NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "VD"],
                             NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "UF"],
                             NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "falls"],
                             NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "amputation"],   
                             NA, results_n_events_SGLT2$n_event[results_n_events_SGLT2$outcome == "dka"]) 


results_incidence_rates_DPP4_plot     <- round(c(NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "GI"], 
                                                 NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "MC"],
                                                 NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "VD"],
                                                 NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "UF"],
                                                 NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "falls"],
                                                 NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "amputation"],   
                                                 NA, results_incidence_rates_DPP4$incidence_rate[results_incidence_rates_DPP4$outcome == "dka"]),2) 

results_incidence_rates_SGLT2_plot    <- round(c(NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "GI"], 
                                                 NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "MC"],
                                                 NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "VD"],
                                                 NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "UF"],
                                                 NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "falls"],
                                                 NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "amputation"],   
                                                 NA, results_incidence_rates_SGLT2$incidence_rate[results_incidence_rates_SGLT2$outcome == "dka"]),2) 


IR_lower_SGLT2_plot           <- c(NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "GI"], 
                                   NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "MC"],
                                   NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "VD"],
                                   NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "UF"],
                                   NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "falls"],
                                   NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "amputation"],   
                                   NA, results_incidence_rates_SGLT2$lower_IR_CI[results_incidence_rates_SGLT2$outcome == "dka"])

IR_upper_SGLT2_plot           <- c(NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "GI"], 
                                   NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "MC"],
                                   NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "VD"],
                                   NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "UF"],
                                   NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "falls"],
                                   NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "amputation"],   
                                   NA, results_incidence_rates_SGLT2$upper_IR_CI[results_incidence_rates_SGLT2$outcome == "dka"])


IR_lower_DPP4_plot            <- c(NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "GI"], 
                                   NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "MC"],
                                   NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "VD"],
                                   NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "UF"],
                                   NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "falls"],
                                   NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "amputation"],   
                                   NA, results_incidence_rates_DPP4$lower_IR_CI[results_incidence_rates_DPP4$outcome == "dka"])

IR_upper_DPP4_plot            <- c(NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "GI"], 
                                   NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "MC"],
                                   NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "VD"],
                                   NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "UF"],
                                   NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "falls"],
                                   NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "amputation"],   
                                   NA, results_incidence_rates_DPP4$upper_IR_CI[results_incidence_rates_DPP4$outcome == "dka"])


RR_estimate_plot        <- c(NA, results_RR$causal_estimate[results_RR$outcome == "GI"], 
                             NA, results_RR$causal_estimate[results_RR$outcome == "MC"],
                             NA, results_RR$causal_estimate[results_RR$outcome == "VD"],
                             NA, results_RR$causal_estimate[results_RR$outcome == "UF"],
                             NA, results_RR$causal_estimate[results_RR$outcome == "falls"],
                             NA, results_RR$causal_estimate[results_RR$outcome == "amputation"],   
                             NA, results_RR$causal_estimate[results_RR$outcome == "dka"])

RR_lower_plot           <- c(NA, results_RR$lower[results_RR$outcome == "GI"], 
                             NA, results_RR$lower[results_RR$outcome == "MC"],
                             NA, results_RR$lower[results_RR$outcome == "VD"],
                             NA, results_RR$lower[results_RR$outcome == "UF"],
                             NA, results_RR$lower[results_RR$outcome == "falls"],
                             NA, results_RR$lower[results_RR$outcome == "amputation"],   
                             NA, results_RR$lower[results_RR$outcome == "dka"])

RR_upper_plot           <- c(NA, results_RR$upper[results_RR$outcome == "GI"], 
                             NA, results_RR$upper[results_RR$outcome == "MC"],
                             NA, results_RR$upper[results_RR$outcome == "VD"],
                             NA, results_RR$upper[results_RR$outcome == "UF"],
                             NA, results_RR$upper[results_RR$outcome == "falls"],
                             NA, results_RR$upper[results_RR$outcome == "amputation"],   
                             NA, results_RR$upper[results_RR$outcome == "dka"])


text_col                 <- paste(as.character(round(RR_estimate_plot, 2)),
                                  rep(" (", times = length(RR_estimate_plot)),  
                                  as.character(round(RR_lower_plot, 2)),  
                                  rep(", ", times = length(RR_estimate_plot)),
                                  as.character(round(RR_upper_plot, 2)),  
                                  rep(")", times = length(RR_estimate_plot)), sep = "")  
text_col[text_col == "NA (NA, NA)"] <- "" 


text_col_IR_SGLT2        <- paste(as.character(round(results_incidence_rates_SGLT2_plot, 2)),
                                  rep(" (", times = length(results_incidence_rates_SGLT2_plot)),  
                                  as.character(round(IR_lower_SGLT2_plot, 2)),  
                                  rep(", ", times = length(results_incidence_rates_SGLT2_plot)),
                                  as.character(round(IR_upper_SGLT2_plot, 2)),  
                                  rep(")", times = length(results_incidence_rates_SGLT2_plot)), sep = "")  
text_col_IR_SGLT2[text_col_IR_SGLT2 == "NA (NA, NA)"] <- "" 


text_col_IR_DPP4        <- paste(as.character(round(results_incidence_rates_DPP4_plot, 2)),
                                  rep(" (", times = length(results_incidence_rates_DPP4_plot)),  
                                  as.character(round(IR_lower_DPP4_plot, 2)),  
                                  rep(", ", times = length(results_incidence_rates_DPP4_plot)),
                                  as.character(round(IR_upper_DPP4_plot, 2)),  
                                  rep(")", times = length(results_incidence_rates_DPP4_plot)), sep = "")  
text_col_IR_DPP4[text_col_IR_DPP4 == "NA (NA, NA)"] <- "" 


data_plot           <- data.frame(cbind(outcome_names, n_events_DPP4_plot, n_events_SGLT2_plot, 
                                        results_incidence_rates_DPP4_plot, text_col_IR_DPP4, 
                                        results_incidence_rates_SGLT2_plot, text_col_IR_SGLT2, 
                                        RR_estimate_plot, RR_lower_plot, RR_upper_plot, text_col))
colnames(data_plot) <- c("outcome_names", "n_events_DPP4", "n_events_SGLT2",  "incidence_rate_DPP4", "incidence_rate_DPP4_text",
                         "incidence_rate_SGLT2", "incidence_rate_SGLT2_text", "mean", "lower", "upper", "text_col")

data_plot$mean      <- as.numeric(data_plot$mean)
data_plot$lower     <- as.numeric(data_plot$lower)
data_plot$upper     <- as.numeric(data_plot$upper)
data_plot$outcome_names[data_plot$outcome_names == "not_elderly"] <- "   <70 years"
data_plot$outcome_names[data_plot$outcome_names == "elderly"]     <- "   >70 years"


#
# Forest plot, part A for relative risk ----------------------------------------
#

Cairo(file = paste0(result_path,"/forestplot_IV_binary_",year_type,"_c", censoring_type,"_",stratification_type,"_stratification_RR_mainresults.png" ), 
      type = "png",
      units = "in", 
      width = 45,#10, 20
      height = 30, #8, 16
      pointsize = 12, 
      dpi = 72)

# c(50,25) looks good
# c(50,35) looks good
data_plot|>
forestplot(labeltext = c(outcome_names, n_events_DPP4, n_events_SGLT2, incidence_rate_DPP4, incidence_rate_SGLT2, text_col),
           xlog = T,
           ci.vertices = TRUE,
           ci.vertices.height = 0.3, 
           boxsize = 0.4,
           zero = 1,
           lwd.zero = 3,
           col = fpColors(box = c("blue"), lines = c("black"), zero = c("black")),
           fn.ci_norm = c("fpDrawCircleCI"), 
           xlab = "Relative risk",
           txt_gp = fpTxtGp(cex = 4, xlab = gpar(cex = 4), ticks = gpar(cex = 4)),
           lty.ci = c(1),
           new_page = TRUE)|>
  fp_add_header(outcome_names = c("Adverse effects", ""), n_events_DPP4 = c("n event", "DPP4i"), n_events_SGLT2 = c("n event", "SGLT2i"), 
                incidence_rate_DPP4 = c("IR", "DPP4i"), incidence_rate_SGLT2 = c("IR", "SGLT2i"), text_col = c("RR", "")) |>
  fp_add_lines(h_3 = gpar(lty = 1), h_6 = gpar(lty = 2), h_9 = gpar(lty = 2), h_12 = gpar(lty = 2),
               h_15 = gpar(lty = 2), h_18 = gpar(lty = 2), h_21 = gpar(lty = 2))|> 
  fp_decorate_graph(graph.pos = 6)


dev.off()



#
# Plot again for forest plot part ----------------------------------------------
#




Cairo(file = paste0(result_path,"/forestplot_IV_binary_",year_type,"_c", censoring_type,"_",stratification_type,"_stratification_RR_mainresults_forestplotpart.png" ), 
      type = "png",
      units = "in", 
      width = 45,#10, 20
      height = 30, #8, 16
      pointsize = 12, 
      dpi = 72)

# c(50,25) looks good
# c(50,35) looks good
data_plot|>
  forestplot(labeltext = c(outcome_names,  incidence_rate_DPP4, incidence_rate_SGLT2, text_col),
             xlog = T,
             ci.vertices = TRUE,
             ci.vertices.height = 0.3, 
             boxsize = 0.4,
             zero = 1,
             lwd.zero = 3,
             col = fpColors(box = c("blue"), lines = c("black"), zero = c("black")),
             fn.ci_norm = c("fpDrawCircleCI"), 
             xlab = "Relative risk",
             txt_gp = fpTxtGp(cex = 4, xlab = gpar(cex = 4), ticks = gpar(cex = 4)),
             lty.ci = c(1),
             new_page = TRUE)|>
  fp_add_header(outcome_names = c("Adverse effects", ""),
                incidence_rate_DPP4 = c("IR", "DPP4i"), incidence_rate_SGLT2 = c("IR", "SGLT2i"), text_col = c("RR", "")) |>
  fp_add_lines(h_3 = gpar(lty = 1), h_6 = gpar(lty = 2), h_9 = gpar(lty = 2), h_12 = gpar(lty = 2),
               h_15 = gpar(lty = 2), h_18 = gpar(lty = 2), h_21 = gpar(lty = 2))|> 
  fp_decorate_graph(graph.pos = 4)


dev.off()

























