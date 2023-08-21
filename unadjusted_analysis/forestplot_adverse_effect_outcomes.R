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
# please set paths here

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
# set paths to load results

# n events
load(paste0("/results_n_events_",year_type,"_c",censoring_type,"_",stratification_type,"_stratification.Rdata"))

results_n_events <- results_n_events[results_n_events$outcome%in%c("GI", "MC", "VD", "UF", "falls", "amputation", "dka"), ]

# incidence rates
load(paste0("/results_incidence_rates_",year_type,"_c",censoring_type,"_",stratification_type,"_stratification.Rdata"))

results_incidence_rates <- results_incidence_rates[results_incidence_rates$outcome%in%c("GI", "MC", "VD", "UF", "falls", "amputation", "dka"), ]

# absolute risk results

load(paste0("/results_AR_",year_type,"_c",censoring_type,"_",stratification_type,"_stratification.Rdata"))

results_AR <- results_AR[results_AR$outcome%in%c("GI", "MC", "VD", "UF", "falls", "amputation", "dka"), ]
results_AR[ ,c("AR", "lower", "upper")] <- (results_AR[ ,c("AR", "lower", "upper")])*100


#
# Prepare data frame for absolute risk forest plot -----------------------------
#


outcome_names           <- c("Genital infection",                         population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i", 
                             "Micturition control",                       population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i",
                             "Volume depletion/ dehydration",             population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i",
                             "Urinary frequency",                         population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i",
                             "Falls",                                     population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i",
                             "Amputation",                                population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i",   
                             "DKA",                                       population_type[1], "      DPP4i", "      SGLT2i", population_type[2], "      DPP4i", "      SGLT2i")

n_events_plots          <- c(NA, NA, results_n_events$n_event[results_n_events$outcome == "GI"][1:2],         NA, results_n_events$n_event[results_n_events$outcome == "GI"][3:4],
                             NA, NA, results_n_events$n_event[results_n_events$outcome == "MC"][1:2],         NA, results_n_events$n_event[results_n_events$outcome == "MC"][3:4],
                             NA, NA, results_n_events$n_event[results_n_events$outcome == "VD"][1:2],         NA, results_n_events$n_event[results_n_events$outcome == "VD"][3:4],
                             NA, NA, results_n_events$n_event[results_n_events$outcome == "UF"][1:2],         NA, results_n_events$n_event[results_n_events$outcome == "UF"][3:4],
                             NA, NA, results_n_events$n_event[results_n_events$outcome == "falls"][1:2],      NA, results_n_events$n_event[results_n_events$outcome == "falls"][3:4],
                             NA, NA, results_n_events$n_event[results_n_events$outcome == "amputation"][1:2], NA, results_n_events$n_event[results_n_events$outcome == "amputation"][3:4],
                             NA, NA, results_n_events$n_event[results_n_events$outcome == "dka"][1:2],        NA, results_n_events$n_event[results_n_events$outcome == "dka"][3:4])


incidence_rate_plot     <- round(c(NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "GI"][1:2],         NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "GI"][3:4],
                                   NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "MC"][1:2],         NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "MC"][3:4],
                                   NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "VD"][1:2],         NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "VD"][3:4],
                                   NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "UF"][1:2],         NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "UF"][3:4],
                                   NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "falls"][1:2],      NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "falls"][3:4],
                                   NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "amputation"][1:2], NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "amputation"][3:4],
                                   NA, NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "dka"][1:2],        NA, results_incidence_rates$incidence_rate[results_incidence_rates$outcome == "dka"][3:4]),2)


AR_estimate_plot        <- c(NA, NA, results_AR$AR[results_AR$outcome == "GI"][1:2],         NA, results_AR$AR[results_AR$outcome == "GI"][3:4],
                             NA, NA, results_AR$AR[results_AR$outcome == "MC"][1:2],         NA, results_AR$AR[results_AR$outcome == "MC"][3:4],
                             NA, NA, results_AR$AR[results_AR$outcome == "VD"][1:2],         NA, results_AR$AR[results_AR$outcome == "VD"][3:4],
                             NA, NA, results_AR$AR[results_AR$outcome == "UF"][1:2],         NA, results_AR$AR[results_AR$outcome == "UF"][3:4],
                             NA, NA, results_AR$AR[results_AR$outcome == "falls"][1:2],      NA, results_AR$AR[results_AR$outcome == "falls"][3:4],
                             NA, NA, results_AR$AR[results_AR$outcome == "amputation"][1:2], NA, results_AR$AR[results_AR$outcome == "amputation"][3:4],
                             NA, NA, results_AR$AR[results_AR$outcome == "dka"][1:2],        NA, results_AR$AR[results_AR$outcome == "dka"][3:4])


AR_lower_plot           <- c(NA, NA, results_AR$lower[results_AR$outcome == "GI"][1:2],         NA, results_AR$lower[results_AR$outcome == "GI"][3:4],
                             NA, NA, results_AR$lower[results_AR$outcome == "MC"][1:2],         NA, results_AR$lower[results_AR$outcome == "MC"][3:4],
                             NA, NA, results_AR$lower[results_AR$outcome == "VD"][1:2],         NA, results_AR$lower[results_AR$outcome == "VD"][3:4],
                             NA, NA, results_AR$lower[results_AR$outcome == "UF"][1:2],         NA, results_AR$lower[results_AR$outcome == "UF"][3:4],
                             NA, NA, results_AR$lower[results_AR$outcome == "falls"][1:2],      NA, results_AR$lower[results_AR$outcome == "falls"][3:4],
                             NA, NA, results_AR$lower[results_AR$outcome == "amputation"][1:2], NA, results_AR$lower[results_AR$outcome == "amputation"][3:4],
                             NA, NA, results_AR$lower[results_AR$outcome == "dka"][1:2],        NA, results_AR$lower[results_AR$outcome == "dka"][3:4])


AR_upper_plot           <- c(NA, NA, results_AR$upper[results_AR$outcome == "GI"][1:2],         NA, results_AR$upper[results_AR$outcome == "GI"][3:4],
                             NA, NA, results_AR$upper[results_AR$outcome == "MC"][1:2],         NA, results_AR$upper[results_AR$outcome == "MC"][3:4],
                             NA, NA, results_AR$upper[results_AR$outcome == "VD"][1:2],         NA, results_AR$upper[results_AR$outcome == "VD"][3:4],
                             NA, NA, results_AR$upper[results_AR$outcome == "UF"][1:2],         NA, results_AR$upper[results_AR$outcome == "UF"][3:4],
                             NA, NA, results_AR$upper[results_AR$outcome == "falls"][1:2],      NA, results_AR$upper[results_AR$outcome == "falls"][3:4],
                             NA, NA, results_AR$upper[results_AR$outcome == "amputation"][1:2], NA, results_AR$upper[results_AR$outcome == "amputation"][3:4],
                             NA, NA, results_AR$upper[results_AR$outcome == "dka"][1:2],        NA, results_AR$upper[results_AR$outcome == "dka"][3:4])

text_col                 <- paste(as.character(round(AR_estimate_plot, 2)),
                                  rep(" (", times = length(AR_estimate_plot)),  
                                  as.character(round(AR_lower_plot, 2)),  
                                  rep(", ", times = length(AR_estimate_plot)),
                                  as.character(round(AR_upper_plot, 2)),  
                                  rep(")", times = length(AR_estimate_plot)), sep = "")  
text_col[text_col == "NA (NA, NA)"] <- ""  


data_plot           <- data.frame(cbind(outcome_names, n_events_plots, incidence_rate_plot, AR_estimate_plot, AR_lower_plot, AR_upper_plot, text_col))
colnames(data_plot) <- c("outcome_names", "n_events", "incidence_rate", "mean", "lower", "upper", "text_col")

data_plot$mean      <- as.numeric(data_plot$mean)
data_plot$lower     <- as.numeric(data_plot$lower)
data_plot$upper     <- as.numeric(data_plot$upper)
data_plot$outcome_names[data_plot$outcome_names == "not_elderly"] <- "   <70 years"
data_plot$outcome_names[data_plot$outcome_names == "elderly"]     <- "   >70 years"


#
# Forest plot absolute risk ----------------------------------------------------
#

Cairo(file = paste0(result_path,"/forestplot_IV_binary_",year_type,"_c", censoring_type,"_",stratification_type,"_stratification_AR_mainresults.png" ), 
      type = "png",
      units = "in", 
      width = 60,#10, 20 
      height = 45, #8, 16
      pointsize = 12, 
      dpi = 72)

data_plot|>
forestplot(labeltext = c(outcome_names, n_events, incidence_rate, text_col),
           xlog = F,
           ci.vertices = TRUE,
           ci.vertices.height = 0.3, 
           boxsize = 0.4,
           zero = 0,
           lwd.zero = 3,
           col = fpColors(box = c("blue"), lines = c("black"), zero = "black"),
           fn.ci_norm = c("fpDrawCircleCI"), 
           xlab = "Absolute risk (%)",
           txt_gp = fpTxtGp(cex = 4.5, xlab = gpar(cex = 4.5), ticks = gpar(cex = 4.5)),
           lty.ci = c(1),
           new_page = TRUE)|>
  fp_add_header(outcome_names = "Adverse effects", n_events = "n event",incidence_rate = "IR", text_col = "AR (CI 95%)") |>
  fp_add_lines(h_2 = gpar(lty = 1), h_9 = gpar(lty = 2), h_16 = gpar(lty = 2), h_23 = gpar(lty = 2),
               h_30 = gpar(lty = 2), h_37 = gpar(lty = 2), h_44 = gpar(lty = 2))|> 
  fp_decorate_graph(graph.pos = 4)


dev.off()









