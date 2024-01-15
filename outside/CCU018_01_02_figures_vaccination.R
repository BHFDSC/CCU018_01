################################################################################
## TITLE: Prepare figures for vaccination analysis 
##
## By : Elena Raffetti
##
################################################################################



################################################################################
################################################################################
#                                MAIN ANALYSIS                                #
################################################################################
################################################################################

rm(list = ls())

#install.packages("readr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("meta")
#install.packages("gridExtra")
#install.packages("grid")
#install.packages("ggplot2")
#install.packages("lattice")
library(dplyr)
library(tidyr)
library(readr)
library(meta)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)  

# Read file --------------------------------------------------------------------
df1 <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_full_period1.csv")
df2 <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_full_period2.csv")

df_eng <- rbind(df1, df2, fill=TRUE)
df_eng$nation <- "England"

# Read file --------------------------------------------------------------------
df_sail <- read_delim("C:/Users/elera574/Desktop/results_CCU018/SAIL/out_vaccination_to_export.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
df_sail$nation <- "Wales"

df <- rbind(df_eng,df_sail, fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$sens=="Total",]
df <- df[df$adj=="full_adjusted"| df$full=="full_adjusted" ,]
df <- df[df$subgroup=="" | is.na(df$subgroup),]
df <- df[df$exp=="E2" | df$exp=="E1"  | df$exp=="vacc_covid",]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","<=14 days",df$exp)
df$exp <- ifelse(df$exp=="E2",">14 days",df$exp)
df$exp <- ifelse(df$exp=="vacc_covid","Total",df$exp)

# Perform meta-analysis for each event/stratum combination ---------------------
needs_meta <- unique(df[,c("event","exp")])

for (i in 1:nrow(needs_meta)) {
  
  tmp <- df[df$exp==needs_meta$exp[i] &
           df$event==needs_meta$event[i] ,]
  tmp
  
  meta <- unique(tmp[,c("event","exp")])
  
  meta$estimate <- NA
  meta$conf.low <- NA
  meta$conf.high <- NA
  meta$p.value <- NA
  meta$stad.error <- NA
  meta$robust.se <- NA
  meta$statistic <- NA
  
  tmp_meta <- meta::metagen(log(tmp$estimate),tmp$stad.error, sm = "HR")
  meta$estimate <- exp(tmp_meta$TE.fixed)
  meta$conf.low <- exp(tmp_meta$lower.fixed)
  meta$conf.high <- exp(tmp_meta$upper.fixed)
  meta$p.value <- tmp_meta$pval.fixed
  meta$stad.error <- tmp_meta$seTE.fixed
  
  meta$term
  
  for (j in unique(meta$term)) {
    tmp2 <- tmp[tmp$term==j,]
    tmp_meta <- meta::metagen(log(tmp2$estimate),tmp2$stad.error, sm = "HR")
    meta[meta$term==j,]$estimate <- exp(tmp_meta$TE.fixed)
    meta[meta$term==j,]$conf.low <- exp(tmp_meta$lower.fixed)
    meta[meta$term==j,]$conf.high <- exp(tmp_meta$upper.fixed)
    meta[meta$term==j,]$p.value <- tmp_meta$pval.fixed
    meta[meta$term==j,]$stad.error <- tmp_meta$seTE.fixed
  }
  df <- plyr::rbind.fill(df, meta)
  
}

df$nation[is.na(df$nation)] <- "meta"
df <- df[df$nation=="meta",]

# Specify time -----------------------------------------------------------------
term_to_time <- data.frame(exp = c("Total","<=14 days", ">14 days"),
                           time = c(4,3,2))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp==">14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="<=14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(">14 days",
                                  "<=14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Ranme event names ------------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event) 
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event) 
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestational age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small for gestational age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

# Plot and save ----------------------------------------------------------------
main_analysis_1st <-ggplot(data = df,
                        mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            vjust=-0.5, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.25,5.5), breaks = c(0.5,1,2, 4), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("COVID-19 vaccine dose 1 during pregnancy") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

ggsave(file="C:/Users/elera574/Desktop/results_CCU018/1st_vaccination_main_results.png", main_analysis_1st,  height = 100, width = 320, unit = "mm", dpi = 600, scale = 1)


# Read files -------------------------------------------------------------------
df <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/2nd_vaccination_out_HRs.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$sens=="Total",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$exp=="E2" | df$exp=="E1"  | df$exp=="vacc_covid",]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","<=14 days",df$exp)
df$exp <- ifelse(df$exp=="E2",">14 days",df$exp)
df$exp <- ifelse(df$exp=="vacc_covid","Total",df$exp)

term_to_time <- data.frame(exp = c("Total","<=14 days", ">14 days"),
                           time = c(4,3,2))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp==">14 days","#A52A2A",df$colour)
df$colour <- ifelse(df$exp=="<=14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour)

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(">14 days",
                                  "<=14 days",
                                  "Total"
))

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names -----------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event)
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event)
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event)
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event)
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestational age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small for gestational age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

# Plot and save ----------------------------------------------------------------
main_analysis_2nd <-ggplot(data = df,
                           mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            vjust=-0.5, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.25,5.5), breaks = c(0.5,1,2, 4), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c("","",""))  +
  ggtitle("COVID-19 vaccine dose 2 during pregnancy") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        #                  legend.title = element_blank(),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

vaccination_main <- arrangeGrob(main_analysis_1st, main_analysis_2nd, ncol=2)

#Figure 3
ggsave(file="C:/Users/elera574/Desktop/results_CCU018/vaccination_main.png", vaccination_main,  height = 220, width = 250, unit = "mm", dpi = 600, scale = 1.3)


################################################################################
################################################################################
#                             STRATIFICATION                                   #
################################################################################
################################################################################

# Read files -------------------------------------------------------------------
df1 <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_full_period1.csv")
df2 <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_full_period2.csv")

df <- rbind(df1, df2,   fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$sens=="total",]
df <- df[df$model=="week2" | df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[!df$subgroup=="",]
df <- df[df$exp=="vacc_covid",]
View(df)

# Tidy variables ---------------------------------------------------------------
df$subgroup[is.na(df$subgroup)] <- "All"

df$stratum <- ""
df$stratum <- ifelse(grepl("hx_of_preg", df$subgroup),"Pregnancy history/No pregnancy history",df$stratum)  
df$stratum <- ifelse(grepl("Other", df$subgroup),"Ethnic group",df$stratum)
df$stratum <- ifelse(grepl("Unknown", df$subgroup),"Ethnic group",df$stratum)
df$stratum <- ifelse(grepl("White", df$subgroup),"Ethnic group",df$stratum)
df$stratum <- ifelse(grepl("Asian", df$subgroup),"Ethnic group",df$stratum)
df$stratum <- ifelse(grepl("Black", df$subgroup),"Ethnic group",df$stratum)
df$stratum <- ifelse(grepl("Mixed", df$subgroup),"Ethnic group",df$stratum)
df$stratum <- ifelse(grepl("dep", df$subgroup),"High deprivation/Low deprivation",df$stratum)  
df$stratum <- ifelse(grepl("<30", df$subgroup),"Age group",df$stratum)  
df$stratum <- ifelse(grepl("30-39", df$subgroup),"Age group",df$stratum)
df$stratum <- ifelse(grepl(">=40", df$subgroup),"Age group",df$stratum) 
df$stratum <- ifelse(grepl("all", df$subgroup),"All",df$stratum)
df$stratum <- ifelse(grepl("hx_of_health_cond", df$subgroup),"Comorbidities history/No comorbidities history",df$stratum) 
df$stratum <- ifelse(df$subgroup=="no", "COVID infection history/No COVID infection history",df$stratum)
df$stratum <- ifelse(df$subgroup=="yes", "COVID infection history/No COVID infection history",df$stratum)

# Select ethnicity categorization with 6 levels --------------------------------
df$categorization_scheme <- NA
current_categorization <- 1

for(i in 2:nrow(df)) {
  if((df$subgroup[i-1] == 'White' & df$subgroup[i] == 'Asian' & df$stratum[i] == "Ethnic group")| (df$subgroup[i-1] == 'hx_of_health_cond' & df$subgroup[i] == 'Asian' & df$stratum[i] == "Ethnic group") ) {
    current_categorization <- 2
  }
  if(df$subgroup[i-1] == 'hx_of_preg' & df$subgroup[i] == 'Other' & df$stratum[i] == "Ethnic group") {
    current_categorization <- 1
  }
  df$categorization_scheme[i] <- current_categorization
}

df <- df[!(df$categorization_scheme==1 & df$stratum== "Ethnic group"),]
df <- select(df, -categorization_scheme)

# Tidy variables ---------------------------------------------------------------
df$stratification <- "All"
df$stratification <- ifelse(df$stratum=="Ethnic group",
                            "Ethnic group",df$stratification)
df$stratification <- ifelse(df$stratum=="High deprivation/Low deprivation",
                            "Deprivation",df$stratification)
df$stratification <- ifelse(df$stratum=="Pregnancy history/No pregnancy history",
                            "Pregnancy history",df$stratification)
df$stratification <- ifelse(df$stratum=="Age group",
                            "Age group",df$stratification)
df$stratification <- ifelse(df$stratum=="All",
                            "All",df$stratification)
df$stratification <- ifelse(df$stratum=="Comorbidities history/No comorbidities history",
                            "Comorbidity history",df$stratification)
df$stratification <- ifelse(df$stratum=="COVID infection history/No COVID infection history",
                            "COVID-19 infection history",df$stratification)

df$stratum <- ifelse(df$stratum=="High deprivation/Low deprivation" & df$subgroup=="high_dep",
                     "High deprivation",df$stratum)
df$stratum <- ifelse(df$stratum=="High deprivation/Low deprivation" & df$subgroup=="low_dep",
                     "Low deprivation",df$stratum)
df$stratum <- ifelse(df$stratum=="Pregnancy history/No pregnancy history" & df$subgroup=="hx_of_preg",
                     "History of pregnancy",df$stratum)
df$stratum <- ifelse(df$stratum=="Pregnancy history/No pregnancy history" & df$subgroup=="no_hx_of_preg",
                     "No history of pregnancy",df$stratum)
df$stratum <- ifelse(df$stratum=="Age group" & df$subgroup=="<30",
                     "Age<30",df$stratum)
df$stratum <- ifelse(df$stratum=="Age group" & df$subgroup=="30-39",
                     "Age 30-39",df$stratum)
df$stratum <- ifelse(df$stratum=="Age group" & df$subgroup==">=40",
                     "Age>=40",df$stratum)
df$stratum <- ifelse(df$stratum=="Ethnic group" & df$subgroup=="White",
                     "White",df$stratum)
df$stratum <- ifelse(df$stratum=="Ethnic group" & df$subgroup=="Unknown",
                     "Unknown",df$stratum)
df$stratum <- ifelse(df$stratum=="Ethnic group" & df$subgroup=="Other",
                     "Ethnic Minorities",df$stratum)
df$stratum <- ifelse(df$stratum=="Ethnic group" & df$subgroup=="Mixed",
                     "Mixed",df$stratum)
df$stratum <- ifelse(df$stratum=="Ethnic group" & df$subgroup=="Asian",
                     "Asian or Asian British",df$stratum)
df$stratum <- ifelse(df$stratum=="Ethnic group" & df$subgroup=="Black",
                     "Black or Black British",df$stratum)
df$stratum <- ifelse(df$stratum=="Comorbidities history/No comorbidities history" & df$subgroup=="hx_of_health_cond",
                     "History of health conditions",df$stratum)
df$stratum <- ifelse(df$stratum=="Comorbidities history/No comorbidities history" & df$subgroup=="no_hx_of_health_cond",
                     "No history of health conditions",df$stratum)
df$stratum <- ifelse(df$stratum=="COVID infection history/No COVID infection history" & df$subgroup=="yes",
                     "History of COVID-19",df$stratum)
df$stratum <- ifelse(df$stratum=="COVID infection history/No COVID infection history" & df$subgroup=="no",
                     "No history of COVID-19",df$stratum)

df$exp <- ifelse(df$exp=="vacc_covid","Total",df$exp)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="Total","#8B0000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(
                                  # ">14 days",
                                  # "=<14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#8B0000"))

# Rename event names -----------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event) 
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event) 
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small gest age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small gest age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

df$stratification <- factor(df$stratification, levels=c( "Deprivation",
                                                         "Ethnic group",
                                                         "Age group",
                                                         "Pregnancy history",
                                                         "Comorbidity history",
                                                         "COVID-19 infection history"
))

df$stratum <- factor(df$stratum, levels=c("High deprivation",
                                          "Low deprivation",
                                          "Unknown",
                                          "Ethnic Minorities",
                                          "Mixed",
                                          "White",
                                          "Asian or Asian British",
                                          "Black or Black British",
                                          "Age>=40",
                                          "Age 30-39",
                                          "Age<30",
                                          "History of pregnancy",
                                          "No history of pregnancy",
                                          "History of health conditions",
                                          "No history of health conditions",
                                          "History of COVID-19",
                                          "No history of COVID-19")) 

# Plot and save ----------------------------------------------------------------
q1 <-ggplot(data = df[df$event=="Gestational Diabetes",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Gestational Diabetes") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

q2 <-ggplot(data = df[df$event=="Gestational Hypertension",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Gestational Hypertension") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

q3 <-ggplot(data = df[df$event=="Preeclampsia",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Preeclampsia") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

q4 <-ggplot(data = df[df$event=="Thrombotic venous event",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Thrombotic venous event") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

grid.arrange(q1, q2, q3, q4, ncol=2)

#Figure S15
g1 <- arrangeGrob(q1, q2, q3, q4, ncol=2)
ggsave(file="C:/Users/elera574/Desktop/results_CCU018/1st_vaccination_stratification_dur_pregn.png", g1,  height = 270, width = 250, unit = "mm", dpi = 600, scale = 1.3)


# Plot and save ----------------------------------------------------------------
q5 <-ggplot(data = df[df$event=="Preterm",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Preterm") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

q6 <-ggplot(data = df[df$event=="Very preterm",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Very preterm") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

q7 <-ggplot(data = df[df$event=="Small gest age",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  ggtitle("Small for gestational age") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

q8 <-ggplot(data = df[df$event=="Stillbirth",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.25,10), breaks = c(0.5,1,2, 4,8), trans = "log")  +
  scale_fill_manual(values = levels(df$colour)[1]) +
  scale_color_manual(values = levels(df$colour)[1]) +
  labs(x ="", y = "HR and 95% CI") +
  ggtitle("Stillbirth") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

grid.arrange(q5, q6, q7, q8, ncol=2)

g2 <- arrangeGrob(q5, q6, q7, q8, ncol=2)
#Figure S16
ggsave(file="C:/Users/elera574/Desktop/results_CCU018/1st_vaccination_stratification_birth.png", g2,  height = 220, width = 250, unit = "mm", dpi = 600, scale = 1.3)


################################################################################
################################################################################
#             SENSITIVITY  ANALYSES                                            #
################################################################################
################################################################################

# Read files --------------------------------------------------------------------
df1 <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_full_period1.csv")
df2 <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_full_period2.csv")

df <- rbind(df1, df2,  fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$model=="week2" | df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="E2" | df$exp=="E1"  | df$exp=="vacc_covid",]
df <- df[!( df$event=="STILLBIRT"   &  df$exp=="E1"),]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","=<14 days",df$exp)
df$exp <- ifelse(df$exp=="E2",">14 days",df$exp)
df$exp <- ifelse(df$exp=="vacc_covid","Total",df$exp)

term_to_time <- data.frame(exp = c("Total","=<14 days", ">14 days"),
                           time = c(4,3,2))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp==">14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="=<14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering ------------------------------------------------
df$exp <- factor(df$exp, levels=c(">14 days",
                                  "=<14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names -----------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event) 
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event) 
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestation age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small for gestation age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

# Plot and save ----------------------------------------------------------------
sens3 <-ggplot(data = df[df$sens=="sens_covid",],
               mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("Censored at \nCOVID-19 diagnosis") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        #                  legend.title = element_blank(),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

sens2 <-ggplot(data = df[df$sens=="sens",],
               mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("Known estimated \ngestational age") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

sens3 <-ggplot(data = df[df$sens=="sens_no_early_preg",],
               mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("Early vaccine \ndose 1 excluded") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

grid.arrange(sens1, sens2, sens3, ncol=3)

#Figure S18
sens <- arrangeGrob(sens1, sens2, sens3, ncol=3) #generates g
ggsave(file="C:/Users/elera574/Desktop/results_CCU018/1st_vaccination_sens.png", sens,  height = 220, width = 250, unit = "mm", dpi = 600, scale = 1)


################################################################################
################################################################################
#                             TRIMESTER                                        #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df<- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/1st_vaccination_out_HRs_trimester.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="vacc_covid",]
df <- df[!(df$event=="STILLBIRTH"   &  df$tri=="trimester1"),]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="vacc_covid","Total",df$exp)

df$tri <- ifelse(df$tri=="trimester1","1st trimester",df$tri)
df$tri <- ifelse(df$tri=="trimester2","2nd trimester",df$tri)
df$tri <- ifelse(df$tri=="trimester3","3rd trimester",df$tri)

term_to_time <- data.frame(tri = c("1st trimester", "2nd trimester", "3rd trimester"),
                           time = c(5,4,3))

df <- merge(df, term_to_time, by = c("tri"), all.x = TRUE)

df$colour <- ""
df$colour <- ifelse(df$tri=="1st trimester","#FF9999",df$colour)
df$colour <- ifelse(df$tri=="2nd trimester","#FF4136",df$colour)
df$colour <- ifelse(df$tri=="3rd trimester","#B22222",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$tri <- factor(df$tri, levels=c(
  "1st trimester",
  "2nd trimester",
  "3rd trimester"))

df$colour <- factor(df$colour, levels=c(
  "#FF9999",
  "#FF4136",
  "#B22222" ))

# Rename event names -----------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event) 
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event) 
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestation age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small for gestation age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

# Save and plot ----------------------------------------------------------------
trimester <-ggplot(data = df[df$sens=="total",],
                         mapping = aes(x = time, y = estimate, color = tri, shape=exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.4))  +
  scale_y_continuous(lim = c(0.25,5.5), breaks = c(0.5,1,2, 4), trans = "log")  +
  scale_x_continuous(lim = c(2,6), breaks = c(3, 4, 5), label = c("3rd trimester","2nd trimester", "1st trimester"))  +
  ggtitle(" ") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        #                  legend.title = element_blank(),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

#Figure S17
ggsave(file="C:/Users/elera574/Desktop/results_CCU018/1st_vaccination_trimester.png", trimester,  height = 220, width = 250, unit = "mm", dpi = 600, scale = 1)


################################################################################
################################################################################
#             SENSITIVITY  ANALYSIS 2ND VACCINATION                            #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df <- data.table::fread("C:/Users/elera574/Desktop/results_CCU018/SDE/2nd_vaccination_out_HRs.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$adj=="full_adjusted",]
df <- df[df$exp=="E2" | df$exp=="E1"  | df$exp=="vacc_covid",]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","=<14 days",df$exp)
df$exp <- ifelse(df$exp=="E2",">14 days",df$exp)
df$exp <- ifelse(df$exp=="vacc_covid","Total",df$exp)

term_to_time <- data.frame(exp = c("Total","=<14 days", ">14 days"),
                           time = c(4,3,2))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp==">14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="=<14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(">14 days",
                                  "=<14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names -----------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event) 
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event) 
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestation age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small for gestation age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

# Plot and save ----------------------------------------------------------------
sens1 <-ggplot(data = df[df$sens=="sens_covid",],
               mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("Censored at \nCOVID-19 diagnosis") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

sens2 <-ggplot(data = df[df$sens=="sens",],
               mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("Known estimated \ngestational age") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

sens3 <-ggplot(data = df[df$sens=="sens_no_early_preg",],
                        mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c(">14 days","=<14 days","Total"))  +
  ggtitle("Early vaccine \ndose 1 excluded") +
  scale_fill_manual(values = levels(df$colour)[1:3],) +
  scale_color_manual(values = levels(df$colour)[1:3],) +
  labs(x ="", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.position="none",
        strip.background = element_blank(), strip.placement = "outside",
        plot.background = element_rect(fill = "white", colour = "white")) +
  facet_wrap(~event,strip.position="top",nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

grid.arrange(sens1, sens2, sens3, ncol=3)

#Figure S19
sens <- arrangeGrob(sens1, sens2, sens3, ncol=3)
ggsave(file="C:/Users/elera574/Desktop/results_CCU018/2nd_vaccination.png", sens,  height = 220, width = 250, unit = "mm", dpi = 600, scale = 1)