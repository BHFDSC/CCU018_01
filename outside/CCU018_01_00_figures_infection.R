################################################################################
## TITLE: Prepare figures for infection analysis
##
## By : Elena Raffetti
##
################################################################################


################################################################################
################################################################################
#                                MAAIN ANALYSIS                                #
################################################################################
################################################################################

rm(list = ls())

# Install.packages -------------------------------------------------------------
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("meta")
install.packages("gridExtra")
install.packages("grid")
install.packages("ggplot2")
install.packages("lattice")
library(dplyr)
library(tidyr)
library(readr)
library(meta)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)  

dev.off()

# Read file --------------------------------------------------------------------
df_full1 <- data.table::fread("./SDE/out_HRs_full_period1.csv")
df_full2 <- data.table::fread("./SDE/out_HRs_full_period2.csv")
df_full <- rbind(df_full1, df_full2, fill=TRUE)

# Read file --------------------------------------------------------------------
df_sens1 <- data.table::fread("./SDE/out_HRs_sens1.csv")
df_sens2 <- data.table::fread("./SDE/out_HRs_sens2.csv")
df_sens3 <- data.table::fread("./SDE/out_HRs_sens3.csv")
df_sens4 <- data.table::fread("./SDE/out_HRs_sens4.csv")
df_sens <- rbind(df_sens1, df_sens2, df_sens3, df_sens4, fill=TRUE)

df_eng <- rbind(df_full, df_sens, fill=TRUE)
df_eng$nation <- "England"

# Read file --------------------------------------------------------------------
df_sail <- read_delim("./SAIL/out_dur_HRs_full_period_to_export.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
df_sail$nation <- "Wales"
df_sail$sens <- df_sail$analysis
df_sail$sens <- ifelse(df_sail$sens=="vaccination","post_vaccination",df_sail$sens)
df <- rbind(df_eng,df_sail, fill=TRUE)

# Criteria ---------------------------------------------------------------------  
df <- df[df$model=="week2" | df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="covid_inf"  | df$exp=="E1" | df$exp=="E2",]
df <- df[is.na(df$sens=="") | df$sens=="pre_vaccination" | df$sens=="post_vaccination",]

# Tidy variables ---------------------------------------------------------------
df$sens <- ifelse(is.na(df$sens),"Total",df$sens)

df$exp <- ifelse(df$exp=="E1","\u2264 14 days",df$exp)
df$exp <- ifelse(df$exp=="E2","> 14 days",df$exp)
df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

needs_meta <- unique(df[,c("event","exp","sens")])

# Perform meta-analysis for each event/stratum combination ---------------------
for (i in 1:nrow(needs_meta)) {
  
  tmp <- df[df$event==needs_meta$event[i] &
              df$sens==needs_meta$sens[i] &
              df$exp==needs_meta$exp[i],]
  
  meta <- unique(tmp[,c("event","exp","sens")])
  
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
term_to_time <- data.frame(exp = c("Total","\u2264 14 days", "> 14 days"),
                           time = c(4,3,2))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="> 14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="\u2264 14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c("> 14 days",
                                 "\u2264 14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names -----------------------------------------------------------
df$event <- ifelse(df$event=="GEST_HYPER","Gestational Hypertension",df$event) 
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestation age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event) 

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
main_analysis1 <-ggplot(data = df[df$sens=="Total",],
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
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c("> 14 days","\u2264 14 days","Total"))  +
  ggtitle("Overall period") +
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

main_analysis2 <-ggplot(data = df[df$sens=="pre_vaccination",],
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
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c("> 14 days","\u2264 14 days","Total"))  +
  ggtitle("Pre-vaccination era") +
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

main_analysis3 <-ggplot(data = df[df$sens=="post_vaccination",],
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
  scale_y_continuous(lim = c(0.1,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(2,5), breaks = c(2,3,4), label = c("> 14 days","\u2264 14 days","Total"))  +
  ggtitle("Vaccination era") +
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

grid.arrange(main_analysis1, main_analysis2, main_analysis3, ncol=3)

#Figure 1
main_analysis <- arrangeGrob(main_analysis1, main_analysis2, main_analysis3, ncol=3) 
ggsave(file="./main_results.png", main_analysis,  height = 220, width = 250, unit = "mm", dpi = 600, scale = 1)


################################################################################
################################################################################
#                             STRATIFICATION                                   #
#                             OVERALL PERIOD                                   #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df_full1 <- data.table::fread("./SDE/out_HRs_full_period1.csv")
df_full2 <- data.table::fread("./SDE/out_HRs_full_period2.csv")
df <- rbind(df_full1, df_full2, fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[!df$subgroup=="",]
df <- df[df$exp=="covid_inf",]

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
  if((df$subgroup[i-1] == 'White' & df$subgroup[i] == 'Asian')| (df$subgroup[i-1] == 'hx_of_health_cond' & df$subgroup[i] == 'Asian') ) {
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

df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="Total","#8B0000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#8B0000"))

# Rename event names ------------------------------------------------------
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

q1 <-ggplot(data = df[df$event=="Gestational Diabetes",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

#Figure S2
g1 <- arrangeGrob(q1, q2, q3, q4, ncol=2) 
ggsave(file="./stratification_dur_pregn.png", g1,  height = 300, width = 250, unit = "mm", dpi = 600, scale = 1.3)


# Plot and save ----------------------------------------------------------------
q5 <-ggplot(data = df[df$event=="Preterm",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.5))  +
  scale_y_continuous(lim = c(0.4,10), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

#Figure S3
g2 <- arrangeGrob(q5, q6, q7, q8, ncol=2) 
ggsave(file="./stratification_birth.png", g2,  height = 300, width = 250, unit = "mm", dpi = 600, scale = 1.3)

################################################################################
################################################################################
#                             STRATIFICATION                                   #
#                             PRE-VACCINATION                                  #
################################################################################
################################################################################

# Read files -------------------------------------------------------------------
df_sens1 <- data.table::fread("./SDE/out_HRs_sens1.csv")
df_sens2 <- data.table::fread("./SDE/out_HRs_sens2.csv")
df_sens3 <- data.table::fread("./SDE/out_HRs_sens3.csv")
df_sens4 <- data.table::fread("./SDE/out_HRs_sens4.csv")
df <- rbind(df_sens1, df_sens2, df_sens3, df_sens4, fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$sens=="pre_vaccination",]
df <- df[df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[!df$subgroup=="",]
df <- df[df$exp=="covid_inf",]

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
  if((df$subgroup[i-1] == 'White' & df$subgroup[i] == 'Asian')| (df$subgroup[i-1] == 'hx_of_health_cond' & df$subgroup[i] == 'Asian') ) {
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

df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="Total","#8B0000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(
  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#8B0000"))

# Rename event names ------------------------------------------------------
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
qpre1 <-ggplot(data = df[df$event=="Gestational Diabetes",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpre2 <-ggplot(data = df[df$event=="Gestational Hypertension",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpre3 <-ggplot(data = df[df$event=="Preeclampsia",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpre4 <-ggplot(data = df[df$event=="Thrombotic venous event",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

grid.arrange(qpre1, qpre2, qpre3, qpre4, ncol=2)

#Figure S4
str_preg_pre <- arrangeGrob(qpre1, qpre2, qpre3, qpre4, ncol=2) 
ggsave(file="./stratification_dur_preg_pre_vaccination.png", str_preg_pre,  height = 300, width = 250, unit = "mm", dpi = 600, scale = 1.3)


# Plot and save ----------------------------------------------------------------
qpre5 <-ggplot(data = df[df$event=="Preterm",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpre6 <-ggplot(data = df[df$event=="Very preterm",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpre7 <-ggplot(data = df[df$event=="Small gest age",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpre8 <-ggplot(data = df[df$event=="Stillbirth",],
            mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

grid.arrange(qpre5, qpre6, qpre7, qpre8, ncol=2)

#Figure S5
str_birth_pre <- arrangeGrob(qpre5, qpre6, qpre7, qpre8, ncol=2) 
ggsave(file="./stratification_birth_pre_vaccination.png", str_birth_pre,  height = 300, width = 250, unit = "mm", dpi = 600, scale = 1.3)


################################################################################
################################################################################
#                             STRATIFICATION                                   #
#                             VACCINATION ERA                                  #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df_sens1 <- data.table::fread("./SDE/out_HRs_sens1.csv")
df_sens2 <- data.table::fread("./SDE/out_HRs_sens2.csv")
df_sens3 <- data.table::fread("./SDE/out_HRs_sens3.csv")
df_sens4 <- data.table::fread("./SDE/out_HRs_sens4.csv")
df <- rbind(df_sens1, df_sens2, df_sens3, df_sens4, fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$sens=="post_vaccination",]
df <- df[df$adj=="full_adjusted",]
df <- df[!df$subgroup=="",]
df <- df[df$exp=="covid_inf",]

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
  if((df$subgroup[i-1] == 'White' & df$subgroup[i] == 'Asian')| (df$subgroup[i-1] == 'hx_of_health_cond' & df$subgroup[i] == 'Asian') ) {
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

df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="Total","#8B0000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c(
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
qpost1 <-ggplot(data = df[df$event=="Gestational Diabetes",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpost2 <-ggplot(data = df[df$event=="Gestational Hypertension",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpost3 <-ggplot(data = df[df$event=="Preeclampsia",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpost4 <-ggplot(data = df[df$event=="Thrombotic venous event",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

grid.arrange(qpost1, qpost2, qpost3, qpost4, ncol=2)

#Figure S6
str_preg_post <- arrangeGrob(qpost1, qpost2, qpost3, qpost4, ncol=2) 
ggsave(file="./stratification_dur_preg_post_vaccination.png", str_preg_post,  height = 300, width = 250, unit = "mm", dpi = 600, scale = 1.3)

# Plot and save ----------------------------------------------------------------
qpost5 <-ggplot(data = df[df$event=="Preterm",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpost6 <-ggplot(data = df[df$event=="Very preterm",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
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

qpost7 <-ggplot(data = df[df$event=="Small gest age",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
  # scale_x_continuous(lim = c(0,5), breaks = c(2, 3, 4), label = c(" "," ", " "))  +
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

qpost8 <-ggplot(data = df[df$event=="Stillbirth",],
               mapping = aes(x = stratum, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.4,9), breaks = c(0.5,1,2,4,8), trans = "log")  +
  # scale_x_continuous(lim = c(0,5), breaks = c(2, 3, 4), label = c(" "," ", " "))  +
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

grid.arrange(qpost5, qpost6, qpost7, qpost8, ncol=2)

#Figure S7
str_birth_post <- arrangeGrob(qpost5, qpost6, qpost7, qpost8, ncol=2) 
ggsave(file="./stratification_birth_post_vaccination.png", str_birth_post,  height = 300, width = 250, unit = "mm", dpi = 600, scale = 1.3)

################################################################################
################################################################################
#             SENSITIVITY  ANALYSES                                   #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df_sens1 <- data.table::fread("./SDE/out_HRs_sens1.csv")
df_sens2 <- data.table::fread("./SDE/out_HRs_sens2.csv")
df_sens3 <- data.table::fread("./SDE/out_HRs_sens3.csv")
df_sens4 <- data.table::fread("./SDE/out_HRs_sens4.csv")
df <- rbind(df_sens1, df_sens2, df_sens3, df_sens4, fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$model=="week2" | df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="covid_inf"  | df$exp=="E1" | df$exp=="E2",]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","\u2264 14 days",df$exp)
df$exp <- ifelse(df$exp=="E2","> 14 days",df$exp)
df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

# 
term_to_time <- data.frame(exp = c("Total","\u2264 14 days", "> 14 days"),
                           time = c(5,4,3))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="> 14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="\u2264 14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c("> 14 days",
                                  "\u2264 14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names ------------------------------------------------------
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
sens1 <-ggplot(data = df[df$sens=="sens_missing",],
                        mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("> 14 days","\u2264 14 days","Total"))  +
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

sens2 <-ggplot(data = df[df$sens=="sens_pandemic_initiation_before_pandemic",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Estimated pregnancy start \ndate < 11/03/2020") +
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

sens3 <-ggplot(data = df[df$sens=="sens_pandemic_initiation",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Pregnancy start \ndate >= 11/03/2020") +
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

sens4 <-ggplot(data = df[df$sens=="post_vaccination_anytime_non_vaccinated",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Anytime \nnon-vaccinated") +
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

sens5 <-ggplot(data = df[df$sens=="post_vaccination_anytime_vaccinated",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Anytime \nvaccinated") +
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

sens6 <-ggplot(data = df[df$sens=="post_vaccination_anytime_vaccinated_no_early_vacc",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Anytime vaccinated \nfrom 18/06/2021") +
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

sens7 <-ggplot(data = df[df$sens=="no_event_day0",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Excluding events \nat day 0") +
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

grid.arrange(sens1, sens2, sens3, sens4, sens5, sens6, sens7, ncol=7)

# Figure S9
sens_infection  <- arrangeGrob(sens1, sens2, sens3, sens4, sens5, sens6, sens7, ncol=7) 
ggsave(file="./sens_infection.png", sens_infection,  height = 200, width = 500, unit = "mm", dpi = 600, scale = 1)

################################################################################
################################################################################
#             SENSITIVITY  ANALYSIS  HOSPITALISATION                           #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df<- data.table::fread("./SDE/out_HRs_sens_hosp.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$model=="week2" | df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="covid_inf"  | df$exp=="E1" | df$exp=="E2",]
df <- df[!(df$conf.high=="0.000"   | df$conf.high==""   |  conf.high=="Inf" ),]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","\u2264 14 days",df$exp)
df$exp <- ifelse(df$exp=="E2","> 14 days",df$exp)
df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

term_to_time <- data.frame(exp = c("Total","\u2264 14 days", "> 14 days"),
                           time = c(5,4,3))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="> 14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="\u2264 14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c("> 14 days",
                                  "\u2264 14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names ------------------------------------------------------
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
sens_total_hosp_no <- ggplot(data = df[df$sens=="total" & df$hosp=="sens_hosp_no",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("> 14 days","\u2264 14 days","Total"))  +
  ggtitle("Non-hospitalised cases \nOverall") +
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
  facet_wrap(~event,strip.position="top", nrow=8,scales = "free_y") +  coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))
 
sens_total_hosp_yes <-ggplot(data = df[df$sens=="total" & df$hosp=="sens_hosp_yes",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Hospitalised cases \nOverall") +
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
  facet_wrap(~event,strip.position="top", nrow=9,scales = "free_y", ) + coord_flip()   +
  theme(legend.text = element_text(size = 9),
        axis.text.y=element_text(size=9),
        axis.title.x = element_text(size = 9),
        plot.title = element_text(size = 11))

sens_pre_vacc_hosp_no <-ggplot(data = df[df$sens=="pre_vaccination" & df$hosp=="sens_hosp_no",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle(" \nPre-vaccination era") +
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

sens_pre_vacc_hosp_yes <-ggplot(data = df[df$sens=="pre_vaccination" & df$hosp=="sens_hosp_yes",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle(" \nPre-vaccination era") +
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

sens_post_vacc_hosp_no <-ggplot(data = df[df$sens=="post_vaccination" & df$hosp=="sens_hosp_no",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle(" \nVaccination era") +
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

sens_post_vacc_hosp_yes <-ggplot(data = df[df$sens=="post_vaccination" & df$hosp=="sens_hosp_yes",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle(" \nVaccination era") +
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

grid.arrange(sens_total_hosp_no, sens_pre_vacc_hosp_no, sens_post_vacc_hosp_no, sens_total_hosp_yes, sens_pre_vacc_hosp_yes, sens_post_vacc_hosp_yes, ncol=6)

#Figure S8
sens_hosp  <- arrangeGrob(sens_total_hosp_no, sens_pre_vacc_hosp_no, sens_post_vacc_hosp_no, sens_total_hosp_yes, sens_pre_vacc_hosp_yes, sens_post_vacc_hosp_yes, ncol=6) 
ggsave(file="./sens_hosp.png", sens_hosp,  height = 200, width = 400, unit = "mm", dpi = 600, scale = 1)


################################################################################
################################################################################
#             SENSITIVITY  ANALYSIS PRETERM - VERY PRETERM                     #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df_sens <- data.table::fread("./SDE/out_HRs_sens_preterm.csv")
df <- rbind(df_sens, fill=TRUE)

# Criteria ---------------------------------------------------------------------
df <- df[df$sens=="ind_lab" | (df$sens=="preterm_cutoff" & df$event=="PRETERM") | (df$sens=="verypreterm_cutoff" & df$event=="VERY_PRETE") ,]
df <- df[df$model=="week2" | df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="covid_inf"  | df$exp=="E1" | df$exp=="E2",]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","\u2264 14 days",df$exp)
df$exp <- ifelse(df$exp=="E2","> 14 days",df$exp)
df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

term_to_time <- data.frame(exp = c("Total","\u2264 14 days", "> 14 days"),
                           time = c(5,4,3))

df <- merge(df, term_to_time, by = c("exp"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$exp=="> 14 days","#A52A2A",df$colour) 
df$colour <- ifelse(df$exp=="\u2264 14 days","#FF4500",df$colour)
df$colour <- ifelse(df$exp=="Total","#000000",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$exp <- factor(df$exp, levels=c("> 14 days",
                                  "\u2264 14 days",
                                  "Total"
)) 

df$colour <- factor(df$colour, levels=c(
  "#A52A2A",
  "#FF4500",
  "#000000" ))

# Rename event names -----------------------------------------------------------
df$sens <- ifelse(df$sens=="verypreterm_cutoff","cutoff",df$sens) 
df$sens <- ifelse(df$sens=="preterm_cutoff","cutoff",df$sens) 

df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 


df$event <- factor(df$event, levels=c( "Preterm",
                                       "Very preterm"
))

# Plot and save ----------------------------------------------------------------
sens1 <-ggplot(data = df[df$sens=="cutoff",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            hjust=-0.18, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("> 14 days","\u2264 14 days","Total"))  +
  ggtitle("Follow-up censored \nat the maximal outcome week") +
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
        plot.title = element_text(size = 11)
        )

sens2 <-ggplot(data = df[df$sens=="ind_lab",],
               mapping = aes(x = time, y = estimate, color = exp, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            hjust=-0.18, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.1,20), breaks = c(0.5,1,2,4,8,16), trans = "log")  +
  scale_x_continuous(lim = c(3,5), breaks = c(3,4,5), label = c("","",""))  +
  ggtitle("Spontaneous preterm") +
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
        plot.title = element_text(size = 11)
        )

grid.arrange(sens1, sens2, ncol=2)

#Figure S10
sens_preterm  <- arrangeGrob(sens1, sens2, ncol=2) 
ggsave(file="./sens_preterm.png", sens_preterm,  height = 80, width = 220, unit = "mm", dpi = 600, scale = 1)

################################################################################
################################################################################
#                             TRIMESTER                                        #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------
df<- data.table::fread("./SDE/out_HRs_trimester.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$model=="full",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]
df <- df[df$exp=="covid_inf",]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

df$tri <- ifelse(df$tri=="trimester1","1st trimester",df$tri)
df$tri <- ifelse(df$tri=="trimester2","2nd trimester",df$tri)
df$tri <- ifelse(df$tri=="trimester3","3rd trimester",df$tri)

term_to_time <- data.frame(tri = c("1st trimester", "2nd trimester", "3rd trimester"),
                           time = c(6,4,2))

df <- merge(df, term_to_time, by = c("tri"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
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
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestation age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event)

df$event <- factor(df$event, levels=c( "Gestational Diabetes",
                                       "Gestational Hypertension",
                                       "Preeclampsia",
                                       "Preterm",
                                       "Very preterm",
                                       "Small for gestation age",
                                       "Stillbirth",
                                       "Thrombotic venous event"
))

# Rename event names -----------------------------------------------------------
overall_period <-ggplot(data = df[df$sens=="overall_period",],
                        mapping = aes(x = time, y = estimate, color = tri, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 1.4)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 1.4))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            vjust=-0.5, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.1,16), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(1,7), breaks = c(2, 4, 6), label = c("3rd trimester","2nd trimester", "1st trimester"))  +
  ggtitle("Overall period") +
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

pre_vaccination <-ggplot(data = df[df$sens=="pre_vaccination",],
                        mapping = aes(x = time, y = estimate, color = tri, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 1.4)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 1.4))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            vjust=-0.5, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.1,16), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(1,7), breaks = c(2, 4, 6), label = c("3rd trimester","2nd trimester", "1st trimester"))  +
  ggtitle("Pre vaccination era") +
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

post_vaccination <-ggplot(data = df[df$sens=="post_vaccination",],
                         mapping = aes(x = time, y = estimate, color = tri, fill=exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 1.4)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 1.4))  +
  geom_text(aes(label=paste(round(estimate, 2), " (", round(conf.low, 2), "-", 
                            round(conf.high, 2), ")", sep="")), 
            position=position_dodge(width=0.25), 
            vjust=-0.5, size=2.5, colour = "#696969") +
  scale_y_continuous(lim = c(0.1,16), breaks = c(0.5,1,2,4,8), trans = "log")  +
  scale_x_continuous(lim = c(1,7), breaks = c(2, 4, 6), label = c("3rd trimester","2nd trimester", "1st trimester"))  +
  ggtitle("Vaccination era") +
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

grid.arrange(overall_period, pre_vaccination, post_vaccination, ncol=3)

#Figure 2
trimester <- arrangeGrob(overall_period, pre_vaccination, post_vaccination, ncol=3) 
ggsave(file="./trimester.png", trimester,  height = 250, width = 250, unit = "mm", dpi = 600, scale = 1)


# Read file --------------------------------------------------------------------
df<- data.table::fread("./SDE/out_HRs_trimester.csv")

# Criteria --------------------------------------------------------------------
df <- df[df$model=="full" | df$model=="week2",]
df <- df[df$adj=="full_adjusted",]
df <- df[df$subgroup==""  | is.na(df$subgroup),]

df <- df[!(df$event=="STILLBIRTH"   &  df$tri=="trimester1"),]

# Tidy variables ---------------------------------------------------------------
df$exp <- ifelse(df$exp=="E1","\u2264 14 days",df$exp)
df$exp <- ifelse(df$exp=="E2","> 14 days",df$exp)
df$exp <- ifelse(df$exp=="covid_inf","Total",df$exp)

df$tri <- ifelse(df$tri=="trimester1","1st trimester",df$tri)
df$tri <- ifelse(df$tri=="trimester2","2nd trimester",df$tri)
df$tri <- ifelse(df$tri=="trimester3","3rd trimester",df$tri)

term_to_time <- data.frame(tri = c("1st trimester", "2nd trimester", "3rd trimester"),
                           time = c(3,2,1))

df <- merge(df, term_to_time, by = c("tri"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
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
df$event <- ifelse(df$event=="GEST_DIABE","Gestational Diabetes",df$event)
df$event <- ifelse(df$event=="PREECLAMPS","Preeclampsia",df$event)
df$event <- ifelse(df$event=="PRETERM","Preterm",df$event) 
df$event <- ifelse(df$event=="VERY_PRETE","Very preterm",df$event) 
df$event <- ifelse(df$event=="SMALL_GEST","Small for gestation age",df$event)
df$event <- ifelse(df$event=="STILLBIRTH","Stillbirth",df$event)
df$event <- ifelse(df$event=="VENOUS","Thrombotic venous event",df$event)

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
trimester_sens <-ggplot(data = df[df$sens=="overall_period",],
                        mapping = aes(x = time, y = estimate, color = exp, fill= exp)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.4))  +
  scale_y_continuous(lim = c(0.35,16), breaks = c(0.4,1,4,10), trans = "log")  +
  scale_x_continuous(lim = c(0,4), breaks = c(3, 2, 1), label = c("1st trimester","2nd trimester", "3rd trimester"))  +
  ggtitle("Overall period") +
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

#Figure S11
ggsave(file="./trimester_sens.png", trimester_sens,  height = 280, width = 220, unit = "mm", dpi = 600, scale = 1)