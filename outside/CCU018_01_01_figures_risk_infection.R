################################################################################
## TITLE: Prepare figures for association between COVID-19 vacciantion and
## infection
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
df_eng <- read_delim("./SDE/out_risk_covid_infection.csv", 
                      ",", escape_double = FALSE, trim_ws = TRUE)
df_eng$nation <- "England"
df_eng <- df_eng[df_eng$subgroup=="" | is.na(df_eng$subgroup),]
df_eng <- select(df_eng, -subgroup)

# Read file --------------------------------------------------------------------
df_sail <- read_delim("./SAIL/out_risk_covid_infection_to_export.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
df_sail$nation <- "Wales"

df <- rbind(df_eng,df_sail, fill=TRUE)

# Criteria  --------------------------------------------------------------------
df <- df[df$adj=="full_adjusted",]
df <- df[df$analysis=="vaccination_era",]
df <- df[df$event=="COVID_INFE",]

# Tidy variables ---------------------------------------------------------------
df$event <- ifelse(df$event=="COVID_INFE","Covid infection",df$event) 

needs_meta <- unique(df[,c("week","event")])

# Perform meta-analysis for each event/stratum combination ---------------------
for (i in 1:nrow(needs_meta)) {
  
  tmp <- df[df$week==needs_meta$week[i],]
  
  meta <- unique(tmp[,c("week","event")])

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
  
  for (j in unique(meta$week)) {
    tmp2 <- tmp[tmp$week==j,]
    tmp_meta <- meta::metagen(log(tmp2$estimate),tmp2$stad.error, sm = "HR")
    meta[meta$week==j,]$estimate <- exp(tmp_meta$TE.fixed)
    meta[meta$week==j,]$conf.low <- exp(tmp_meta$lower.fixed)
    meta[meta$week==j,]$conf.high <- exp(tmp_meta$upper.fixed)
    meta[meta$week==j,]$p.value <- tmp_meta$pval.fixed
    meta[meta$week==j,]$stad.error <- tmp_meta$seTE.fixed
  }
  df <- plyr::rbind.fill(df, meta)
}

df$nation[is.na(df$nation)] <- "meta"
df <- df[df$nation=="meta",]

# Specify time -----------------------------------------------------------------
term_to_time <- data.frame(week = c("week1_4", "week5_8", "week9_50"),
                           time = c(0.5,3.5,7))
df <- merge(df, term_to_time, by = c("week"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$event=="Covid infection","#228B22",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$event <- factor(df$event, levels=c("Covid infection"
)) 

df$colour <- factor(df$colour, levels=c(
  "#228B22"
))  

# Plot and save ----------------------------------------------------------------
risk_infection_1st<-ggplot(data = df,
                           mapping = aes(x = time, y = estimate, color = event, shape=event, fill=event)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9") +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                                ymax = ifelse(conf.high>64,64,conf.high),  
                                                width = 0), 
                         position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  scale_y_continuous(lim = c(0.5,1.1), breaks = c(0.5,0.6, 0.7, 0.8,0.9,1,1.1), trans = "log") +
  scale_x_continuous(breaks = c(0.5, 3.5, 7), label = c("week 1-4", "week 5-8", "week 9+")) +
  scale_fill_manual(values = levels(df$colour)[1],) +
  scale_color_manual(values = levels(df$colour)[1],) +
  labs(x = "\nWeeks since dose 1 of COVID-19 vaccine", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.spacing.x = unit(0.5, "lines"),
                 panel.spacing.y = unit(0, "lines"),
                 legend.key = element_rect(colour = NA, fill = NA),
                 #                  legend.title = element_blank(),
                 legend.position="none",
                 plot.background = element_rect(fill = "white", colour = "white")) 
risk_infection_1st + theme(legend.text = element_text(size = 11))

# Read file --------------------------------------------------------------------
df <- data.table::fread("./SDE/out_risk_covid_infection_2nd.csv")

# Criteria --------------------------------------------------------------------
df <- df[df$adj=="full_adjusted",]
df <- df[df$analysis=="vaccination_era",]
df <- df[df$event=="COVID_INFE",]
df <- df[df$subgroup=="",]

# Tidy variables --------------------------------------------------------------
df$event <- ifelse(df$event=="COVID_INFE","Covid infection",df$event) 
# df$event <- ifelse(df$event=="COVID_HOSP","Covid hospitalization",df$event) 

# Specify time -----------------------------------------------------------------
term_to_time <- data.frame(week = c("week1_4", "week5_8", "week9_50"),
                           time = c(0.5,3.5,7))
df <- merge(df, term_to_time, by = c("week"), all.x = TRUE)

# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$event=="Covid infection","#228B22",df$colour) 

# Factor variables for ordering-------------------------------------------------
df$event <- factor(df$event, levels=c("Covid infection"
)) 

df$colour <- factor(df$colour, levels=c(
  "#228B22"
))  

# Plot and save ----------------------------------------------------------------
risk_infection_2nd<-ggplot(data = df,
                       mapping = aes(x = time, y = estimate, color = event, shape=event, fill=event)) +
  geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9") +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                              ymax = ifelse(conf.high>64,64,conf.high),  
                              width = 0), 
                position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  scale_y_continuous(lim = c(0.5,1.1), breaks = c(0.5,0.6, 0.7, 0.8,0.9,1,1.1), trans = "log") +
  scale_x_continuous(breaks = c(0.5, 3.5, 7), label = c("week 1-4", "week 5-8", "week 9+")) +
  scale_fill_manual(values = levels(df$colour)[1],) +
  scale_color_manual(values = levels(df$colour)[1],) +
  labs(x = "\nWeeks since dose 2 of COVID-19 vaccine", y = "HR and 95% CI") +
  guides(fill=guide_legend(ncol = 4, byrow = TRUE)) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.spacing.x = unit(0.5, "lines"),
        panel.spacing.y = unit(0, "lines"),
        legend.key = element_rect(colour = NA, fill = NA),
        #                  legend.title = element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "white", colour = "white")) 
risk_infection_2nd + theme(legend.text = element_text(size = 11))

grid.arrange(risk_infection_1st, risk_infection_2nd, ncol=2)

#Figure S12
risk_infection_main <- arrangeGrob(risk_infection_1st, risk_infection_2nd, ncol=2)
ggsave(file="./out_risk_covid_infection.png", risk_infection_main,  height = 180, width = 300, unit = "mm", dpi = 600, scale = 1)


################################################################################
################################################################################
#                         SENSITIVITY   ANALYSES                               #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------  
df <- data.table::fread("./SDE/out_risk_covid_infection.csv")

# Criteria ---------------------------------------------------------------------  
df <- df[df$adj=="full_adjusted",]
df <- df[df$analysis=="vaccination_era_anytime_vaccinated" | df$analysis=="vaccination_era_anytime_vaccinated_no_early_vacc" | df$analysis=="vaccination_era_noprevcovid",]
df <- df[df$event=="COVID_INFE",]
df <- df[df$subgroup=="",]
  
# Tidy variables --------------------------------------------------------------
df$event <- ifelse(df$event=="COVID_INFE","Covid infection",df$event) 

# Specify time -----------------------------------------------------------------
term_to_time <- data.frame(week = c("week1_4", "week5_8", "week9_50"),
                             time = c(0.5,3.5,7))
df <- merge(df, term_to_time, by = c("week"), all.x = TRUE)
  
# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$analysis=="vaccination_era_anytime_vaccinated","#9ACD32",df$colour) 
df$colour <- ifelse(df$analysis=="vaccination_era_anytime_vaccinated_no_early_vacc","#66CDAA",df$colour)
df$colour <- ifelse(df$analysis=="vaccination_era_noprevcovid","#228B22",df$colour) 
  
# Factor variables for ordering-------------------------------------------------
df$event <- factor(df$analysis, levels=c("vaccination_era_anytime_vaccinated",
                                           "vaccination_era_anytime_vaccinated_no_early_vacc",
                                           "vaccination_era_noprevcovid"
  )) 
  
df$colour <- factor(df$colour, levels=c(
    "#9ACD32",
    "#66CDAA",
    "#228B22"
))  
  
# Plot and save ----------------------------------------------------------------
risk_infection_sens_1st<-ggplot(data = df,
                         mapping = aes(x = time, y = estimate, color = analysis, shape=analysis, fill=analysis)) +
    geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9") +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                ymax = ifelse(conf.high>64,64,conf.high),  
                                width = 0), 
                  position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2)) +
    scale_y_continuous(lim = c(0.4,1.1), breaks = c(0.4,0.5,0.6, 0.7,0.8,0.9,1,1.1), trans = "log") +
    scale_x_continuous(breaks = c(0.5, 3.5, 7), label = c("week1-4", "week5-8", "week9+")) +
    scale_fill_manual(values = levels(df$colour)[1:3],) +
    scale_color_manual(values = levels(df$colour)[1:3],) +
    labs(x = "\nWeeks since dose 1 of COVID-19 vaccine", y = "HR and 95% CI") +
    guides(fill=guide_legend(ncol = 4, byrow = TRUE)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(0.5, "lines"),
          panel.spacing.y = unit(0, "lines"),
          legend.key = element_rect(colour = NA, fill = NA),
          #                  legend.title = element_blank(),
          legend.position="bottom",
          plot.background = element_rect(fill = "white", colour = "white")) 
  risk_infection_sens_1st + theme(legend.text = element_text(size = 11))  

# Read file --------------------------------------------------------------------
df <- data.table::fread("./SDE/out_risk_covid_infection_2nd.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$adj=="full_adjusted",]
df <- df[df$analysis=="vaccination_era_anytime_vaccinated" | df$analysis=="vaccination_era_anytime_vaccinated_no_early_vacc" | df$analysis=="vaccination_era_noprevcovid",]
df <- df[df$event=="COVID_INFE",]
df <- df[df$subgroup=="",]
  
# Tidy variables --------------------------------------------------------------
df$event <- ifelse(df$event=="COVID_INFE","Covid infection",df$event) 
  
# Specify time -----------------------------------------------------------------
term_to_time <- data.frame(week = c("week1_4", "week5_8", "week9_50"),
                             time = c(0.5,3.5,7))
df <- merge(df, term_to_time, by = c("week"), all.x = TRUE)
  
# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$analysis=="vaccination_era_anytime_vaccinated","#9ACD32",df$colour) 
df$colour <- ifelse(df$analysis=="vaccination_era_anytime_vaccinated_no_early_vacc","#66CDAA",df$colour)
df$colour <- ifelse(df$analysis=="vaccination_era_noprevcovid","#228B22",df$colour) 
  
# Factor variables for ordering-------------------------------------------------
df$event <- factor(df$analysis, levels=c("vaccination_era_anytime_vaccinated",
                                           "vaccination_era_anytime_vaccinated_no_early_vacc",
                                           "vaccination_era_noprevcovid"
)) 
  
df$colour <- factor(df$colour, levels=c(
    "#9ACD32",
    "#66CDAA",
    "#228B22"
))  
  
# Plot and save ----------------------------------------------------------------
risk_infection_sens_2nd<-ggplot(data = df,
                                  mapping = aes(x = time, y = estimate, color = analysis, shape=analysis, fill=analysis)) +
    geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9") +
    geom_point(position = position_dodge(width = 0.2)) +
    geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                ymax = ifelse(conf.high>64,64,conf.high),  
                                width = 0), 
                  position = position_dodge(width = 0.2)) +
    geom_line(position = position_dodge(width = 0.2)) +
    scale_y_continuous(lim = c(0.4,1.1), breaks = c(0.4,0.5,0.6, 0.7,0.8,0.9,1,1.1), trans = "log") +
    scale_x_continuous(breaks = c(0.5, 3.5, 7), label = c("week1-4", "week5-8", "week9+")) +
    scale_fill_manual(values = levels(df$colour)[1:3],) +
    scale_color_manual(values = levels(df$colour)[1:3],) +
    labs(x = "\nWeeks since dose 2 of COVID-19 vaccine", y = "HR and 95% CI") +
    guides(fill=guide_legend(ncol = 4, byrow = TRUE)) +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(0.5, "lines"),
          panel.spacing.y = unit(0, "lines"),
          legend.key = element_rect(colour = NA, fill = NA),
          #                  legend.title = element_blank(),
          legend.position="bottom",
          plot.background = element_rect(fill = "white", colour = "white")) 
  risk_infection_sens_2nd + theme(legend.text = element_text(size = 11))  
  
grid.arrange(risk_infection_sens_1st, risk_infection_sens_2nd, ncol=2)

#Figure S13
risk_infection_sens <- arrangeGrob(risk_infection_sens_1st, risk_infection_sens_2nd, ncol=2) #generates g
ggsave(file="./out_risk_covid_infection_sens.png", risk_infection_sens,  height = 200, width = 250, unit = "mm", dpi = 600, scale = 1)

################################################################################
################################################################################
#                                STRATIFICATION                                #
################################################################################
################################################################################

# Read file --------------------------------------------------------------------   
df <- data.table::fread("./SDE/out_risk_covid_infection.csv")

# Criteria ---------------------------------------------------------------------  
df <- df[df$adj=="full_adjusted",]
df <- df[df$analysis=="vaccination_era",]
df <- df[df$event=="COVID_INFE",]
df <- df[!df$subgroup=="",]
  
# Tidy variables ---------------------------------------------------------------
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
    if(df$subgroup[i-1] == 'White' & df$subgroup[i] == 'Asian' & df$stratum[i] == "Ethnic group") {
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
  
# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$week=="week9_50","#228B22",df$colour) 
df$colour <- ifelse(df$week=="week5_8","#9ACD32",df$colour)
df$colour <- ifelse(df$week=="week1_4","#66CDAA",df$colour) 
  
# Factor variables for ordering-------------------------------------------------
df$week <- factor(df$week, levels=c("week9_50",
                                      "week5_8",
                                      "week1_4"
)) 
  
df$colour <- factor(df$colour, levels=c(
    "#228B22",
    "#9ACD32",
    "#66CDAA"
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
risk_infection_stratification_1st <-ggplot(data = df,
              mapping = aes(x = stratum, y = estimate, color = week, fill= week)) +
    geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
    geom_point(position = position_dodge(width = 1)) +
    geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                ymax = ifelse(conf.high>64,64,conf.high),  
                                width = 0), 
                  position = position_dodge(width = 1))  +
    geom_line(position = position_dodge(width = 1)) +
    scale_y_continuous(lim = c(0.1,5), breaks = c(0.3,0.5,1,2,4), trans = "log")  +
    # scale_x_continuous(lim = c(0,5), breaks = c(2, 3, 4), label = c(" "," ", " "))  +
    scale_fill_manual(values = levels(df$colour)[1:3]) +
    scale_color_manual(values = levels(df$colour)[1:3]) +
    labs(x ="", y = "HR and 95% CI") +
    ggtitle("dose 1 of COVID-19 vaccine") +
    guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(0.5, "lines"),
          panel.spacing.y = unit(0, "lines"),
          legend.key = element_rect(colour = NA, fill = NA),
          #                  legend.title = element_blank(),
          legend.position="bottom",
          strip.background = element_blank(), strip.placement = "outside",
          plot.background = element_rect(fill = "white", colour = "white")) +
    facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
    theme(legend.text = element_text(size = 9),
          axis.text.y=element_text(size=9),
          axis.title.x = element_text(size = 9),
          plot.title = element_text(size = 11))

# Read file --------------------------------------------------------------------  
df <- data.table::fread("./SDE/out_risk_covid_infection_2nd.csv")

# Criteria ---------------------------------------------------------------------
df <- df[df$adj=="full_adjusted",]
df <- df[df$analysis=="vaccination_era",]
df <- df[df$event=="COVID_INFE",]
df <- df[!df$subgroup=="",]
  
# Tidy variables ---------------------------------------------------------------
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
  
# Specify line colours ---------------------------------------------------------
df$colour <- ""
df$colour <- ifelse(df$week=="week9_50","#228B22",df$colour) 
df$colour <- ifelse(df$week=="week5_8","#9ACD32",df$colour)
df$colour <- ifelse(df$week=="week1_4","#66CDAA",df$colour) 
  
# Factor variables for ordering-------------------------------------------------
df$week <- factor(df$week, levels=c("week9_50",
                                      "week5_8",
                                      "week1_4"
  )) 
  
df$colour <- factor(df$colour, levels=c(
    "#228B22",
    "#9ACD32",
    "#66CDAA"
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
risk_infection_stratification_2nd <-ggplot(data = df,
                                             mapping = aes(x = stratum, y = estimate, color = week, fill= week)) +
    geom_hline(mapping = aes(yintercept = 1), colour = "#A9A9A9")  +
    geom_point(position = position_dodge(width = 1)) +
    geom_errorbar(mapping = aes(ymin = ifelse(conf.low<0.25,0.25,conf.low), 
                                ymax = ifelse(conf.high>64,64,conf.high),  
                                width = 0), 
                  position = position_dodge(width = 1))  +
    geom_line(position = position_dodge(width = 1)) +
    scale_y_continuous(lim = c(0.1,5), breaks = c(0.3,0.5,1,2,4), trans = "log")  +
    scale_fill_manual(values = levels(df$colour)[1:3]) +
    scale_color_manual(values = levels(df$colour)[1:3]) +
    labs(x ="", y = "HR and 95% CI") +
    ggtitle("dose 2 of COVID-19 vaccine") +
    guides(fill=guide_legend(ncol = 4, byrow = TRUE))  +
    theme_minimal() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.spacing.x = unit(0.5, "lines"),
          panel.spacing.y = unit(0, "lines"),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.position="bottom",
          strip.background = element_blank(), strip.placement = "outside",
          plot.background = element_rect(fill = "white", colour = "white")) +
    facet_wrap(~stratification,strip.position="top",nrow=9,scales = "free_y") + coord_flip()   +
    theme(legend.text = element_text(size = 9),
          axis.text.y=element_text(size=9),
          axis.title.x = element_text(size = 9),
          plot.title = element_text(size = 11))

grid.arrange(risk_infection_stratification_1st, risk_infection_stratification_2nd, ncol=2)

#Figure S14
risk_infection_stratification <- arrangeGrob(risk_infection_stratification_1st, risk_infection_stratification_2nd, ncol=2)
ggsave(file="./out_risk_covid_infection_stratification.png", risk_infection_stratification,  height = 200, width = 250, unit = "mm", dpi = 600, scale = 1)
  