library(reshape2)
library(readr)
library(cowplot)
setwd("~/Desktop/RAALLPET")

# Read file with demographical data
Subject_data <- read.csv("~/Desktop/RAALLPET/PET_group_gene_EVF_170323.csv", sep=";")
Subject_data <- Subject_data[ ,c(1, 2, 4:7, 11:15)]
names(Subject_data) <- c("Subject", "Sex", "Age", "Group", "BMI_1", "BMI_2", "PET1", "PET2", "In_season",
                         "Out_season", "Genotype")

# Rename group
levels(Subject_data$Group)[levels(Subject_data$Group)=="Allergi"] <- "Allergy"
levels(Subject_data$Group)[levels(Subject_data$Group)=="Kontroll-RA"] <- "Ctrl-RA"
levels(Subject_data$Group)[levels(Subject_data$Group)=="Kontroll-allergi"] <- "Ctrl-all"
levels(Subject_data$Group)[levels(Subject_data$Group)=="Kontroll-allergi+RA"] <- "Ctrl-all+RA"

# Rename sex
levels(Subject_data$Sex)[levels(Subject_data$Sex)=="Kvinna"] <- "Female"
levels(Subject_data$Sex)[levels(Subject_data$Sex)=="Man"] <- "Male"



# Add variables for which ones to use etc. Calculate demographics or divide in age categories. Maybe some more should be added in file

# Convert to long format and reorder
Subject_data_long <- melt(Subject_data, id.vars = c("Subject", "Sex", 
                                                    "Age", "Group","BMI_1", "BMI_2",
                                                    "PET1", "PET2", "Genotype"))

Subject_data_long <- Subject_data_long[ order(Subject_data_long$Subject, Subject_data_long$variable), ]

# Generate data frame for allergy only
Subject_data_long_allergy <- subset(Subject_data_long, Group == "Allergy" | Group == "Ctrl-all" 
                                    | Group == "Ctrl-all+RA")

# Define a new column for session (first or second PET), based on 2 previous columns
Subject_data_long_allergy$Session <- as.Date(Subject_data_long_allergy$PET1) - as.Date(Subject_data_long_allergy$value)
Subject_data_long_allergy$Session[Subject_data_long_allergy$Session == 0] <- 1
Subject_data_long_allergy$Session[Subject_data_long_allergy$Session < 0] <- 2
Subject_data_long_allergy$Session <- as.factor(Subject_data_long_allergy$Session)


# Rename columns
names(Subject_data_long_allergy)[names(Subject_data_long_allergy) == 'value'] <- 'Date'
names(Subject_data_long_allergy)[names(Subject_data_long_allergy) == 'variable'] <- 'Pollen_status'

# Remove observations from subject 19, session 2 (no PET), 
Subject_data_long_allergy <- subset(Subject_data_long_allergy, (!is.na(Subject_data_long_allergy$Session)))

# Subject 69, 2:nd sessions (no PET) and subject 78, 2:nd sessions (no PET) are kept for other data

# Combine 2 groups of controls
Subject_data_long_allergy$Group[Subject_data_long_allergy$Group == "Ctrl-all+RA"] <- "Ctrl-all"

# Drop unused levels
Subject_data_long_allergy$Group <- droplevels(Subject_data_long_allergy$Group)
Subject_data_long_allergy$Sex <- droplevels(Subject_data_long_allergy$Sex)

# Rename groups
levels(Subject_data_long_allergy$Group)[levels(Subject_data_long_allergy$Group)=="Ctrl-all"] <- "Control"

# Date in correct format
Subject_data_long_allergy$PET.date <- as.Date(Subject_data_long_allergy$Date)

# Mean BMI for those with 2 values
Subject_data_long_allergy$MeanBMI <- (Subject_data_long_allergy$BMI_1 + Subject_data_long_allergy$BMI_2)/2

Subject_list_allergy <- Subject_data_long_allergy[ ,c(1:4, 9, 10, 12:14)]

# Rename pollen status
levels(Subject_list_allergy$Pollen_status)[levels(Subject_list_allergy$Pollen_status)=="In_season"] <- "IN"
levels(Subject_list_allergy$Pollen_status)[levels(Subject_list_allergy$Pollen_status)=="Out_season"] <- "OUT"
# Pseudomise data if released

save(Subject_list_allergy, file = "Demographics_allergy.RData")
save(Subject_data, file = "Demographics_all.RData")

rm(list = ls())


#This part is also presented in the open repo
require(Gmisc)
require(gdata)
require(ggplot2)
require(reshape2)
require(nlme)
require(multcomp)
library(readxl)
library(htmlTable)

source('Utils/SummarisingFunctions.R', chdir = T)

cbbPalette <- c("#0072B2", "#009E73", "#D55E00", "#F0E442", "#CC79A7")

load("Demographics_allergy.RData")
Education <- read.xls("Education_170329.xlsx")[c(1, 5)]
MFI <- read.xls("MFI20_170404.xlsx")
Rhinitis_symptoms <- read.csv("~/Desktop/RAALLPET/Rhinitis_symptoms_170512.csv", sep = ";")
ListOfPairs <- read_delim("~/Desktop/RAALLPET/ListOfPairs.csv", ";", escape_double = FALSE, trim_ws = TRUE)


Subject_list_allergy <- merge(Subject_list_allergy, Education, by.x = "Subject", by.y = "Fp.nummer") 
Subject_list_allergy <- merge(Subject_list_allergy, MFI, 
                              by.x = c("Subject", "Pollen_status"), by.y = c("Subject", "Pollen_season"))

Subject_list_allergy <- merge(Subject_list_allergy, ListOfPairs)

Rhinitis_symptoms <- merge(Subject_list_allergy, Rhinitis_symptoms, by.x = c("Subject", "Pollen_status"), 
                           by.y = c("X...Subject", "Pollen_season"))

Astma_symptoms <- read_delim("~/Desktop/RAALLPET/Astma_symptoms.csv", "\t", escape_double = FALSE, trim_ws = TRUE)

Demographic <- Subject_list_allergy[!duplicated(Subject_list_allergy$Subject),]
Demographic$Education_level <- droplevels(Demographic$Education_level)
Demographic$Genotype <- droplevels(Demographic$Genotype)

Count_data <- summary(Demographic$Group)
Age_data <- getDescriptionStatsBy(Demographic$Age, Demographic$Group, html=TRUE, 
                                  continuous_fn = describeMedian)
Sex_data <- getDescriptionStatsBy(Demographic$Sex, Demographic$Group, html=TRUE)
BMI_data <- getDescriptionStatsBy(Demographic$MeanBMI, Demographic$Group, html=TRUE, useNA = "no")
Edu_data <- getDescriptionStatsBy(Demographic$Education_level, Demographic$Group, html=TRUE, useNA = "no")
Gene_data <- getDescriptionStatsBy(Demographic$Genotype, Demographic$Group, html=TRUE, useNA = "no")


# Function to show htmlTable in viewer.
viewHtmlTable <- function(htmlText) {
  tf <- tempfile(fileext = ".html")
  writeLines(htmlText, tf)
  getOption("viewer")(tf)
}

# Make table with demographics
viewHtmlTable(htmlTable(
  x        = rbind(Count_data, Gene_data, Age_data, Sex_data, BMI_data, Edu_data),
  caption  = paste("Table 1. Demographical variables. Continuous values are reported as",
                   "means with standard deviations, unless otherwise indicated). Categorical data",
                   "are reported with percentages."),
  label    = "Table1",
  rowlabel = "Variables",
  rnames = c("Number of subjects", "Low affinity binders", "Mixed affinity binders", 
             "High affinity binders", "Age (median, interquartile range)", "Sex (females)", "BMI", 
             "College graduate", "Elementary/High School", "Other", "Some college",
             "Some university", "University graduate"),
  rgroup   = c("Sample", "Genotype",
               "Demographics", "Education"),
  n.rgroup = c(1,
               3,
               3,
               6),
  ctable   = TRUE,
))


summariseRow <- function(measurevar) {
  symptomRow <- summarySEwithin(summarySEwithin(Rhinitis_symptoms, measurevar=measurevar, 
                                                betweenvars = "Group", withinvars = c("Pollen_status", "Subject"), 
                                                na.rm = T), measurevar=measurevar, 
                                betweenvars = "Group", withinvars = "Pollen_status")
  
  f <- reformulate("Pollen_status", measurevar)
  allergyP <- anova(lme(f, data = subset(Rhinitis_symptoms, Group == "Allergy"),
                        random = ~ 1|Subject, na.action = na.exclude))$`p-value`[2]
  
  if(sum(subset(Rhinitis_symptoms, Group == "Control")[measurevar], na.rm = T) == 0){
    controlP <- 1
  }else{
    controlP <- anova(lme(f, data = subset(Rhinitis_symptoms, Group == "Control"),
                          random = ~ 1|Subject, na.action = na.exclude))$`p-value`[2]
  }
  
  f <- reformulate("Group", measurevar)
  allergyVControlP <- anova(lme(f, data = subset(Rhinitis_symptoms, Pollen_status == "OUT"),
                                random = ~ 1|Subject, na.action = na.exclude))$`p-value`[2]
  
  means <- round(symptomRow[measurevar], digits = 2)
  lowerBounds <- round(symptomRow[measurevar] - symptomRow["ci"], digits = 2)
  upperBounds <- round(symptomRow[measurevar] + symptomRow["ci"], digits = 2)
  
  result <- c(paste(means[1, 1], " (", lowerBounds[1, 1], "-", upperBounds[1, 1], ")", sep = ""))
  result <- c(result, paste(means[2, 1], " (", lowerBounds[2, 1], "-", upperBounds[2, 1], ")", sep = ""))
  if(allergyP < 0.001){
    result <- c(result, paste("<0.001"))
  }else{
    result <- c(result, allergyP) 
  }
  result <- c(result, paste(means[3, 1], " (", lowerBounds[3, 1], "-", upperBounds[3, 1], ")", sep = ""))
  result <- c(result, paste(means[4, 1], " (", lowerBounds[4, 1], "-", upperBounds[4, 1], ")", sep = ""))
  if(controlP < 0.001){
    result <- c(result, paste("<0.001"))
  }else{
    result <- c(result, round(controlP, digits = 2)) 
  }
  if(allergyVControlP < 0.001){
    result <- c(result, paste("<0.001"))
  }else{
    result <- c(result, round(allergyVControlP, digits = 2)) 
  }
  
  return(result)
}

XRunny_Nose <- summariseRow("Runny_nose")
XItching_nose <- summariseRow("Itching_nose")
XSneezing <- summariseRow("Sneezing")
XNasal_congestion <- summariseRow("Nasal_congestion")
XLoss_of_smell <- summariseRow("Loss_of_smell")
XRunny_eyes <- summariseRow("Runny_eyes")
XItching_eyes <- summariseRow("Itching_eyes")
XEye_redness <- summariseRow("Eye_redness")
XSwollen_eyes <- summariseRow("Swollen_eyes")


Astma_symptoms$ACQ <- round(rowMeans(Astma_symptoms[5:10],
                                     na.rm = T), digits = 2)

Astma_summary <- summarySEwithin(Astma_symptoms, measurevar= "ACQ", 
                                 betweenvars = "Group", withinvars = "Pollen_season", 
                                 na.rm = T)

Astma_summary[4:8] <- round(Astma_summary[4:8], digits = 2)

ACQ <- vector()
ACQ[1] <- c(paste(Astma_summary[1,4], " (", Astma_summary[1,4]-Astma_summary[1,8], "-", 
                  Astma_summary[1,4]+Astma_summary[1,8], ")", sep = ""))
ACQ[2] <- c(paste(Astma_summary[2,4], " (", Astma_summary[2,4]-Astma_summary[2,8], "-", 
                  Astma_summary[2,4]+Astma_summary[2,8], ")", sep = ""))
ACQ[3] <- round(anova(lme(ACQ ~ Pollen_season, data = subset(Astma_symptoms, Group == "All"), 
                          random = ~ 1|Subject, na.action = na.exclude))$`p-value`[2], digits = 3)
ACQ[4] <- c(paste(Astma_summary[3,4], " (", Astma_summary[3,4]-Astma_summary[3,8], "-", 
                  Astma_summary[3,4]+Astma_summary[3,8], ")", sep = ""))
ACQ[5] <- c(paste(Astma_summary[4,4], " (", Astma_summary[4,4]-Astma_summary[4,8], "-", 
                  Astma_summary[4,4]+Astma_summary[4,8], ")", sep = ""))
ACQ[6] <- 1
ACQ[7] <- round(anova(lme(ACQ ~ Group, data = subset(Astma_symptoms, Pollen_season == "out"), 
                          random = ~ 1|Subject, na.action = na.exclude))$`p-value`[2], digits = 2)

# Make table with symptoms
viewHtmlTable(htmlTable(
  x        = rbind(c("Allergic", " ", " ", "Controls", " ", " ", " "),
                   c("In", "Out", "p (allergic: in vs out)", "In", "Out", "p (controls: in vs out)", 
                     "p (allergic vs controls (out))"), XRunny_Nose, XItching_nose, XSneezing, XNasal_congestion,
                   XLoss_of_smell, XRunny_eyes, XItching_eyes, XEye_redness, XSwollen_eyes,
                   ACQ
  ),
  caption  = paste("Table 2. Rhinitis symptoms"),
  label    = "Table2",
  rowlabel = "Variables",
  rnames = c("Group", "Pollen season", "Runny nose", "Itching nose", "Sneezing", "Nasal congestion", "Loss of smell",
             "Runny eyes", "Itching eyes", "Eye redness", "Swollen eyes", "Asthma Control Questionnaire"),
  rgroup   = c(" ", "Symptoms"),
  n.rgroup = c(2, 10),
  ctable   = TRUE,
))




# Change to long format
long_fatigue <- melt(Subject_list_allergy, id.vars=c("Subject", "Group", "Pollen_status", "Sex", "Age", "Genotype", 
                                                     "Session", "PET.date", "MeanBMI", "Education_level", "Pair"))
# Summarize MFI for plot
SummaryMFI <- summarySEwithin(long_fatigue, measurevar="value", 
                              betweenvars = "Group", withinvars = c("Pollen_status", "variable"), na.rm = T)

# Plot MFI
dodge <- position_dodge(width = 0.9)

ggplot(SummaryMFI, aes(x = variable, y = value, fill = factor(interaction(Pollen_status, Group)))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  ylim(0, 16) +
  geom_errorbar(aes(ymax = value + ci, ymin = value - ci), position = dodge, width = 0) +
  ylab("Score") +
  scale_x_discrete(labels=c("General_Fatigue" = "General \nfatigue", "Physical_Fatigue" = "Physical \nfatigue",
                            "Mental_Fatigue" = "Mental \nfatigue", "Reduced_Activity" = "Reduced \nactivity", 
                            "Reduced_Motivation" = "Reduced \nmotivation"))+
  scale_fill_manual(name = "Group", 
                    breaks=c("IN.Allergy", "OUT.Allergy", "IN.Control", "OUT.Control"),
                    labels=c("Allergy (in)", "Allergy (out)", "Control (in)", "Control (out)"), values=cbbPalette) +
  theme(axis.title.x=element_blank(), legend.justification = "center", 
        legend.position=c(0.5,0.9), legend.direction = "horizontal")

# Relevel factors with logical ref
Subject_list_allergy$Group <- relevel(Subject_list_allergy$Group, ref = "Control")
Subject_list_allergy$Pollen_status <- relevel(Subject_list_allergy$Pollen_status, ref = "OUT")


contrasts(Subject_list_allergy$Group) <- rbind(-.5, .5)
colnames(contrasts(Subject_list_allergy$Group)) <- levels(Subject_list_allergy$Group)[2]

contrasts(Subject_list_allergy$Pollen_status) <- rbind(-.5, .5)
colnames(contrasts(Subject_list_allergy$Pollen_status)) <- levels(Subject_list_allergy$Pollen_status)[2]



Subject_list_allergy$Pollen_status_n[Subject_list_allergy$Pollen_status == "IN"] <- 2
Subject_list_allergy$Pollen_status_n[Subject_list_allergy$Pollen_status == "OUT"] <- 1

Subject_list_allergy$scat_adj[Subject_list_allergy$Group == "Allergy"] <- 0.20
Subject_list_allergy$scat_adj[Subject_list_allergy$Group == "Control"] <- -0.20


GF_plot <- ggplot(Subject_list_allergy, aes(x=Pollen_status, y=General_Fatigue, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 20) +
  scale_fill_manual(name = "Group", 
                    breaks=c("Allergy", "Control"),
                    labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, General_Fatigue),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2) +
  xlab("Pollen season") +
  ylab("General fatigue")

PF_plot <- ggplot(Subject_list_allergy, aes(x=Pollen_status, y=Physical_Fatigue, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 20) +
  scale_fill_manual(name = "Group", 
                    breaks=c("All", "Ctrl"),
                    labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, Physical_Fatigue),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2,
              show.legend=FALSE) +
  xlab("Pollen season") +
  ylab("Physical fatigue")

MF_plot <- ggplot(Subject_list_allergy, aes(x=Pollen_status, y=Mental_Fatigue, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 20) +
  scale_fill_manual(name = "Group", 
                    breaks=c("All", "Ctrl"),
                    labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, Mental_Fatigue),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2,
              show_guide=FALSE) +
  xlab("Pollen season") +
  ylab("Mental fatigue")

RM_plot <- ggplot(Subject_list_allergy, aes(x=Pollen_status, y=Reduced_Motivation, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 20) +
  scale_fill_manual(name = "Group", 
                    breaks=c("All", "Ctrl"),
                    labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, Reduced_Motivation),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2,
              show_guide=FALSE) +
  xlab("Pollen season") +
  ylab("Reduced motivation")

RA_plot <- ggplot(Subject_list_allergy, aes(x=Pollen_status, y=Reduced_Activity, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 20) +
  scale_fill_manual(name = "Group", 
                    breaks=c("All", "Ctrl"),
                    labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, Reduced_Activity),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2,
              show_guide=FALSE) +
  xlab(label = "none") +
  ylab("Reduced activity")

fatigue_plot <- plot_grid(
  GF_plot + theme(legend.position="none", axis.title.x = element_blank()),
  PF_plot + theme(legend.position="none", axis.title.x = element_blank()),
  MF_plot + theme(legend.position="none", axis.title.x = element_blank()),
  RM_plot + theme(legend.position="none", axis.title.x = element_blank()),
  RA_plot + theme(legend.position="none", axis.title.x = element_blank()),
  align = 'vh',
  hjust = -1,
  nrow = 1
)

# Add legend
legend_b <- get_legend(GF_plot + theme(legend.position="bottom"))
plot_grid(fatigue_plot, legend_b, ncol = 1, rel_heights = c(1, .05))



summariseFatigueRow <- function(measurevar) {
  
  f <- reformulate("Group + Pollen_status + Group*Pollen_status", measurevar)
  model <- lme(f, data = Subject_list_allergy,
               random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate <- intervals(model, which = "fixed")
  RoundEstimates <- round(estimate$fixed, digits = 2)
  pval <- anova(model, type = "marginal")
  
  result <- c(paste(RoundEstimates[1,2], " (", RoundEstimates[1,1], "-",  RoundEstimates[1,3], ")",
                    sep = ""),
              paste(RoundEstimates[2,2], " (", RoundEstimates[2,1], "-",  RoundEstimates[2,3], ")",
                    sep = ""),
              paste(round(pval[2,4], digits = 3), sep = ""),
              paste(RoundEstimates[3,2], " (", RoundEstimates[3,1], "-",  RoundEstimates[3,3], ")",
                    sep = ""),
              paste(round(pval[3,4], digits = 3), sep = ""),
              paste(RoundEstimates[4,2], " (", RoundEstimates[4,1], "-",  RoundEstimates[4,3], ")",
                    sep = ""),
              paste(round(pval[4,4], digits = 3), sep = ""))
  
  return(result)
}

XGeneral_fatigue <- summariseFatigueRow("General_Fatigue")
XPhysical_fatigue <- summariseFatigueRow("Physical_Fatigue")
XMental_Fatigue <- summariseFatigueRow("Mental_Fatigue")
XReduced_Motivation <- summariseFatigueRow("Reduced_Motivation")
XReduced_Activity <- summariseFatigueRow("Reduced_Activity")

# Make table with fatigue effects
viewHtmlTable(htmlTable(
  x        = rbind(c("Intercept", "Group (allergic vs controls)", " ", "Season (in vs out)", 
                     " ", "Group*season", " "),
                   c("Estimate + CI", "Estimate + CI", "p", "Estimate + CI", 
                     "p", "Estimate + CI", "p"),
                   XGeneral_fatigue, XPhysical_fatigue, XMental_Fatigue, 
                   XReduced_Motivation, XReduced_Activity
  ),
  caption  = paste("Table 3. Fatigue"),
  label    = "Table3",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "General fatigue", "Physical fatigue", "Mental fatigue", 
             "Reduced motivation", "Reduced activity"),
  rgroup   = c("", "MFI-20"),
  n.rgroup = c(2, 5),
  ctable   = TRUE,
))



# Test effect of PET on fatigue
load("PET_VT_93_2TCM_3exp.RData")

Data_MFI_PET <- merge(Data_93, MFI, by.x = c("Subject", "Pollen_status"), by.y = c("Subject", "Pollen_season"))

# General fatigue

ggplot(Data_MFI_PET, aes(x= GM, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("General fatigue vs PBR-28 PET")

lme_GF_PET <- lme(General_Fatigue ~ GM + Group*Pollen_status, data = Data_MFI_PET,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_GF_PET, type = "marginal")
intervals(lme_GF_PET, which = "fixed")

# Physical fatigue
ggplot(Data_MFI_PET, aes(x= GM, y= Physical_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("Physical fatigue vs PBR-28 PET")

lme_PF_PET <- lme(Physical_Fatigue ~ GM + Group*Pollen_status, data = Data_MFI_PET,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_PF_PET, type = "marginal")
intervals(lme_PF_PET, which = "fixed")

# Mental fatigue
ggplot(Data_MFI_PET, aes(x= GM, y= Mental_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("Mental fatigue vs PBR-28 PET")

lme_MF_PET <- lme(Mental_Fatigue ~ GM + Group*Pollen_status, data = Data_MFI_PET,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_MF_PET, type = "marginal")
intervals(lme_MF_PET)

# Reduced motivation
ggplot(Data_MFI_PET, aes(x= GM, y= Reduced_Motivation, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("Reduced motivation vs PBR-28 PET")

lme_RM_PET <- lme(Reduced_Motivation ~ GM + Group*Pollen_status, data = Data_MFI_PET,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_RM_PET, type = "marginal")
intervals(lme_RM_PET, which = "fixed")

# Reduced activity
ggplot(Data_MFI_PET, aes(x= GM, y= Reduced_Activity, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("Reduced motivation vs PBR-28 PET")

lme_RA_PET <- lme(Reduced_Activity ~ GM + Group*Pollen_status, data = Data_MFI_PET,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_RA_PET, type = "marginal")
intervals(lme_RA_PET, which = "fixed")



# Test relation between sleep and fatigue

load("MeanTST.RData")
Data_MFI_TST <- merge(Data_MFI_cytokines, MeanSleep, by = c("Subject", "Pollen_season"))

# TST - fatigue
ggplot(Data_MFI_TST, aes(x= Mean_TST, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("TST vs General fatigue")


lme_GF_TST <- lme(General_Fatigue ~ Mean_TST + Group*Pollen_season, data = Data_MFI_TST,
                  random = ~ 1|Subject, na.action = na.exclude)

anova(lme_GF_TST, type = "marginal")
intervals(lme_GF_TST)
