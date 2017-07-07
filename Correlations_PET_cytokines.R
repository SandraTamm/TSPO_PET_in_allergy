require(ggplot2)
require(nlme)

setwd("~/Desktop/RAALLPET")

# Read data
load("PET_VT_93_2TCM_3exp.RData")
load("Data_cytokines_allergy.RData")

PET_data <- Data_93

# Rename season to merge
levels(PET_data$Pollen_status)[levels(PET_data$Pollen_status)=="Out_season"] <- "OUT"
levels(PET_data$Pollen_status)[levels(PET_data$Pollen_status)=="In_season"] <- "IN"

# Merge data
Data_merged <- merge(PET_data, data_cytokines, by.x = c("Subject", "Pollen_status", "Pair"), 
                     by.y = c("Sample.ID", "Season", "Pair"), all.y = T)

# Rename Group column
names(Data_merged)[names(Data_merged) == 'Group.y'] <- 'Group'


# Read fatigue data
MFI <- read.xls("MFI20_170404.xlsx")

Data_merged <- merge(MFI, Data_merged, by.x = c("Subject", "Pollen_season"), 
                            by.y = c("Subject", "Pollen_status"))


Data_merged$Pollen_season <- droplevels(Data_merged$Pollen_season)

# Read sleep data
load("MeanTST.RData")
MeanSleep$Pollen_season <- droplevels(MeanSleep$Pollen_season)

Data_merged <- merge(MeanSleep, Data_merged, all.y = T)

# Add deviation coding, so that main effect represent true main effect and not simple main effect
contrasts(Data_merged$Pollen_season) <- rbind(-.5, .5)
colnames(contrasts(Data_merged$Pollen_season)) <- levels(Data_merged$Pollen_season)[2]



# Make function to summarise effects on cytokines

summariseRow <- function(measurevar) {
  
  f_GM <- reformulate(paste(measurevar, "+ Group*Pollen_season + Genotype"), "GM")
  model_GM <- lme(f_GM, data = Data_merged,
               random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate_GM <- intervals(model_GM, which = "fixed")
  RoundEstimates_GM <- round(estimate_GM$fixed, digits = 2)
  pval_GM <- anova(model_GM, type = "marginal")
  
  f_GF <- reformulate(paste(measurevar, "+ Group*Pollen_season"), "General_Fatigue")
  model_GF <- lme(f_GF, data = Data_merged,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate_GF <- intervals(model_GF, which = "fixed")
  RoundEstimates_GF <- round(estimate_GF$fixed, digits = 2)
  pval_GF <- anova(model_GF, type = "marginal")
  
  f_sleep <- reformulate(paste(measurevar, "+ Group*Pollen_season"), "Mean_TST")
  model_sleep <- lme(f_sleep, data = Data_merged,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate_sleep <- intervals(model_sleep, which = "fixed")
  RoundEstimates_sleep <- round(estimate_sleep$fixed, digits = 2)
  pval_sleep <- anova(model_sleep, type = "marginal")
  
  result <- c(paste(RoundEstimates_GM[2,2], " (", RoundEstimates_GM[2,1], "-",  RoundEstimates_GM[2,3], ")",
                    sep = ""),
              paste(round(pval_GM[2,4], digits = 3), sep = ""),
              paste(RoundEstimates_GF[2,2], " (", RoundEstimates_GF[2,1], "-",  RoundEstimates_GF[2,3], ")",
                    sep = ""),
              paste(round(pval_GF[2,4], digits = 3), sep = ""),
              paste(RoundEstimates_sleep[2,2], " (", RoundEstimates_sleep[2,1], "-",  RoundEstimates_sleep[2,3], ")",
                    sep = ""),
              paste(round(pval_sleep[2,4], digits = 3), sep = ""))
  
  return(result)
}

TNF_row <- summariseRow("TNF_concentration")
logIL6_row <- summariseRow("IL6_log_concentration")
logIL5_row <- summariseRow("IL5_log_concentration")
logIFN_row <- summariseRow("IFN_log_concentration")
logIL8_row <- summariseRow("IL8_log_concentration")
IFN_IL5_ratio_row <- summariseRow("IFN_IL5_ratio")


# Function to show htmlTable in viewer.
viewHtmlTable <- function(htmlText) {
  tf <- tempfile(fileext = ".html")
  writeLines(htmlText, tf)
  getOption("viewer")(tf)
}


# Make table with cytokine effects
viewHtmlTable(htmlTable(
  x        = rbind(c("[11C]PBR28", " ", "General fatigue", 
                     " ", "Total sleep time", " "),
                   c("Estimate + CI", "p", "Estimate + CI", 
                     "p", "Estimate + CI", "p"),
                   TNF_row, logIL6_row, logIL5_row, logIFN_row, logIL8_row, IFN_IL5_ratio_row
  ),
  caption  = paste("Table 5. Cytokines as predictors of central inflammation, fatigue and sleep"),
  label    = "Table5",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "TNF-", "log(IL-6)", "log(IL-5", "log(IFN-)", 
             "log(IL-8)", "log(IFN-)/log(IL-5)"),
  rgroup   = c("", "Cytokines"),
  n.rgroup = c(2, 6),
  ctable   = TRUE,
))



# Plot PET vs TNF
ggplot(Data_merged, aes(x=TNF_concentration, y=GM, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("TNF vs PBR-28 PET")

lme_TNF <- lme(GM ~ TNF_concentration + Group*Pollen_status + Genotype, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_TNF, type = "marginal")
summary(lme_TNF)
intervals(lme_TNF, which = "fixed")

lme_TNF_adj <- lme(GM ~ TNF_concentration + Group*Pollen_status + Genotype + Sex, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_TNF_adj, type = "marginal")
summary(lme_TNF_adj)
intervals(lme_TNF_adj)

# Plot PET vs IL-6
ggplot(Data_merged, aes(x=IL6_log_concentration, y=GM, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("IL-6 vs PBR-28 PET")

lme_IL6 <- lme(GM ~ IL6_log_concentration + Group*Pollen_status + Genotype, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IL6, type = "marginal")
summary(lme_IL6)
intervals(lme_IL6)

lme_IL6_adj <- lme(GM ~ IL6_log_concentration + Group*Pollen_status + Genotype + Sex, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IL6_adj, type = "marginal")
summary(lme_IL6_adj)
intervals(lme_IL6_adj)

# Plot PET vs IL-5
ggplot(Data_merged, aes(x=IL5_log_concentration, y=GM, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("IL-5 vs PBR-28 PET")

lme_IL5 <- lme(GM ~ IL5_log_concentration + Group*Pollen_status + Genotype, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IL5, type = "marginal")
summary(lme_IL5)
intervals(lme_IL5)

lme_IL5_adj <- lme(GM ~ IL5_log_concentration + Group*Pollen_status + Genotype + Sex, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IL5_adj, type = "marginal")
summary(lme_IL5_adj)
intervals(lme_IL5_adj)

# Plot PET vs IFN
ggplot(Data_merged, aes(x=IFN_log_concentration, y=GM, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("IFN vs PBR-28 PET")


lme_IFN <- lme(GM ~ IFN_log_concentration + Group*Pollen_status + Genotype, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IFN, type = "marginal")
summary(lme_IFN)
intervals(lme_IFN, which = "fixed")

lme_IFN_adj <- lme(GM ~ IFN_log_concentration + Group*Pollen_status + Genotype + Sex, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IFN_adj, type = "marginal")
summary(lme_IFN_adj)
intervals(lme_IFN_adj)

# Plot PET vs IL-8
ggplot(Data_merged, aes(x=IL8_log_concentration, y=GM, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("IL-8 vs PBR-28 PET")


lme_IL8 <- lme(GM ~ IL8_log_concentration + Group*Pollen_status + Genotype, data = Data_merged,
               random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IL8, type = "marginal")
summary(lme_IL8)
intervals(lme_IL8, which = "fixed")

lme_IL8_adj <- lme(GM ~ IL8_log_concentration + Group*Pollen_status + Genotype + Sex, data = Data_merged,
                   random = ~ 1|Subject, na.action = na.exclude)

anova(lme_IL8_adj, type = "marginal")
summary(lme_IL8_adj)
intervals(lme_IL8_adj)

# Test associations between cytokines and general fatigue
MFI <- read.xls("MFI20_170404.xlsx")

Data_MFI_cytokines <- merge(MFI, data_cytokines, by.x = c("Subject", "Pollen_season"), 
                            by.y = c("Sample.ID", "Season"))

Data_MFI_cytokines$Group <- droplevels(Data_MFI_cytokines$Group)
Data_MFI_cytokines$Pollen_season <- droplevels(Data_MFI_cytokines$Pollen_season)

# TNF
ggplot(Data_MFI_cytokines, aes(x= TNF_concentration, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("TNF vs General fatigue")

lme_GF_TNF <- lme(General_Fatigue ~ TNF_concentration + Group*Pollen_season, data = Data_MFI_cytokines,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_GF_TNF, type = "marginal")
intervals(lme_GF_TNF, which = "fixed")

# IL-6
ggplot(Data_MFI_cytokines, aes(x= IL6_log_concentration, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("IL-6 vs General fatigue")

lme_GF_IL6 <- lme(General_Fatigue ~ IL6_log_concentration + Group*Pollen_season, data = Data_MFI_cytokines,
                  random = ~ 1|Subject, na.action = na.exclude)

anova(lme_GF_IL6, type = "marginal")
intervals(lme_GF_IL6)

# IL-5
ggplot(Data_MFI_cytokines, aes(x= IL5_log_concentration, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("IL-5 vs General fatigue")

lme_GF_IL5 <- lme(General_Fatigue ~ IL5_log_concentration + Group*Pollen_season, data = Data_MFI_cytokines,
                  random = ~ 1|Subject, na.action = na.exclude)

anova(lme_GF_IL5, type = "marginal")
intervals(lme_GF_IL5)

# IFN
ggplot(Data_MFI_cytokines, aes(x= IFN_log_concentration, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("IFN vs General fatigue")

lme_GF_IFN <- lme(General_Fatigue ~ IFN_log_concentration + Group*Pollen_season, data = Data_MFI_cytokines,
                  random = ~ 1|Subject, na.action = na.exclude)

anova(lme_GF_IFN, type = "marginal")
intervals(lme_GF_IFN)

# IL-8
ggplot(Data_MFI_cytokines, aes(x= IL8_log_concentration, y= General_Fatigue, color=Group, shape=Group)) +
  geom_point(size= 3) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  ggtitle("IL-8 vs General fatigue")

lme_GF_IL8 <- lme(General_Fatigue ~ IL8_log_concentration + Group*Pollen_season, data = Data_MFI_cytokines,
                  random = ~ 1|Subject, na.action = na.exclude)

anova(lme_GF_IL8, type = "marginal")
intervals(lme_GF_IL8)


