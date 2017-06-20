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
Data_merged <- merge(PET_data, data_cytokines, by.x = c("Subject", "Pollen_status"), 
                     by.y = c("Sample.ID", "Season"))

# Rename Group column
names(Data_merged)[names(Data_merged) == 'Group.y'] <- 'Group'


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

