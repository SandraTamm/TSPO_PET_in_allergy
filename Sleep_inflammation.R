# 
require(ggplot2)
require(nlme)

load("MeanTST.RData")
load("Data_cytokines_allergy.RData")
load("PET_VT_93_2TCM_3exp.RData")


Data_sleep_inflammation <- merge(MeanSleep, data_cytokines, 
                                 by.x = c("Subject", "Pollen_season"), by.y = c("Sample.ID", "Season"),
                                 all = T)

Data_sleep_inflammation <- merge(Data_sleep_inflammation, Data_93, by.x = c("Subject", "Pollen_season"),
                                 by.y = c("Subject", "Pollen_status"), all = T)

Data_sleep_inflammation$Pollen_season <- droplevels(Data_sleep_inflammation$Pollen_season)

contrasts(Data_sleep_inflammation$Pollen_season) <- rbind(-.5, .5)
colnames(contrasts(Data_sleep_inflammation$Pollen_season)) <- levels(Data_sleep_inflammation$Pollen_season)[2]


ggplot(Data_sleep_inflammation, aes(x = GM, y = Mean_TST)) +
  geom_point()

lme_GM_TST <- lme(GM ~ Mean_TST + Group.x*Pollen_season + Genotype, data = Data_sleep_inflammation,
               random = list(~1|Subject, ~1|Pair.x), na.action = na.exclude)

anova(lme_GM_TST, type = "marginal")
intervals(lme_GM_TST, which = "fixed")


ggplot(Data_sleep_inflammation, aes(x = TNF_concentration, y = Mean_TST)) +
  geom_point()


ggplot(Data_sleep_inflammation, aes(x = IL5_log_concentration, y = Mean_TST)) +
  geom_point()



ggplot(Data_sleep_inflammation, aes(x = IL6_log_concentration, y = Mean_TST)) +
  geom_point()


ggplot(Data_sleep_inflammation, aes(x = IFN_log_concentration, y = Mean_TST)) +
  geom_point()
