require(nlme)
require(ggplot2)
require(cowplot)
require(gdata)
require(psych)

source('Utils/SummarisingFunctions.R', chdir = T)

# ANALYSES OF SLEEP DIARY DATA

# Load demographic data
load("Demographics_allergy.RData")

# Load sleep diaries
load("data_diary_processed.RData")

Data_all_diaries <- merge(Subject_list_allergy, data_diary, by = c("Subject", "PET.date"), all.x = T)

# Add list of pairs
ListOfPairs <- read_delim("~/Desktop/RAALLPET/ListOfPairs.csv", ";", escape_double = FALSE, trim_ws = TRUE)

Data_all_diaries <- merge(Data_all_diaries, ListOfPairs)

# Relevel factors with logical ref
Data_all_diaries$Group <- relevel(Data_all_diaries$Group, ref = "Control")
Data_all_diaries$Pollen_status <- relevel(Data_all_diaries$Pollen_status, ref = "OUT")

# Add deviation coding, so that main effect represent true main effect and not simple main effect
contrasts(Data_all_diaries$Group) <- rbind(-.5, .5)
colnames(contrasts(Data_all_diaries$Group)) <- levels(Data_all_diaries$Group)[2]

contrasts(Data_all_diaries$Pollen_status) <- rbind(-.5, .5)
colnames(contrasts(Data_all_diaries$Pollen_status)) <- levels(Data_all_diaries$Pollen_status)[2]


# Plot KSS at risetime. 
SummaryKSS_rise <- summarySEwithin(Data_all_diaries, measurevar="KSS_at_risetime", 
                                   betweenvars = "Group", withinvars = "Pollen_status", na.rm = T)

KSS_plot <- ggplot(SummaryKSS_rise, aes(x=Pollen_status, y=KSS_at_risetime, colour=Group)) + 
  geom_errorbar(aes(ymin=KSS_at_risetime-ci, ymax=KSS_at_risetime+ci), width=.1) +
  ylim(1, 9) +
  geom_point(size = 2) +
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Control"), values = c("#D55E00", "#0072B2")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Morning sleepiness (KSS)") 


lme_KSS_morning <- lme(KSS_at_risetime ~ Group + Pollen_status + Group*Pollen_status, data = Data_all_diaries,
                       random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_KSS_morning, type = "marginal")
intervals(lme_KSS_morning, which = "fixed")

lme_KSS_morning_all <- lme(KSS_at_risetime ~ Pollen_status, data = subset(Data_all_diaries, Group == "Allergy"),
                           random = ~ 1|Subject, na.action = na.exclude) 

anova(lme_KSS_morning_all, type = "marginal")
intervals(lme_KSS_morning_all)

lme_KSS_morning_ctrl <- lme(KSS_at_risetime ~ Pollen_status, data = subset(Data_all_diaries, Group == "Control"),
                            random = ~ 1|Subject, na.action = na.exclude) 

anova(lme_KSS_morning_ctrl, type = "marginal")
intervals(lme_KSS_morning_ctrl)

lme_KSS_morning_out <- lme(KSS_at_risetime ~ Group, data = subset(Data_all_diaries, Pollen_status == "OUT"),
                           random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_KSS_morning_out, type = "marginal")
intervals(lme_KSS_morning_out)

# Calculate subjective sleep quality based on "Difficulties_falling_asleep" "How_was_your_sleep",
# "Restless_sleep" and "Early_wakeup"
Data_all_diaries$SleepQuality <- rowMeans(Data_all_diaries[ ,18:21], na.rm = T)


# Plot subjective sleep quality 
SummarySQ <- summarySEwithin(Data_all_diaries, measurevar="SleepQuality", 
                             betweenvars = "Group", withinvars = "Pollen_status", na.rm = T)

SQ_plot <- ggplot(SummarySQ, aes(x=Pollen_status, y=SleepQuality, colour=Group)) + 
  geom_errorbar(aes(ymin=SleepQuality-ci, ymax=SleepQuality+ci), width=.1) +
  ylim(1, 5) +
  geom_point(size = 2) +
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Control"), values = c("#D55E00", "#0072B2")) +
  theme(legend.justification=c(1,0), legend.position=c(1,0)) +
  xlab("Pollen season") +
  ylab("Rated sleep quality") 

lme_SQ <- lme(SleepQuality ~ Group + Pollen_status + Group*Pollen_status, data = Data_all_diaries,
              random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_SQ, type = "marginal")
intervals(lme_SQ, which = "fixed")

lme_SQ_all <- lme(SleepQuality ~ Pollen_status, data = subset(Data_all_diaries, Group == "Allergy"),
                  random = ~ 1|Subject, na.action = na.exclude) 

anova(lme_SQ_all, type = "marginal")
intervals(lme_SQ_all)

lme_SQ_ctrl <- lme(SleepQuality ~ Pollen_status, data = subset(Data_all_diaries, Group == "Control"),
                   random = ~ 1|Subject, na.action = na.exclude) 

anova(lme_SQ_ctrl, type = "marginal")
intervals(lme_SQ_ctrl)

lme_SQ_out <- lme(SleepQuality ~ Group, data = subset(Data_all_diaries, Pollen_status == "OUT"),
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_SQ_out, type = "marginal")
intervals(lme_SQ_out, which = "fixed")

### MyZeo
load("data_Zeo_fine.RData")

# Separate allergy data
Data_allergy <- subset(data_Zeo_fine, Pollen_status == "IN" | Pollen_status == "OUT")

# Rename groups (1 ctrl)
levels(Data_allergy$Group)[levels(Data_allergy$Group)=="Ctrl-all+RA"] <- "Ctrl"
levels(Data_allergy$Group)[levels(Data_allergy$Group)=="Ctrl-all"] <- "Ctrl"
Data_allergy$Group <- droplevels(Data_allergy$Group)

# Add pairs for random effects
Data_allergy <- merge(Data_allergy, ListOfPairs)

# Add deviation coding, so that main effect represent true main effect and not simple main effect
contrasts(Data_allergy$Group) <- rbind(-.5, .5)
colnames(contrasts(Data_allergy$Group)) <- levels(Data_allergy$Group)[2]

Data_allergy$Pollen_status <- droplevels(Data_allergy$Pollen_status)

contrasts(Data_allergy$Pollen_status) <- rbind(-.5, .5)
colnames(contrasts(Data_allergy$Pollen_status)) <- levels(Data_allergy$Pollen_status)[2]


# Relevel factors with logical ref
Data_allergy$Group <- relevel(Data_allergy$Group, ref = "Ctrl")
Data_allergy$Pollen_status <- relevel(Data_allergy$Pollen_status, ref = "OUT")


# Plot TST per group and season
Data_allergy$Pollen_status_n[Data_allergy$Pollen_status == "IN"] <- 2
Data_allergy$Pollen_status_n[Data_allergy$Pollen_status == "OUT"] <- 1

Data_allergy$scat_adj[Data_allergy$Group == "All"] <- 0.20
Data_allergy$scat_adj[Data_allergy$Group == "Ctrl"] <- -0.20

TST_plot <- ggplot(Data_allergy, aes(x=Pollen_status, y=TST, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 10) +
  scale_fill_manual(name = "Group", 
                    breaks=c("All", "Ctrl"),
                    labels=c("Allergy", "Control"), values = c("#D55E00", "#0072B2")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, TST),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2,
              show_guide=FALSE) +
  theme(legend.justification=c(0,0), legend.position=c(0, 0)) +
  xlab("Pollen season") +
  ylab("Total sleep time (hours)")


lme_TST <- lme((TST*60) ~ Group + Pollen_status + Group*Pollen_status, data = Data_allergy,
               random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_TST, type = "marginal")
intervals(lme_TST)

# Plot deep sleep % per group and season
Deepsleep_plot <- ggplot(Data_allergy, aes(x=Pollen_status, y=Deep_Sleep_perc, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(aes(Pollen_status_n + scat_adj, Deep_Sleep_perc),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=2,
              show_guide=FALSE) +
  scale_fill_manual(values = c("#D55E00", "#0072B2")) +
  theme(legend.position = "none") +
  xlab("Pollen season") +
  ylab("Percentage of deep sleep")


lme_deep_sleep_perc <- lme(Deep_Sleep_perc ~ Group + Pollen_status + Group*Pollen_status, data = Data_allergy,
                           random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_deep_sleep_perc, type = "marginal")
intervals(lme_deep_sleep_perc, which = "fixed")

lme_deep_sleep_perc_all <- lme(Deep_Sleep_perc ~ Pollen_status, data = subset(Data_allergy, Group == "All"),
                               random = ~ 1|Subject, na.action = na.exclude) 

anova(lme_deep_sleep_perc_all, type = "marginal")
intervals(lme_deep_sleep_perc_all)

lme_deep_sleep_perc_ctrl <- lme(Deep_Sleep_perc ~ Pollen_status, data = subset(Data_allergy, Group == "Ctrl"),
                                random = ~ 1|Subject, na.action = na.exclude) 

anova(lme_deep_sleep_perc_ctrl, type = "marginal")
intervals(lme_deep_sleep_perc_ctrl)

lme_deep_sleep_perc_out <- lme(Deep_Sleep_perc ~ Group, data = subset(Data_allergy, Pollen_status == "OUT"),
                               random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_deep_sleep_perc_out, type = "marginal")
intervals(lme_deep_sleep_perc_out)

grid.arrange(TST_plot, Deepsleep_plot, ncol=2,
             top=textGrob("Effect of group and pollen season on objective sleep measures",gp=gpar(fontsize=15,font=3)))

# Plot awakenings per group and season (MyZeo data)
ggplot(Data_allergy, aes(x=Pollen_status, y=Awakenings.x, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  geom_jitter(aes(Pollen_status_n + scat_adj, Awakenings.x),
              position=position_jitter(width=0.1,height=0),
              alpha=0.4,
              size=2,
              show_guide=FALSE) +
  ggtitle("Awakenings in patients and controls")


lme_awakenings <- lme(Awakenings.x ~ Group + Pollen_status + Group*Pollen_status, data = Data_allergy,
                      random = list(~1|Subject, ~1|Pair), na.action = na.exclude)

anova(lme_awakenings, type = "marginal")
intervals(lme_awakenings)


plot_grid(KSS_plot + theme(legend.position="top"),
          SQ_plot + theme(legend.position="none"),
          TST_plot + theme(legend.position="none"),
          Deepsleep_plot + theme(legend.position="none"),
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 2
)


# Calculate mean sleep per subject for other analyses
MeanSleep <- describeBy(Data_allergy$TST,list(Data_allergy$Subject, Data_allergy$Pollen_status), mat = T)
MeanSleep <- subset(MeanSleep, !is.na(MeanSleep$mean))
MeanSleep <- subset(MeanSleep, select = c(group1, group2, mean))
MeanSleep$group1 <- as.integer(as.character(MeanSleep$group1))
names(MeanSleep)[names(MeanSleep) == 'group1'] <- 'Subject'
names(MeanSleep)[names(MeanSleep) == 'group2'] <- 'Pollen_season'
names(MeanSleep)[names(MeanSleep) == 'mean'] <- 'Mean_TST'

save(MeanSleep, file = "MeanTST.RData")