require(ggplot2)
require(Hmisc)
require(dplyr)
require(DataCombine)
require(nlme)
require(readr)
require(ggsci)
require(cowplot)
require(reshape2)
require(readxl)
require(htmlTable)

setwd("~/Desktop/RAALLPET")

load("Demographics_all.RData")

# Add pair info
ListOfPairs_all <- read_delim("~/Desktop/RAALLPET/ListOfPairs_all.csv", ";", escape_double = FALSE, trim_ws = TRUE)
ListOfPairs_all$Pair <- as.factor(ListOfPairs_all$Pair)

Subject_data <- merge(Subject_data, ListOfPairs_all, all.x = T)
Subject_data$Pair <- as.factor(Subject_data$Pair)
Subject_data <- subset(Subject_data, !is.na(Subject_data$Subject))


load("data_diary_processed.RData")
names(data_diary)[4:34] <- paste(names(data_diary[4:34]), "_diary", sep = "")



# Add pollen info
load("Demographics_allergy.RData")
Pollen_status <- Subject_list_allergy[ , c(1,6,8)]

data_diary <- merge(data_diary, Pollen_status, by = c("Subject", "PET.date"), all.x = T)

# Change factors to numerical variables
data_diary$Enough_sleep_diary <- as.integer(as.character(data_diary$Enough_sleep_diary))

data_diary$Awakenings_diary <- as.integer(as.character(data_diary$Awakenings_diary))


load("data_Zeo_processed.RData")
names(data_Zeo)[3:8] <- paste(names(data_Zeo[3:8]), "_Zeo", sep = "")
names(data_Zeo)[13:25] <- paste(names(data_Zeo[13:25]), "_Zeo", sep = "")
names(data_Zeo)[30:31] <- paste(names(data_Zeo[30:31]), "_Zeo", sep = "")

load("data_actigraphs.RData")
names(data_sleep_actigraphs)[12:47] <- paste(names(data_sleep_actigraphs[12:47]), "_actigraphs", sep = "")

levels(data_Zeo$Group)[levels(data_Zeo$Group)=="All"] <- "Allergy"


data_Zeo_diary <- merge(data_diary, data_Zeo, by.x = c("Subject", "Date_start_diary", "Group", "Age", "Genotype", "Sex"), 
                        by.y = c("Subject", "Date_start", "Group", "Age", "Genotype", "Sex.y"), all.y = T)

data_Zeo_diary$Percent_Zeo <- (data_Zeo_diary$Total_Duration/data_Zeo_diary$Time_in_bed)*100

### Piece of code to clean MyZeo data


Zeo_diary_corr <- vector()
for(i in 1:length(data_Zeo_diary$Percent_Zeo)){
  if(is.na(data_Zeo_diary$Percent_Zeo[i])){
    Zeo_diary_corr[i] <- NA
    }else if(data_Zeo_diary$Percent_Zeo[i] < 75){
    Zeo_diary_corr[i] <- "Bad"
    }else{
      Zeo_diary_corr[i] <- "Fine"
  }
}

data_Zeo_diary$Zeo_diary_corr <- as.factor(Zeo_diary_corr)


data_Zeo_fine <- subset(data_Zeo_diary, data_Zeo_diary$Quality != "Bad" & data_Zeo_diary$Quality != "Potentially_bad" &
                          data_Zeo_diary$Zeo_diary_corr != "Bad")



NROW(unique(data_Zeo_fine$Subject))

data_sleep_measures <- merge(data_diary, data_Zeo_fine, all.x = T)

# Something weird with this one

data_sleep_measures <- merge(data_sleep_measures, data_sleep_actigraphs, by.x = c("Subject", "Date_start_diary", "Group", "Age", "Genotype", "Sex"), 
                             by.y = c("Subject", "Start.Date_actigraphs", "Group", "Age", "Genotype", "Sex"), all.x = TRUE)

data_sleep_measures <- merge(data_sleep_measures, ListOfPairs_all, all.x = T)
data_sleep_measures$Pair <- as.factor(data_sleep_measures$Pair)


data_sleep_measures$Sleep.Time_actigraphs <- as.numeric(substr(as.character(data_sleep_measures$Sleep.Time), 1, 3))
data_sleep_measures$Duration_actigraphs <- as.numeric(substr(as.character(data_sleep_measures$Duration), 1, 3))

data_Zeo_fine <- merge(data_Zeo_fine, ListOfPairs_all, all.x = T)
data_Zeo_fine$Pair <- as.factor(data_Zeo_fine$Pair)

save(data_Zeo_fine, file = "data_Zeo_fine.RData")
write.csv2(data_Zeo_fine, file = "data_Zeo_fine.csv")

save(data_sleep_measures, file = "data_sleep_measures.RData")
write.csv2(data_sleep_measures, file = "data_sleep_measures.csv")


data_sleep_measures$Awakenings_diary <- as.numeric(data_sleep_measures$Awakenings_diary)

# Calculate subjective sleep quality based on "Difficulties_falling_asleep" "How_was_your_sleep",
# "Restless_sleep" and "Early_wakeup"
data_sleep_measures$SleepQuality <- rowMeans(data_sleep_measures[ ,16:19], na.rm = T)




# Make data frames for RA and allergy respectively

RA_data <- droplevels(grepl.sub(data_sleep_measures, Var = "Group", pattern = "RA"))
levels(RA_data$Group)[levels(RA_data$Group)=="Ctrl-all+RA"] <- "Ctrl-RA"

Data_allergy <- droplevels(grepl.sub(data_sleep_measures, Var = "Group", pattern = "all", ignore.case = T))
levels(Data_allergy$Group)[levels(Data_allergy$Group)=="Ctrl-all+RA"] <- "Ctrl-all"

# Add pairs for random effects (allergy)
ListOfPairs <- read_delim("~/Desktop/RAALLPET/ListOfPairs.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Data_allergy <- merge(Data_allergy, ListOfPairs)

# Relevel factors with logical ref
Data_allergy$Group <- relevel(Data_allergy$Group, ref = "Ctrl-all")
Data_allergy$Pollen_status <- relevel(Data_allergy$Pollen_status, ref = "OUT")

# Add deviation coding, so that main effect represent true main effect and not simple main effect
# contrasts(Data_allergy$Group) <- rbind(-.5, .5)
# colnames(contrasts(Data_allergy$Group)) <- levels(Data_allergy$Group)[2]
#   
# contrasts(Data_allergy$Pollen_status) <- rbind(-.5, .5)
# colnames(contrasts(Data_allergy$Pollen_status)) <- levels(Data_allergy$Pollen_status)[2]


# Plots for associations between sleep variables

levels(data_sleep_measures$Group)[levels(data_sleep_measures$Group) == "Ctrl-all"] <- "Control"
levels(data_sleep_measures$Group)[levels(data_sleep_measures$Group) == "Ctrl-RA"] <- "Control"
levels(data_sleep_measures$Group)[levels(data_sleep_measures$Group) == "Ctrl-all+RA"] <- "Control"
data_sleep_measures$Group <- droplevels(data_sleep_measures$Group)

data_sleep_measures$Group <- relevel(data_sleep_measures$Group, ref = "Control")


data_sleep_measures$TST_Zeo <- data_sleep_measures$TST_Zeo*60

TST_Zeo_Diary <- ggplot(data_sleep_measures, aes(x = TST_Zeo, y = Total_sleep_diary)) +
  geom_point() +
  ylim(0,800)+
  xlim(0,800)+
  xlab("Total sleep time, measured with single electrode EEG (min)") +
  ylab("Total self-reported sleep time (min)") +
  theme_minimal(base_size = 10) +
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_TST_Zeo_Diary <- lme(Total_sleep_diary ~ TST_Zeo + Group, data = data_sleep_measures,
                  random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

summary(lme_TST_Zeo_Diary)
intervals(lme_TST_Zeo_Diary, which = "fixed")


TST_diary_actigraphs <- ggplot(data_sleep_measures, aes(x = Total_sleep_diary, y = Sleep.Time_actigraphs)) +
  geom_point() +
  ylim(0,800)+
  xlim(0,800)+
  xlab("Total self-reported sleep time (min)") +
  ylab("Total sleep time, measured with actigraphy (min)") +
  theme_minimal(base_size = 10)+
  geom_smooth(method='lm',formula=y~x, col = "red")

lme_TST_Actigraphy_Diary <- lme(Sleep.Time_actigraphs ~ Total_sleep_diary + Group, data = data_sleep_measures,
                              random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

summary(lme_TST_Actigraphy_Diary)
intervals(lme_TST_Actigraphy_Diary, which = "fixed")


TST_Zeo_actigraphs <- ggplot(data_sleep_measures, aes(x = TST_Zeo, y = Sleep.Time_actigraphs)) +
  geom_point() +
  ylim(0,800)+
  xlim(0,800)+
  xlab("Total sleep time, measured with single electrode EEG (min)") +
  ylab("Total sleep time, measured with actigraphy (min)") +
  theme_minimal(base_size = 10)+
  geom_smooth(method='lm',formula=y~x, col = "red")

lme_TST_Actigraphy_Zeo <- lme(Sleep.Time_actigraphs ~ TST_Zeo + Group, data = data_sleep_measures,
                              random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

summary(lme_TST_Actigraphy_Zeo)
intervals(lme_TST_Actigraphy_Zeo, which = "fixed")


Awakenings_Zeo_diary <- ggplot(data_sleep_measures, aes(x = Awakenings_diary, y = Awakenings.x_Zeo)) +
  geom_jitter(width = 0.1, height = 0.1) +
  xlab("Awakenings, self-reported (diaries)") +
  ylab("Awakenings, measured with single electrode EEG") +
  theme_minimal(base_size = 10)+
  geom_smooth(method='lm',formula=y~x, col = "red")

lme_awakenings_Zeo_Diary <- lme(Awakenings.x_Zeo ~ Awakenings_diary + Group, data = data_sleep_measures,
                              random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

summary(lme_awakenings_Zeo_Diary)
intervals(lme_awakenings_Zeo_Diary, which = "fixed")


plot_grid(TST_Zeo_Diary,
          TST_diary_actigraphs,
          TST_Zeo_actigraphs,
          Awakenings_Zeo_diary,
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)


# Count no of observations

# No of available nights with self-reported sleep time
length(data_sleep_measures$Total_sleep_diary) - sum(is.na(data_sleep_measures$Total_sleep_diary))
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$Total_sleep_diary)), 
              Group == "Allergy")$Subject)
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$Total_sleep_diary)), 
              Group == "RA")$Subject)
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$Total_sleep_diary)), 
              Group == "Control")$Subject)


# No of available nights with MyZeo sleep time
length(data_sleep_measures$TST_Zeo) - sum(is.na(data_sleep_measures$TST_Zeo))
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$TST_Zeo)), 
              Group == "Allergy")$Subject)
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$TST_Zeo)), 
              Group == "RA")$Subject)
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$TST_Zeo)), 
              Group == "Control")$Subject)


# No of available nights with actigraphy sleep time
length(data_sleep_measures$Sleep.Time_actigraphs) - sum(is.na(data_sleep_measures$Sleep.Time_actigraphs))
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$Sleep.Time_actigraphs)), 
              Group == "Allergy")$Subject)
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$Sleep.Time_actigraphs)), 
              Group == "RA")$Subject)
unique(subset(subset(data_sleep_measures, !is.na(data_sleep_measures$Sleep.Time_actigraphs)), 
              Group == "Control")$Subject)


# Plot subjective sleep quality
data_diary$SleepQuality <- rowMeans(data_diary[ ,11:14], na.rm = T)

data_diary <- merge(data_diary, ListOfPairs_all)


# Make data frames for RA and allergy respectively

RA_diary <- droplevels(grepl.sub(data_diary, Var = "Group", pattern = "RA"))
levels(RA_diary$Group)[levels(RA_diary$Group)=="Ctrl-all+RA"] <- "Ctrl-RA"

Allergy_diary <- droplevels(grepl.sub(data_diary, Var = "Group", pattern = "all", ignore.case = T))
levels(Allergy_diary$Group)[levels(Allergy_diary$Group)=="Ctrl-all+RA"] <- "Ctrl-all"


# Relevel factors with logical ref
Allergy_diary$Group <- relevel(Allergy_diary$Group, ref = "Ctrl-all")
Allergy_diary$Pollen_status <- relevel(Allergy_diary$Pollen_status, ref = "OUT")

# Add deviation coding, so that main effect represent true main effect and not simple main effect
#contrasts(Allergy_diary$Group) <- rbind(-.5, .5)
#colnames(contrasts(Allergy_diary$Group)) <- levels(Allergy_diary$Group)[2]

#contrasts(Allergy_diary$Pollen_status) <- rbind(-.5, .5)
#colnames(contrasts(Allergy_diary$Pollen_status)) <- levels(Allergy_diary$Pollen_status)[2]



#RA, subj sleep quality

RA_SQ <- ggplot(RA_diary, aes(x=Group, y=SleepQuality, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  ylim(0, 6)+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  geom_jitter(aes(Group, SleepQuality),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=0.5) +
  xlab("Group") +
  ylab("Subjective sleep quality (diaries)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 



# Relevel factors with logical ref
Allergy_diary$Group <- relevel(Allergy_diary$Group, ref = "Ctrl-all")
Allergy_diary$Pollen_status <- relevel(Allergy_diary$Pollen_status, ref = "OUT")


# Add scatter adjustment
Allergy_diary$Pollen_status_n[Allergy_diary$Pollen_status == "IN"] <- 2
Allergy_diary$Pollen_status_n[Allergy_diary$Pollen_status == "OUT"] <- 1

Allergy_diary$scat_adj[Allergy_diary$Group == "Allergy"] <- 0.20
Allergy_diary$scat_adj[Allergy_diary$Group == "Ctrl-all"] <- -0.20


  
# All: Plot SQ per group and season 

All_SQ <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$SleepQuality)), aes(x=Pollen_status, y=SleepQuality, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  ylim(0, 6) +
  scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, SleepQuality),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=0.5) +
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Subjective sleep quality (diaries)")



# Total sleep time in RA/controls/Allergy
# Sleep diaries



RA_diary_plot <- ggplot(RA_diary, aes(x=Group, y=Total_sleep_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  ylim(0, 750)+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  geom_jitter(aes(Group, Total_sleep_diary),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=0.5) +
  xlab("Group") +
  ylab("Total sleep time (diaries)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 




# Plot TST per group and season


All_diary <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Total_sleep_diary)), aes(x=Pollen_status, y=Total_sleep_diary, fill = Group)) +
geom_boxplot(outlier.size = 0, alpha = 0.8) +
ylim(0, 750) +
scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
geom_jitter(aes(Pollen_status_n + scat_adj, Total_sleep_diary),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=0.5,
              show.guide =F) +
theme_minimal(base_size = 7)+
theme(legend.position="top") +
xlab("Pollen season") +
ylab("Total sleep time (diaries)")



# My Zeo

data_Zeo_fine$TST_Zeo <- data_Zeo_fine$TST_Zeo*60

# Make data frames for RA and allergy respectively

RA_Zeo <- droplevels(grepl.sub(data_Zeo_fine, Var = "Group", pattern = "RA"))
levels(RA_Zeo$Group)[levels(RA_Zeo$Group)=="Ctrl-all+RA"] <- "Ctrl-RA"

Allergy_Zeo <- droplevels(grepl.sub(data_Zeo_fine, Var = "Group", pattern = "all", ignore.case = T))
levels(Allergy_Zeo$Group)[levels(Allergy_Zeo$Group)=="Ctrl-all+RA"] <- "Ctrl-all"


# Relevel factors with logical ref
Allergy_Zeo$Group <- relevel(Allergy_Zeo$Group, ref = "Ctrl-all")
names(Allergy_Zeo)[45] <- "Pollen_status"
Allergy_Zeo$Pollen_status <- relevel(Allergy_Zeo$Pollen_status, ref = "OUT")

# Add deviation coding, so that main effect represent true main effect and not simple main effect
#contrasts(Allergy_Zeo$Group) <- rbind(-.5, .5)
#colnames(contrasts(Allergy_Zeo$Group)) <- levels(Allergy_Zeo$Group)[2]

#contrasts(Allergy_Zeo$Pollen_status) <- rbind(-.5, .5)
#colnames(contrasts(Allergy_Zeo$Pollen_status)) <- levels(Allergy_Zeo$Pollen_status)[2]

#RA

RA_Zeo_plot <- ggplot(RA_Zeo, aes(x=Group, y=TST_Zeo, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  ylim(0, 750)+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  geom_jitter(aes(Group, TST_Zeo),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=0.5) +
  xlab("Group") +
  ylab("Total sleep time (MyZeo)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 


# All: Plot TST per group and season 

# Add scatter adjustment
Allergy_Zeo$Pollen_status_n[Allergy_Zeo$Pollen_status == "IN"] <- 2
Allergy_Zeo$Pollen_status_n[Allergy_Zeo$Pollen_status == "OUT"] <- 1

Allergy_Zeo$scat_adj[Allergy_Zeo$Group == "Allergy"] <- 0.20
Allergy_Zeo$scat_adj[Allergy_Zeo$Group == "Ctrl-all"] <- -0.20



  All_Zeo <- ggplot(subset(Allergy_Zeo, !is.na(Allergy_Zeo$TST_Zeo)), aes(x=Pollen_status, y=TST_Zeo, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  ylim(0, 750) +
  scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, TST_Zeo),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=0.5) +
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Total sleep time (MyZeo)")



# Awakenings 


RA_Awakenings_diary <- ggplot(subset(RA_diary, !is.na(RA_diary$Awakenings_diary)), aes(x=Group, y=Awakenings_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Group, Awakenings_diary),
              position=position_jitter(width=0.2,height=0.1),
              alpha=0.6,
              size=1) +
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  ylab("No of awakenings (diaries)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 

# Should NA be 0? Maybe without boxplot

All_Awakenings_diary <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Awakenings_diary)), aes(x=Pollen_status, y=Awakenings_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Pollen_status_n + scat_adj, Awakenings_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=1) +
  scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("No of awakenings (diaries)")




RA_Awakenings_Zeo <- ggplot(subset(RA_Zeo, !is.na(RA_Zeo$Awakenings.x_Zeo)), aes(x=Group, y=Awakenings.x_Zeo, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  geom_jitter(aes(Group, Awakenings.x_Zeo),
              position=position_jitter(width=0.2,height=0.1),
              alpha=0.6,
              size=0.5) +
  ylab("No of awakenings (MyZeo)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 



All_Awakenings_Zeo <- ggplot(subset(Allergy_Zeo, !is.na(Allergy_Zeo$Awakenings.x_Zeo)), aes(x=Pollen_status, y=Awakenings.x_Zeo, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Pollen_status_n + scat_adj, Awakenings.x_Zeo),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("No of awakenings (MyZeo)")


plot_grid(RA_SQ + theme(legend.position="top"),
          All_SQ + theme(legend.position="top"),
          RA_Zeo_plot + theme(legend.position="top"),
          All_Zeo + theme(legend.position="top"),
          RA_Awakenings_Zeo + theme(legend.position="top"),
          All_Awakenings_Zeo + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 3,
          rel_widths = c(1, 2)
)




RA_DeepSleep_abs <- ggplot(subset(RA_Zeo, !is.na(RA_Zeo$Deep_Sleep_abs_num)), aes(x=Group, y=Deep_Sleep_abs_num, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Group, Deep_Sleep_abs_num),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  ylim(0, 150)+
  scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  ylab("Deep Sleep \n(absolute values)")+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")


RA_LightSleep_abs <- ggplot(subset(RA_Zeo, !is.na(RA_Zeo$Light_Sleep_abs_num)), aes(x=Group, y=Light_Sleep_abs_num, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Group, Light_Sleep_abs_num),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  ylim(0, 450)+
  scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  ylab("Light Sleep \n(absolute values)")+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")



RA_REM_abs <- ggplot(subset(RA_Zeo, !is.na(RA_Zeo$REM_abs_num)), aes(x=Group, y=REM_abs_num, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Group, REM_abs_num),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  ylim(0, 300)+
  scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  ylab("REM Sleep \n(absolute values)")+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")



RA_TST_perc <- ggplot(subset(RA_Zeo, !is.na(RA_Zeo$Total_Sleep_perc_Zeo)), aes(x=Group, y=Total_Sleep_perc_Zeo, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Group, Total_Sleep_perc_Zeo),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  ylim(60, 100)+
  scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  ylab("TST/TIB (T%)")+
  theme_minimal(base_size = 15)+
  theme(legend.position="top")



plot_grid(RA_DeepSleep_abs+ theme(legend.position="top"),
          RA_LightSleep_abs + theme(legend.position="top"),
          RA_REM_abs + theme(legend.position="top"),
          RA_TST_perc + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)

# Analyse sleep in allergy
All_DeepSleep_abs <- ggplot(subset(Allergy_Zeo, !is.na(Allergy_Zeo$Deep_Sleep_abs_num)), aes(x=Pollen_status, y=Deep_Sleep_abs_num, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Pollen_status_n + scat_adj, Deep_Sleep_abs_num),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=1) +
  scale_fill_npg() +
  theme_minimal(base_size = 15)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Deep Sleep \n(absolute values)")



All_LightSleep_abs <- ggplot(subset(Allergy_Zeo, !is.na(Allergy_Zeo$Light_Sleep_abs_num)), aes(x=Pollen_status, y=Light_Sleep_abs_num, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Pollen_status_n + scat_adj, Light_Sleep_abs_num),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=1) +
  scale_fill_npg() +
  theme_minimal(base_size = 15)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Light Sleep \n(absolute values)")



All_REM_abs <- ggplot(subset(Allergy_Zeo, !is.na(Allergy_Zeo$REM_abs_num)), aes(x=Pollen_status, y=REM_abs_num, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Pollen_status_n + scat_adj, REM_abs_num),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=1) +
  scale_fill_npg() +
  theme_minimal(base_size = 15)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("REM Sleep \n(absolute values)")



All_TST_perc <- ggplot(subset(Allergy_Zeo, !is.na(Allergy_Zeo$Total_Sleep_perc_Zeo)), aes(x=Pollen_status, y=Total_Sleep_perc_Zeo, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8)+
  geom_jitter(aes(Pollen_status_n + scat_adj, Total_Sleep_perc_Zeo),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=1) +
  scale_fill_npg() +
  theme_minimal(base_size = 15)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("TST/TIB (T%)")


plot_grid(All_DeepSleep_abs+ theme(legend.position="top"),
          All_LightSleep_abs + theme(legend.position="top"),
          All_REM_abs + theme(legend.position="top"),
          All_TST_perc + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)


# Sleepiness

RA_morning_sleepiness <- ggplot(RA_data, aes(x=Group, y=KSS_at_risetime_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  geom_jitter(aes(Group, KSS_at_risetime_diary),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  xlab("Group") +
  ylab("KSS at risetime")+
  theme_minimal()+
  theme(legend.position="top") 



RA_evening_sleepiness <- ggplot(RA_data, aes(x=Group, y=KSS_at_bedtime_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"), 
                    name = "Group", 
                    breaks=c("Ctrl-RA", "RA"),
                    labels=c("Control", "RA"))+
  geom_jitter(aes(Group, KSS_at_bedtime_diary),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  xlab("Group") +
  ylab("KSS at bedtime")+
  theme_minimal()+
  theme(legend.position="top") 



# All: Plot morning sleepiness per group and season 

All_morning_sleepiness <- ggplot(subset(Data_allergy, !is.na(Data_allergy$KSS_at_risetime_diary)), aes(x=Pollen_status, y=KSS_at_risetime_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, KSS_at_risetime_diary),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=1) +
  theme_minimal()+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("KSS at risetime")



All_evening_sleepiness <- ggplot(subset(Data_allergy, !is.na(Data_allergy$KSS_at_bedtime_diary)), aes(x=Pollen_status, y=KSS_at_bedtime_diary, fill = Group)) +
  geom_boxplot(outlier.size = 0, alpha = 0.8) +
  scale_fill_npg(name = "Group", 
                 breaks=c("Ctrl-all", "Allergy"),
                 labels=c("Control", "Allergy")) +
  geom_jitter(aes(Pollen_status_n + scat_adj, KSS_at_bedtime_diary),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=1) +
  theme_minimal()+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("KSS at bedtime")



plot_grid(RA_morning_sleepiness+ theme(legend.position="top"),
          All_morning_sleepiness + theme(legend.position="top"),
          RA_evening_sleepiness + theme(legend.position="top"),
          All_evening_sleepiness + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 2,
          rel_widths = c(1, 2)
)



# Relation to Sickness Q

Sickness_Q <- read_excel("~/Desktop/RAALLPET/Sickness_Q.xlsx")
Sickness_Q <- Sickness_Q[ ,c(1:4, 15)]
Sickness_Q$Sickness_Q <- as.numeric(Sickness_Q$Sickness_Q)

# Change date variable to POSIXct format
Sickness_Q$Date_Sickness <- as.Date(paste("20", substr(as.character(Sickness_Q$Date_Sickness), 1,2), "-", 
                                 substr(as.character(Sickness_Q$Date_Sickness), 3,4), "-", 
                                 substr(as.character(Sickness_Q$Date_Sickness), 5,6), 
                                 sep = ""))


Sickness_Q <- merge(Subject_data, Sickness_Q, all.y = T)

# Make data frames for RA and allergy respectively

RA_sickness <- droplevels(grepl.sub(Sickness_Q, Var = "Group", pattern = "RA"))
levels(RA_sickness$Group)[levels(RA_sickness$Group)=="Ctrl-all+RA"] <- "Ctrl-RA"

Allergy_sickness <- droplevels(grepl.sub(Sickness_Q, Var = "Group", pattern = "all", ignore.case = T))
levels(Allergy_sickness$Group)[levels(Allergy_sickness$Group)=="Ctrl-all+RA"] <- "Ctrl-all"

# Add pairs for random effects (allergy)
Allergy_sickness <- merge(Allergy_sickness, ListOfPairs)

# Relevel factors with logical ref
Allergy_sickness$Group <- relevel(Allergy_sickness$Group, ref = "Ctrl-all")
Allergy_sickness$Pollen_status <- as.factor(Allergy_sickness$`Pollen season`)
Allergy_sickness$Pollen_status <- relevel(Allergy_sickness$Pollen_status, ref = "out")

# Add deviation coding, so that main effect represent true main effect and not simple main effect
#contrasts(Allergy_sickness$Group) <- rbind(-.5, .5)
#colnames(contrasts(Allergy_sickness$Group)) <- levels(Allergy_sickness$Group)[2]


#contrasts(Allergy_sickness$Pollen_status) <- rbind(-.5, .5)
#colnames(contrasts(Allergy_sickness$Pollen_status)) <- levels(Allergy_sickness$Pollen_status)[2]


# Sickness Q per group



RA_SQ <- ggplot(RA_sickness, aes(x=Group, y=Sickness_Q, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 30) +
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  geom_jitter(aes(Group, Sickness_Q),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=1) +
  xlab("Group") +
  ylab("Sickness_Q (10)")+
  theme_minimal()+
  theme(legend.position="top") 

Lme_SQ_RA <- lme(Sickness_Q ~ Group, data = RA_sickness,
                     random = ~ 1|Subject, na.action = na.exclude)
summary(Lme_SQ_RA)
anova(Lme_SQ_RA, type = "marginal")
intervals(Lme_SQ_RA)



# Plot Sickness per group and season
Allergy_sickness$Pollen_status_n[Allergy_sickness$Pollen_status == "in"] <- 2
Allergy_sickness$Pollen_status_n[Allergy_sickness$Pollen_status == "out"] <- 1

Allergy_sickness$scat_adj[Allergy_sickness$Group == "Allergy"] <- 0.20
Allergy_sickness$scat_adj[Allergy_sickness$Group == "Ctrl-all"] <- -0.20

All_SQ <- ggplot(Allergy_sickness, aes(x=Pollen_status, y=Sickness_Q, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  ylim(0, 30) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Sickness_Q),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=1,
              show.guide =FALSE) +
  theme_minimal()+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Sickness_Q (10)")


lme_SQ_all <- lme(Sickness_Q ~ Group + Pollen_status + Group*Pollen_status, data = Allergy_sickness,
                         random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_SQ_all, type = "marginal")
summary(lme_SQ_all)
intervals(lme_SQ_all, which = "fixed")


plot_grid(RA_SQ + theme(legend.position="top"),
          All_SQ + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 1,
          rel_widths = c(1, 2)
)




# Get cytokine data and test relation to sleep measures

load("~/Desktop/RAALLPET/Data_cytokines_allergy.RData")
Cytokines_allergy <- data_cytokines
load("~/Desktop/RAALLPET/RA_cytokines.RData")
Cytokines_RA <- data_cytokines



Cytokines_allergy <- subset(Cytokines_allergy, select = c("Sample.ID", "Group", "Season", "TNF_concentration", 
                                                   "TNF_log_concentration", "IL6_concentration",
                                                   "IL6_log_concentration", "IL8_concentration",
                                                   "IL8_log_concentration", "Pair"))

Cytokines_RA <- subset(Cytokines_RA, Cytokines_RA$Season !=("UT"))
Cytokines_RA <- subset(Cytokines_RA, Cytokines_RA$Season !=("IN"))
Cytokines_RA$Season <- NA

names(Cytokines_RA)[1] <- "Subject" 
names(Cytokines_allergy)[1] <- "Subject" 

Sleep_cytokines_RA <- merge(data_sleep_measures, Cytokines_RA, by.x = c("Subject", "Pollen_status"), by.y = c("Subject", "Season"), all.y = T)
Sleep_cytokines_All <- merge(data_sleep_measures, Cytokines_allergy, by.x = c("Subject", "Pollen_status"), by.y = c("Subject", "Season"), all.y = T)

Sleep_cytokines <- rbind(Sleep_cytokines_All, Sleep_cytokines_RA, row.names = NULL)


Sleep_cytokines$Group <- droplevels(as.factor(Sleep_cytokines$Group.y))
Sleep_cytokines$Pair <- as.factor(Sleep_cytokines$Pair.x)


Sleep_cytokines$Cytokine_composite <- Sleep_cytokines$TNF_log_concentration + Sleep_cytokines$IL6_log_concentration + Sleep_cytokines$IL8_log_concentration

TST_Cytokine_composite_plot <- ggplot(Sleep_cytokines, aes(x = Cytokine_composite, y = TST_Zeo)) +
  geom_point() +
  xlab("Cytokine composite (TNF-alpha, IL-6, IL-8)") +
  ylab("Total sleep time (EEG)") +
  xlim(0, 2)+
  ggtitle("Association between proinflammatory cytokines and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_Cytokine_composite_TST <- lme(TST_Zeo ~ Group*Cytokine_composite, data = Sleep_cytokines,
                  random = list(~1|Subject), na.action = na.exclude) 

anova(lme_Cytokine_composite_TST, type = "marginal")
intervals(lme_Cytokine_composite_TST, which = "fixed")


SQ_Cytokine_composite_plot <- ggplot(Sleep_cytokines, aes(x = Cytokine_composite, y = SleepQuality)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("Cytokine_composite") +
  ylab("TST Zeo") +
  xlim(0, 2)+
  ggtitle("Association between Cytokine_composite and SleepQuality")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_Cytokine_composite_SQ <- lme(SleepQuality ~ Group*Cytokine_composite, data = Sleep_cytokines,
                                  random = list(~1|Subject), na.action = na.exclude) 

anova(lme_Cytokine_composite_SQ, type = "marginal")
intervals(lme_Cytokine_composite_SQ, which = "fixed")


SWS_Cytokine_composite_plot <- ggplot(Sleep_cytokines, aes(x = Cytokine_composite, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("Cytokine_composite") +
  ylab("Deep_Sleep_abs_num") +
  xlim(0, 2)+
  ggtitle("Association between Cytokine_composite and Deep_Sleep_abs_num")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_Cytokine_composite_SWS <- lme(Deep_Sleep_abs_num ~ Group*Cytokine_composite, data = Sleep_cytokines,
                                 random = list(~1|Subject), na.action = na.exclude) 

anova(lme_Cytokine_composite_SWS, type = "marginal")
summary(lme_Cytokine_composite_SWS, which = "fixed")


plot_grid(Sickness_TST_plot+ theme(legend.position="top"),
          Sickness_SWS_plot + theme(legend.position="top"),
          Sickness_SQ_plot + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)


plot_grid(TST_Cytokine_composite_plot+ theme(legend.position="top"),
          SWS_Cytokine_composite_plot + theme(legend.position="top"),
          SQ_Cytokine_composite_plot + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)


# Load PET data

load("Data_GM_all.RData")

PET_sleep <- merge(data_sleep_measures, Data_GM_all, by = c("Subject", "PET.date"))
PET_sleep$Group <- PET_sleep$Group.y

levels(PET_sleep$Group)[levels(PET_sleep$Group)=="Ctrl-RA"] <- "Control"

PET_sleep$Group <- droplevels(PET_sleep$Group)

PET_sleep$Group <- relevel(PET_sleep$Group, ref = "Control")


TST_PET_plot <- ggplot(PET_sleep, aes(x = GM, y = TST_Zeo)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("GM") +
  ylab("TST Zeo") +
  ggtitle("Association between grey matter PBR binding and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SWS_PET_plot <- ggplot(PET_sleep, aes(x = GM, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("GM") +
  ylab("Deep_Sleep_abs_num") +
  ggtitle("Association between grey matter PBR binding and SWS")+
  geom_smooth(method='lm',formula=y~x, col = "red")

lme_PET_SWS <- lme(Deep_Sleep_abs_num ~ Group*GM, data = PET_sleep,
                                  random = list(~1|Subject), na.action = na.exclude) 

anova(lme_PET_SWS, type = "marginal")
summary(lme_PET_SWS, which = "fixed")


SQ_PET_plot <- ggplot(PET_sleep, aes(x = GM, y = SleepQuality)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("GM") +
  ylab("SleepQuality") +
  ggtitle("Association between grey matter PBR binding and sleep quality")+
  geom_smooth(method='lm',formula=y~x, col = "red")

plot_grid(TST_PET_plot + theme(legend.position="top"),
          SWS_PET_plot + theme(legend.position="top"),
          SQ_PET_plot + theme(legend.position="top"),
          align = 'vh',
          labels = c("A", "B", "C"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)


# Test if TST, deep sleep or subjective sleep quality is related to sickness


Sickness_sleep <- merge(Sickness_Q, data_sleep_measures, by.x = c("Subject", "Date_Sickness", "Pair"), 
                        by.y = c("Subject", "PET.date", "Pair"), all = T)


Sickness_sleep <- subset(Sickness_sleep, Sickness_sleep$Group.y != "Kontroll, exkluded")

levels(Sickness_sleep$Group.y)[levels(Sickness_sleep$Group.y)=="Ctrl-RA"] <- "Control"
levels(Sickness_sleep$Group.y)[levels(Sickness_sleep$Group.y)=="Ctrl-all"] <- "Control"
levels(Sickness_sleep$Group.y)[levels(Sickness_sleep$Group.y)=="Ctrl-all+RA"] <- "Control"

Sickness_sleep$Group <- droplevels(Sickness_sleep$Group.y)

Sickness_TST_plot <- ggplot(Sickness_sleep, aes(x = Sickness_Q, y = TST_Zeo)) +
  geom_point() +
  xlab("Sickness Q") +
  ylab("TST Zeo") +
  ggtitle("Association between sickness and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_SQ_TST <- lme(TST_Zeo ~ Group*Sickness_Q, data = Sickness_sleep,
                  random = list(~1|Subject), na.action = na.exclude) 

anova(lme_SQ_TST, type = "marginal")
intervals(lme_SQ_TST, which = "fixed")



Sickness_SQ_plot <- ggplot(Sickness_sleep, aes(x = Sickness_Q, y = SleepQuality)) +
  geom_point() +
  xlab("Sickness Q") +
  ylab("Sleep quality") +
  ggtitle("Association between sickness and SQ")+
  geom_smooth(method='lm',formula=y~x, col = "red")



lme_SQ_SQ <- lme(SleepQuality ~ Group*Sickness_Q, data = Sickness_sleep,
                 random = list(~1|Subject), na.action = na.exclude) 

anova(lme_SQ_SQ, type = "marginal")
intervals(lme_SQ_SQ, which = "fixed")

Sickness_SWS_plot <- ggplot(Sickness_sleep, aes(x = Sickness_Q, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group.y))) +
  xlab("Sickness Q") +
  ylab("Deep sleep") +
  ggtitle("Association between sickness and SWS")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_SQ_DeepSleep <- lme(Deep_Sleep_abs_num ~ Group*Sickness_Q, data = Sickness_sleep,
                        random = list(~1|Subject), na.action = na.exclude) 

anova(lme_SQ_DeepSleep, type = "marginal")
intervals(lme_SQ_DeepSleep, which = "fixed")




Rhinitis_symptoms <- read.csv("~/Desktop/RAALLPET/Rhinitis_symptoms_170512.csv", sep = ";")
names(Rhinitis_symptoms)[1] <- "Subject"

Rhinitis_symptoms$Rhinitis_symptoms_total <- (Rhinitis_symptoms$Runny_nose + Rhinitis_symptoms$Itching_nose +
                                                Rhinitis_symptoms$Sneezing + Rhinitis_symptoms$Nasal_congestion + 
                                                Rhinitis_symptoms$Loss_of_smell + Rhinitis_symptoms$Runny_eyes + 
                                                Rhinitis_symptoms$Itching_eyes + Rhinitis_symptoms$Eye_redness + 
                                                Rhinitis_symptoms$Swollen_eyes)


Rhinitis_symptoms$Nose <- (Rhinitis_symptoms$Runny_nose + Rhinitis_symptoms$Itching_nose +
                             Rhinitis_symptoms$Sneezing + Rhinitis_symptoms$Nasal_congestion + 
                             Rhinitis_symptoms$Loss_of_smell)

Rhinitis_symptoms$Eyes <-  (Rhinitis_symptoms$Runny_eyes + Rhinitis_symptoms$Itching_eyes + 
                              Rhinitis_symptoms$Eye_redness + Rhinitis_symptoms$Swollen_eyes)

Rhinitis_symptoms <- subset(Rhinitis_symptoms, select = c("Subject", "Date", "Rhinitis_symptoms_total", 
                                                          "Nose", "Eyes"))

Rhinitis_symptoms$Date <- as.Date(paste("20", substr(as.character(Rhinitis_symptoms$Date), 1,2), "-", 
                                          substr(as.character(Rhinitis_symptoms$Date), 3,4), "-", 
                                          substr(as.character(Rhinitis_symptoms$Date), 5,6), 
                                          sep = ""))


Sleep_rhinitis <- merge(Rhinitis_symptoms, data_sleep_measures, by = c("Subject", "Date"))
Sleep_rhinitis$Group[Sleep_rhinitis$Group == "Ctrl-all+RA"] <- "Ctrl-all"
Sleep_rhinitis$Group <- droplevels(Sleep_rhinitis$Group)

# Relevel factors
Sleep_rhinitis$Pollen_status <- relevel(Sleep_rhinitis$Pollen_status, ref = "OUT")

Rhinitis_TST_plot <- ggplot(Sleep_rhinitis, aes(x = Rhinitis_symptoms_total, y = TST_Zeo)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("Rhinitis symptoms") +
  ylab("TST Zeo") +
  ggtitle("Association between rhinitis symptoms and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_rhinitis_TST <- lme(TST_Zeo ~ Rhinitis_symptoms_total, data = subset(Sleep_rhinitis, Group == "Allergy"),
                  random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_TST, type = "marginal")
intervals(lme_rhinitis_TST, which = "fixed")


Rhinitis_SQ_plot <- ggplot(subset(Sleep_rhinitis, Group == "Allergy"), aes(x = Rhinitis_symptoms_total, y = SleepQuality)) +
  geom_point(aes(colour = factor(Pollen_status))) +
  xlab("Rhinitis symptoms") +
  ylab("Sleep quality") +
  ggtitle("Association between rhinitis symptoms and sleep quality")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_rhinitis_SQ <- lme(SleepQuality ~ Pollen_status + Rhinitis_symptoms_total, data = subset(Sleep_rhinitis, Group == "Allergy"),
                        random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_SQ, type = "marginal")
intervals(lme_rhinitis_SQ, which = "fixed")


Rhinitis_SWS_plot <- ggplot(Sleep_rhinitis, aes(x = Rhinitis_symptoms_total, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("Rhinitis symptoms") +
  ylab("Deep sleep") +
  ggtitle("Association between rhinitis symptoms and SWS")+
  geom_smooth(method='lm',formula=y~x, col = "red")



lme_rhinitis_DeepSleep_1 <- lme(Deep_Sleep_abs_num ~ Rhinitis_symptoms_total, data = subset(Sleep_rhinitis, Group == "Allergy"),
                              random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_DeepSleep_1, type = "marginal")
intervals(lme_rhinitis_DeepSleep_1, which = "fixed")


lme_rhinitis_DeepSleep <- lme(Deep_Sleep_abs_num ~ Pollen_status + Rhinitis_symptoms_total, data = subset(Sleep_rhinitis, Group == "Allergy"),
                        random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_DeepSleep, type = "marginal")
intervals(lme_rhinitis_DeepSleep, which = "fixed")


# Exploratory test rhinitis symptoms vs conjunctivitis 

lme_rhinitis_SQ_nose <- lme(SleepQuality ~ Pollen_status + Nose, data = subset(Sleep_rhinitis, Group == "Allergy"),
                       random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_SQ_nose, type = "marginal")
intervals(lme_rhinitis_SQ_nose, which = "fixed")

lme_rhinitis_SQ_eyes <- lme(SleepQuality ~ Pollen_status + Eyes, data = subset(Sleep_rhinitis, Group == "Allergy"),
                            random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_SQ_eyes, type = "marginal")
intervals(lme_rhinitis_SQ_eyes, which = "fixed")


lme_rhinitis_DeepSleep_1_nose <- lme(Deep_Sleep_abs_num ~ Nose, data = subset(Sleep_rhinitis, Group == "Allergy"),
                                random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_DeepSleep_1_nose, type = "marginal")
intervals(lme_rhinitis_DeepSleep_1_nose, which = "fixed")

lme_rhinitis_DeepSleep_1_eyes <- lme(Deep_Sleep_abs_num ~ Eyes, data = subset(Sleep_rhinitis, Group == "Allergy"),
                                     random = list(~1|Subject), na.action = na.exclude) 

anova(lme_rhinitis_DeepSleep_1_eyes, type = "marginal")
intervals(lme_rhinitis_DeepSleep_1_eyes, which = "fixed")



plot_grid(Rhinitis_TST_plot+ theme(legend.position="top"),
          Rhinitis_SQ_plot + theme(legend.position="top"),
          Rhinitis_SWS_plot + theme(legend.position="top"),
          align = 'vh',
          #labels = c("A", "B", "C"),
          hjust = -1,
          nrow = 2
          #rel_widths = c(1, 2)
)


# Make function to summarise effects on sleep measures (allergy)

summariseTableRow <- function(measurevar, DATA) {
  
  f <- reformulate("Group*Pollen_status", measurevar)
  model <- lme(f, data = DATA,
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

SQ_all_row <- summariseTableRow("SleepQuality", Allergy_diary)
TST_Zeo_all_row <- summariseTableRow("TST_Zeo", Allergy_Zeo)
TST_Diary_all_row <- summariseTableRow("Total_sleep_diary", Allergy_diary)
Awakenings_Zeo_all_row <- summariseTableRow("Awakenings.x_Zeo", Allergy_Zeo)
Awakenings_Diary_all_row <- summariseTableRow("Awakenings_diary", Allergy_diary)
Deep_sleep_all_row <- summariseTableRow("Deep_Sleep_abs_num", Allergy_Zeo)
Light_sleep_all_row <- summariseTableRow("Light_Sleep_abs_num", Data_allergy) # Needs fixing
REM_sleep_all_row <- summariseTableRow("REM_abs_num", Allergy_Zeo)
Sleep_efficiency_all_row <- summariseTableRow("Total_Sleep_perc_Zeo", Allergy_Zeo)
KSS_morning_all_row <- summariseTableRow("KSS_at_risetime_diary", Allergy_diary)
KSS_evening_all_row <- summariseTableRow("KSS_at_bedtime_diary", Allergy_diary)
Sickness_all_row <- summariseTableRow("Sickness_Q", Allergy_sickness)

# Function to show htmlTable in viewer.
viewHtmlTable <- function(htmlText) {
  tf <- tempfile(fileext = ".html")
  writeLines(htmlText, tf)
  getOption("viewer")(tf)
}


# Make table with sleep effects in allergy
viewHtmlTable(htmlTable(
  x        = rbind(c("Intercept", "Group (allergic vs controls)", " ", "Season (in vs out)", 
                     " ", "Group*season", " "),
                   c("Estimate + CI", "Estimate + CI", "p", "Estimate + CI", 
                     "p", "Estimate + CI", "p"),
                   SQ_all_row, TST_Zeo_all_row, TST_Diary_all_row, Awakenings_Zeo_all_row, 
                   Awakenings_Diary_all_row, Deep_sleep_all_row, Light_sleep_all_row,
                   REM_sleep_all_row, Sleep_efficiency_all_row, KSS_morning_all_row,
                   KSS_evening_all_row
  ),
  caption  = paste("Table 2. Sleep allergy"),
  label    = "Table2",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "Sleep quality", "Total sleep time (EEG)", "Total sleep time (Diary)", 
             "No of awakenings (EEG)", 
             "No of awakenings (diary)", "Deep sleep (abs)", "Light sleep (abs)",
             "REM sleep (abs)", "Sleep efficiency", "Morning sleepiness (KSS)", "Evening sleepiness (KSS)",
             "Sickness Q"),
  rgroup   = c("", "Sleep", "Sleepiness"),
  n.rgroup = c(2, 9, 2),
  ctable   = TRUE,
))

# Follow up t test for sign interctions
# SQ
model <- lme(SleepQuality ~ Pollen_status, data = subset(Allergy_diary, Group  == "Allergy"),
             random = list(~1|Subject), na.action = na.exclude)
intervals(model, which = "fixed")
anova(model, type = "marginal")


model <- lme(SleepQuality ~ Pollen_status, data = subset(Allergy_diary, Group  == "Ctrl-all"),
             random = list(~1|Subject), na.action = na.exclude)
intervals(model, which = "fixed")
anova(model, type = "marginal")

# Deep sleep
model <- lme(Deep_Sleep_abs_num ~ Pollen_status, data = subset(Allergy_Zeo, Group  == "Allergy"),
  random = list(~1|Subject), na.action = na.exclude)
intervals(model, which = "fixed")
anova(model, type = "marginal")


model <- lme(Deep_Sleep_abs_num ~ Pollen_status, data = subset(Allergy_Zeo, Group  == "Ctrl-all"),
             random = list(~1|Subject), na.action = na.exclude)
intervals(model, which = "fixed")
anova(model, type = "marginal")

# KSS
model <- lme(KSS_at_risetime_diary ~ Pollen_status, data = subset(Allergy_diary, Group  == "Allergy"),
             random = list(~1|Subject), na.action = na.exclude)
intervals(model, which = "fixed")
anova(model, type = "marginal")


model <- lme(KSS_at_risetime_diary ~ Pollen_status, data = subset(Allergy_diary, Group  == "Ctrl-all"),
             random = list(~1|Subject), na.action = na.exclude)
intervals(model, which = "fixed")
anova(model, type = "marginal")

# Make function to summarise effects on sleep measures (RA)

summariseTableRow_RA <- function(measurevar, DATA) {
  
  f <- reformulate("Group", measurevar)
  model <- lme(f, data = DATA,
               random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate <- intervals(model, which = "fixed")
  RoundEstimates <- round(estimate$fixed, digits = 2)
  pval <- anova(model, type = "marginal")
  
  result <- c(paste(RoundEstimates[1,2], " (", RoundEstimates[1,1], "-",  RoundEstimates[1,3], ")",
                    sep = ""),
              paste(RoundEstimates[2,2], " (", RoundEstimates[2,1], "-",  RoundEstimates[2,3], ")",
                    sep = ""),
              paste(round(pval[2,4], digits = 3), sep = ""))
  
  return(result)
}

SQ_RA_row <- summariseTableRow_RA("SleepQuality", RA_diary)
TST_Zeo_RA_row <- summariseTableRow_RA("TST_Zeo", RA_Zeo)
TST_Diary_RA_row <- summariseTableRow_RA("Total_sleep_diary", RA_diary)
Awakenings_Zeo_RA_row <- summariseTableRow_RA("Awakenings.x_Zeo", RA_Zeo)
Awakenings_Diary_RA_row <- summariseTableRow_RA("Awakenings_diary", RA_diary)
Deep_sleep_RA_row <- summariseTableRow_RA("Deep_Sleep_abs_num", RA_Zeo)
Light_sleep_RA_row <- summariseTableRow_RA("Light_Sleep_abs_num", RA_Zeo)
REM_sleep_RA_row <- summariseTableRow_RA("REM_abs_num", RA_Zeo)
Sleep_efficiency_RA_row <- summariseTableRow_RA("Total_Sleep_perc_Zeo", RA_Zeo)
KSS_morning_RA_row <- summariseTableRow_RA("KSS_at_risetime_diary", RA_diary)
KSS_evening_RA_row <- summariseTableRow_RA("KSS_at_bedtime_diary", RA_diary)
Sickness_RA_row <- summariseTableRow_RA("Sickness_Q", RA_sickness)

# Function to show htmlTable in viewer.
viewHtmlTable <- function(htmlText) {
  tf <- tempfile(fileext = ".html")
  writeLines(htmlText, tf)
  getOption("viewer")(tf)
}


# Make table with sleep effects in allergy
viewHtmlTable(htmlTable(
  x        = rbind(c("Intercept", "Group (RA vs controls)", ""),
                   c("Estimate + CI", "Estimate + CI", "p"),
                   SQ_RA_row, TST_Zeo_RA_row, TST_Diary_RA_row, Awakenings_Zeo_RA_row, 
                   Awakenings_Diary_RA_row, Deep_sleep_RA_row, Light_sleep_RA_row,
                   REM_sleep_RA_row, Sleep_efficiency_RA_row, KSS_morning_RA_row,
                   KSS_evening_RA_row
  ),
  caption  = paste("Table 3. Sleep RA"),
  label    = "Table3",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "Sleep quality", "Total sleep time (EEG)", "Total sleep time (Diary)", 
             "No of awakenings (EEG)", 
             "No of awakenings (diary)", "Deep sleep (abs)", "Light sleep (abs)",
             "REM sleep (abs)", "Sleep efficiency", "Morning sleepiness (KSS)", "Evening sleepiness (KSS)",
             "Sickness Q"),
  rgroup   = c("", "Sleep", "Sleepiness"),
  n.rgroup = c(2, 9, 2),
  ctable   = TRUE,
))






# Analyse effect of DAS on sleep


DAS28 <- read_excel("~/Desktop/RAALLPET/DAS28.xlsx")
names(DAS28)[2] <- "Subject"


Sleep_DAS28 <- merge(DAS28, data_sleep_measures, by = c("Subject"))

DAS28_TST_plot <- ggplot(Sleep_DAS28, aes(x = DAS28, y = TST_Zeo)) +
  geom_point() +
  xlab("DAS28") +
  ylab("TST Zeo") +
  ggtitle("Association between DAS28 ratings and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_DAS28_TST <- lme(TST_Zeo ~ DAS28, data = Sleep_DAS28,
                        random = list(~1|Subject), na.action = na.exclude) 

anova(lme_DAS28_TST, type = "marginal")
intervals(lme_DAS28_TST, which = "fixed")

DAS28_SQ_plot <- ggplot(Sleep_DAS28, aes(x = DAS28, y = SleepQuality)) +
  geom_point() +
  xlab("DAS28") +
  ylab("Sleep quality") +
  ggtitle("Association between DAS28 ratings and rated sleep quality")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_DAS28_SQ <- lme(SleepQuality ~ DAS28, data = Sleep_DAS28,
                     random = list(~1|Subject), na.action = na.exclude) 

anova(lme_DAS28_SQ, type = "marginal")
intervals(lme_DAS28_SQ, which = "fixed")

DAS28_SWS_plot <- ggplot(Sleep_DAS28, aes(x = DAS28, y = Deep_Sleep_abs_num)) +
  geom_point() +
  xlab("DAS28") +
  ylab("SWS Zeo") +
  ggtitle("Association between DAS28 ratings and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


lme_DAS28_SWS <- lme(Deep_Sleep_abs_num ~ DAS28, data = Sleep_DAS28,
                     random = list(~1|Subject), na.action = na.exclude) 

anova(lme_DAS28_SWS, type = "marginal")
intervals(lme_DAS28_SWS, which = "fixed")









# Detailed plot of sleep diaries

#RA

# Write function to plot diary variables
myplot <- function(plot_data = RA_diary, xvar = "Group", yvar, ylabel) {
  ggplot(plot_data, aes_string(x=xvar, y=yvar, fill = xvar)) +
    geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
    geom_jitter(aes_string(xvar, yvar),
                position=position_jitter(width=0.2,height=0.1),
                alpha=0.6,
                size=0.5) +
    scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
    scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
    ylab(ylabel)+
    theme_minimal(base_size = 7)+
    theme(legend.position="top") 
}

RA_Time_to_sleep <- ggplot(RA_diary, aes(x=Group, y=Time_to_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  geom_jitter(aes(Group, Time_to_sleep_diary),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=0.5) +
  ylim(0, 250)+
  scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  ylab("Time to sleep (diaries)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 


RA_Stress_at_bedtime <- myplot(yvar = "Stress_at_bedtime_diary", 
                               ylabel = "Did yo feel worried when going to bed? \n(1 = a lot, 5 = not at all)")



RA_Difficulties_falling_asleep <- myplot(yvar = "Difficulties_falling_asleep_diary", 
                                         ylabel = "Did yo have difficulties falling asleep? \n(1 = a lot, 5 = not at all)")



RA_How_was_your_sleep <- myplot(yvar = "How_was_your_sleep_diary", 
                                ylabel = "How was your sleep? \n(1 = very poor, 5 = very good)")



RA_Restless_sleep <- myplot(yvar = "Restless_sleep_diary", 
                            ylabel = "Restless sleep? \n(1 = very much, 5 = not at all)")




RA_Early_wakeup <- myplot(yvar = "Early_wakeup_diary", 
                          ylabel = "Did you wake up early and could not go back to sleep? \n(1 = very early, 5 = no)")




RA_Wake_time_during_sleep <- ggplot(RA_diary, aes(x=Group, y=Wake_time_during_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  geom_jitter(aes(Group, Wake_time_during_sleep_diary),
              position=position_jitter(width=0.2,height=0),
              alpha=0.6,
              size=0.5) +
  ylim(0, 50)+
  scale_color_manual(values = c("#E64B35FF", "#bcbddc"))+
  scale_fill_manual(values = c("#E64B35FF", "#bcbddc"))+
  xlab("Group") +
  ylab("How long (approx.) have you \nbeen awake during the night? (diaries)")+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") 

RA_Enough_sleep <- myplot(plot_data = subset(RA_diary, !is.na(RA_diary$Enough_sleep_diary)), yvar = "Enough_sleep_diary", 
                          ylabel = "How you slept enough? \n(1 = no, definitely not, 5 = yes, definitely)")



RA_Sleep_depth <- myplot(yvar = "Sleep_depth_diary", 
                         ylabel = "How deep (light) did you sleep? \n(1 = very light, 5 = very deep)")


RA_Easy_wakeup <- myplot(yvar = "Easy_wakeup_diary", 
                         ylabel = "Ease waking up? \n(1 = very hard, 5 = very easy)")


RA_Rested <- myplot(yvar = "Rested_diary", 
                    ylabel = "Well-rested? \n(1 = not at all, 5 = completely)")



plot_grid(RA_Time_to_sleep + theme(legend.position="none"),
          RA_Stress_at_bedtime + theme(legend.position="none"),
          RA_Difficulties_falling_asleep + theme(legend.position = "none"),
          RA_How_was_your_sleep + theme(legend.position = "none"),
          RA_Restless_sleep + theme(legend.position = "none"),
          RA_Early_wakeup + theme(legend.position = "none"),
          RA_Wake_time_during_sleep + theme(legend.position = "none"),
          RA_Enough_sleep + theme(legend.position = "none"),
          RA_Sleep_depth + theme(legend.position = "none"),
          RA_Easy_wakeup + theme(legend.position = "none"),
          RA_Rested + theme(legend.position = "none"),
          align = 'vh',
          #labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 4
          #rel_widths = c(1, 2)
)




# Allergy sleep diaries

All_Time_to_sleep <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Time_to_sleep_diary)), aes(x=Pollen_status, y=Time_to_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  ylim(0, 250) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Time_to_sleep_diary),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Time to sleep (diaries)")



All_Stress_at_bedtime <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Stress_at_bedtime_diary)), aes(x=Pollen_status, y=Stress_at_bedtime_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  ylim(0, 5) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Stress_at_bedtime_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Did yo feel worried when going to bed? \n(1 = a lot, 5 = not at all)")



All_Difficulties_falling_asleep <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Difficulties_falling_asleep_diary)), aes(x=Pollen_status, y=Difficulties_falling_asleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Difficulties_falling_asleep_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Did yo have difficulties falling asleep? \n(1 = a lot, 5 = not at all)")




All_How_was_your_sleep <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$How_was_your_sleep_diary)), aes(x=Pollen_status, y=How_was_your_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, How_was_your_sleep_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("How was your sleep? \n(1 = very poor, 5 = very good)")


All_Restless_sleep <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Restless_sleep_diary)), aes(x=Pollen_status, y=Restless_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Restless_sleep_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Restless sleep? \n(1 = very much, 5 = not at all)")

All_Early_wakeup <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Early_wakeup_diary)), aes(x=Pollen_status, y=Early_wakeup_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Early_wakeup_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Did you wake up early and could not go back to sleep? \n(1 = very early, 5 = no)")





All_Wake_time_during_sleep <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Wake_time_during_sleep_diary)), aes(x=Pollen_status, y=Wake_time_during_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Wake_time_during_sleep_diary),
              position=position_jitter(width=0.1,height=0),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("How long (approx.) have you \nbeen awake during the night? (diaries)")



All_Enough_sleep <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Enough_sleep_diary)), aes(x=Pollen_status, y=Enough_sleep_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Enough_sleep_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("How you slept enough? \n(1 = no, definitely not, 5 = yes, definitely)")


All_Sleep_depth <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Sleep_depth_diary)), aes(x=Pollen_status, y=Sleep_depth_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Sleep_depth_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("How deep (light) did you sleep? \n(1 = very light, 5 = very deep)")


All_Easy_wakeup <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Easy_wakeup_diary)), aes(x=Pollen_status, y=Easy_wakeup_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Easy_wakeup_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Ease waking up? \n(1 = very hard, 5 = very easy)")


All_Rested <- ggplot(subset(Allergy_diary, !is.na(Allergy_diary$Rested_diary)), aes(x=Pollen_status, y=Rested_diary, fill = Group)) +
  geom_boxplot(aes(color = Group), alpha = 0.3, outlier.size = 0) +
  scale_fill_npg() +
  geom_jitter(aes(Pollen_status_n + scat_adj, Rested_diary),
              position=position_jitter(width=0.1,height=0.1),
              alpha=0.6,
              size=0.5) +
  scale_color_npg()+
  theme_minimal(base_size = 7)+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("Well-rested? \n(1 = not at all, 5 = completely)")



plot_grid(All_Time_to_sleep + theme(legend.position="none"),
          All_Stress_at_bedtime + theme(legend.position="none"),
          All_Difficulties_falling_asleep + theme(legend.position = "none"),
          All_How_was_your_sleep + theme(legend.position = "none"),
          All_Restless_sleep + theme(legend.position = "none"),
          All_Early_wakeup + theme(legend.position = "none"),
          All_Wake_time_during_sleep + theme(legend.position = "none"),
          All_Enough_sleep + theme(legend.position = "none"),
          All_Sleep_depth + theme(legend.position = "none"),
          All_Easy_wakeup + theme(legend.position = "none"),
          All_Rested + theme(legend.position = "none"),
          align = 'vh',
          #labels = c("A", "B", "C", "D"),
          hjust = -1,
          nrow = 4
          #rel_widths = c(1, 2)
)


# Analyze individual cytokines


TST_TNF_plot <- ggplot(Sleep_cytokines, aes(x = TNF_log_concentration, y = TST_Zeo)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(TNF)") +
  ylab("TST Zeo") +
  ggtitle("Association between TNF and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SQ_TNF_plot <- ggplot(Sleep_cytokines, aes(x = TNF_log_concentration, y = SleepQuality)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(TNF)") +
  ylab("TST Zeo") +
  ggtitle("Association between TNF and SleepQuality")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SWS_TNF_plot <- ggplot(Sleep_cytokines, aes(x = TNF_log_concentration, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(TNF)") +
  ylab("Deep_Sleep_abs_num") +
  ggtitle("Association between TNF and Deep_Sleep_abs_num")+
  geom_smooth(method='lm',formula=y~x, col = "red")


TST_IL6_plot <- ggplot(Sleep_cytokines, aes(x = IL6_log_concentration, y = TST_Zeo)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(IL6)") +
  ylab("TST Zeo") +
  ggtitle("Association between IL6 and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SQ_IL6_plot <- ggplot(Sleep_cytokines, aes(x = IL6_log_concentration, y = SleepQuality)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(IL6)") +
  ylab("TST Zeo") +
  ggtitle("Association between IL6 and SleepQuality")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SWS_IL6_plot <- ggplot(Sleep_cytokines, aes(x = IL6_log_concentration, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(IL6)") +
  ylab("Deep_Sleep_abs_num") +
  ggtitle("Association between IL6 and Deep_Sleep_abs_num")+
  geom_smooth(method='lm',formula=y~x, col = "red")


TST_IL8_plot <- ggplot(Sleep_cytokines, aes(x = IL8_log_concentration, y = TST_Zeo)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(IL8)") +
  ylab("TST Zeo") +
  ggtitle("Association between IL8 and TST")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SQ_IL8_plot <- ggplot(Sleep_cytokines, aes(x = IL8_log_concentration, y = SleepQuality)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(IL8)") +
  ylab("TST Zeo") +
  ggtitle("Association between IL8 and SleepQuality")+
  geom_smooth(method='lm',formula=y~x, col = "red")


SWS_IL8_plot <- ggplot(Sleep_cytokines, aes(x = IL8_log_concentration, y = Deep_Sleep_abs_num)) +
  geom_point(aes(colour = factor(Group))) +
  xlab("log(IL8)") +
  ylab("Deep_Sleep_abs_num") +
  ggtitle("Association between IL8 and Deep_Sleep_abs_num")+
  geom_smooth(method='lm',formula=y~x, col = "red")



plot_grid(TST_TNF_plot+ theme(legend.position="top"),
          SWS_TNF_plot + theme(legend.position="top"),
          SQ_TNF_plot + theme(legend.position="top"),
          TST_IL6_plot+ theme(legend.position="top"),
          SWS_IL6_plot + theme(legend.position="top"),
          SQ_IL6_plot + theme(legend.position="top"),
          TST_IL8_plot+ theme(legend.position="top"),
          SWS_IL8_plot + theme(legend.position="top"),
          SQ_IL8_plot + theme(legend.position="top"),
          align = 'vh',
          #labels = c("A", "B", "C"),
          hjust = -1,
          nrow = 3
          #rel_widths = c(1, 2)
)





summarisePredictorRow <- function(measurevar, predictor, DATA) {
  
  f <- reformulate(paste("Group*", predictor), measurevar)
  model <- lme(f, data = DATA,
               random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate <- intervals(model, which = "fixed")
  RoundEstimates <- round(estimate$fixed, digits = 2)
  pval <- summary(model)$tTable[25:30]
  
  result <- c(paste(RoundEstimates[1,2], " (", RoundEstimates[1,1], "-",  RoundEstimates[1,3], ")",
                    sep = ""),
              paste(RoundEstimates[2,2], " (", RoundEstimates[2,1], "-",  RoundEstimates[2,3], ")",
                    sep = ""),
              paste(round(pval[2], digits = 3), sep = ""),
              paste(RoundEstimates[3,2], " (", RoundEstimates[3,1], "-",  RoundEstimates[3,3], ")",
                    sep = ""),
              paste(round(pval[3], digits = 3), sep = ""),
              paste(RoundEstimates[4,2], " (", RoundEstimates[4,1], "-",  RoundEstimates[4,3], ")",
                    sep = ""),
              paste(round(pval[4], digits = 3), sep = ""),
              paste(RoundEstimates[5,2], " (", RoundEstimates[5,1], "-",  RoundEstimates[5,3], ")",
              sep = ""),
              paste(round(pval[5], digits = 3), sep = ""),
              paste(RoundEstimates[6,2], " (", RoundEstimates[6,1], "-",  RoundEstimates[6,3], ")",
              sep = ""),
              paste(round(pval[6], digits = 3), sep = ""))
  
  return(result)
}

DeepSleep_Cytokines_row <- summarisePredictorRow("Deep_Sleep_abs_num", "Cytokine_composite", Sleep_cytokines)
TST_Cytokines_row <- summarisePredictorRow("TST_Zeo", "Cytokine_composite", Sleep_cytokines)
SleepQuality_Cytokines_row <- summarisePredictorRow("SleepQuality", "Cytokine_composite", Sleep_cytokines)


DeepSleep_PET_row <- summarisePredictorRow("Deep_Sleep_abs_num", "GM", PET_sleep)
TST_PET_row <- summarisePredictorRow("TST_Zeo", "GM", PET_sleep)
SleepQuality_PET_row <- summarisePredictorRow("SleepQuality", "GM", PET_sleep)

DeepSleep_Sickness_row <- summarisePredictorRow("Deep_Sleep_abs_num", "Sickness_Q", Sickness_sleep)
TST_Sickness_row <- summarisePredictorRow("TST_Zeo", "Sickness_Q", Sickness_sleep)
SleepQuality_Sickness_row <- summarisePredictorRow("SleepQuality", "Sickness_Q", Sickness_sleep)



# Function to show htmlTable in viewer.
viewHtmlTable <- function(htmlText) {
  tf <- tempfile(fileext = ".html")
  writeLines(htmlText, tf)
  getOption("viewer")(tf)
}


# Make table with predictors
viewHtmlTable(htmlTable(
  x        = rbind(c("Intercept", "Allergy", " ", "RA", "", "Predictor", 
                     " ", "Allergy*Predictor", " ", "RA*Predictor", ""),
                   c("Estimate + CI", "Estimate + CI", "p", "Estimate + CI", "p", "Estimate + CI", 
                     "p", "Estimate + CI", "p", "Estimate + CI", "p"),
                   DeepSleep_Cytokines_row, TST_Cytokines_row, SleepQuality_Cytokines_row, 
                   DeepSleep_PET_row, TST_PET_row, SleepQuality_PET_row,
                   DeepSleep_Sickness_row, TST_Sickness_row, SleepQuality_Sickness_row
  ),
  caption  = paste("Table X. Sleep, predictors"),
  label    = "Table2",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "Deep sleep", "Total sleep time (EEG)", "Sleep quality", 
             "Deep sleep", "Total sleep time (EEG)", "Sleep quality", "Deep sleep",
             "Total sleep time (EEG)", "Sleep quality"),
  rgroup   = c("", "Cytokines", "PBR-28 binding", "Sickness"),
  n.rgroup = c(2, 3, 3, 3),
  ctable   = TRUE,
))


#Control for age and sex

summarisePredictorRow_corrected <- function(measurevar, predictor, DATA) {
  
  f <- reformulate(paste("Group*", predictor, "+ Sex + Age"), measurevar)
  model <- lme(f, data = DATA,
               random = list(~1|Subject, ~1|Pair), na.action = na.exclude)
  
  estimate <- intervals(model, which = "fixed")
  RoundEstimates <- round(estimate$fixed, digits = 2)
  pval <- summary(model)$tTable[33:40]
  
  result <- c(paste(RoundEstimates[1,2], " (", RoundEstimates[1,1], "-",  RoundEstimates[1,3], ")",
                    sep = ""),
              paste(RoundEstimates[2,2], " (", RoundEstimates[2,1], "-",  RoundEstimates[2,3], ")",
                    sep = ""),
              paste(round(pval[2], digits = 3), sep = ""),
              paste(RoundEstimates[3,2], " (", RoundEstimates[3,1], "-",  RoundEstimates[3,3], ")",
                    sep = ""),
              paste(round(pval[3], digits = 3), sep = ""),
              paste(RoundEstimates[4,2], " (", RoundEstimates[4,1], "-",  RoundEstimates[4,3], ")",
                    sep = ""),
              paste(round(pval[4], digits = 3), sep = ""),
              paste(RoundEstimates[7,2], " (", RoundEstimates[7,1], "-",  RoundEstimates[7,3], ")",
                    sep = ""),
              paste(round(pval[7], digits = 3), sep = ""),
              paste(RoundEstimates[8,2], " (", RoundEstimates[8,1], "-",  RoundEstimates[8,3], ")",
                    sep = ""),
              paste(round(pval[8], digits = 3), sep = ""))
  
  return(result)
}

DeepSleep_Cytokines_row_corr <- summarisePredictorRow_corrected("Deep_Sleep_abs_num", "Cytokine_composite", Sleep_cytokines)
TST_Cytokines_row_corr <- summarisePredictorRow_corrected("TST_Zeo", "Cytokine_composite", Sleep_cytokines)
SleepQuality_Cytokines_row_corr <- summarisePredictorRow_corrected("SleepQuality", "Cytokine_composite", Sleep_cytokines)

#Fix age and sex factors
PET_sleep$Sex <- PET_sleep$Sex.x
PET_sleep$Age <- PET_sleep$Age.x

DeepSleep_PET_row_corr <- summarisePredictorRow_corrected("Deep_Sleep_abs_num", "GM", PET_sleep)
TST_PET_row_corr <- summarisePredictorRow_corrected("TST_Zeo", "GM", PET_sleep)
SleepQuality_PET_row_corr <- summarisePredictorRow_corrected("SleepQuality", "GM", PET_sleep)


# Fix age and sex columns
Sickness_sleep$Sex <- Sickness_sleep$Sex.x
Sickness_sleep$Age <- Sickness_sleep$Age.x

DeepSleep_Sickness_row_corr <- summarisePredictorRow_corrected("Deep_Sleep_abs_num", "Sickness_Q", Sickness_sleep)
TST_Sickness_row_corr <- summarisePredictorRow_corrected("TST_Zeo", "Sickness_Q", Sickness_sleep)
SleepQuality_Sickness_row_corr <- summarisePredictorRow_corrected("SleepQuality", "Sickness_Q", Sickness_sleep)




# Make table with predictors
viewHtmlTable(htmlTable(
  x        = rbind(c("Intercept", "Allergy", " ", "RA", "", "Predictor", 
                     " ", "Allergy*Predictor", " ", "RA*Predictor", ""),
                   c("Estimate + CI", "Estimate + CI", "p", "Estimate + CI", "p", "Estimate + CI", 
                     "p", "Estimate + CI", "p", "Estimate + CI", "p"),
                   DeepSleep_Cytokines_row_corr, TST_Cytokines_row_corr, SleepQuality_Cytokines_row_corr, 
                   DeepSleep_PET_row_corr, TST_PET_row_corr, SleepQuality_PET_row_corr,
                   DeepSleep_Sickness_row_corr, TST_Sickness_row_corr, SleepQuality_Sickness_row_corr
  ),
  caption  = paste("Table X. Sleep, predictors. Sex and age included as fixed factors"),
  label    = "Table2",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "Deep sleep", "Total sleep time (EEG)", "Sleep quality", 
             "Deep sleep", "Total sleep time (EEG)", "Sleep quality", "Deep sleep",
             "Total sleep time (EEG)", "Sleep quality"),
  rgroup   = c("", "Cytokines", "PBR-28 binding", "Sickness"),
  n.rgroup = c(2, 3, 3, 3),
  ctable   = TRUE,
))



load("~/Desktop/RAALLPET/PSQI_scores.RData")

PSQI_allergy <- merge(Subject_list_allergy, PSQI_out, 
                      by.x = c("Subject", "PET.date"), by.y = c("Subject", "Date"))

PSQI_allergy$Group <- relevel(PSQI_allergy$Group, ref = "Control")
PSQI_allergy$Pollen_status <- relevel(PSQI_allergy$Pollen_status, ref = "OUT")


lme_PSQI_allergy <- lme(PSQI_total ~ Group*Pollen_status, data = PSQI_allergy,
       random = list(~1|Subject), na.action = na.exclude) 

anova(lme_PSQI_allergy, type = "marginal")
intervals(lme_PSQI_allergy)

ggplot(PSQI_allergy, aes(x=Pollen_status, y=PSQI_total, fill = Group)) +
  geom_boxplot(outlier.size = 0) +
  scale_fill_npg() +
  theme_minimal()+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("PSQI")


Subjects_RA <- subset(Subject_data, Subject_data$Group == "RA" | Subject_data$Group == "Ctrl-RA" | Subject_data$Group == "Ctrl-all+RA")
Subjects_RA$PET1 <- as.Date(as.character(Subjects_RA$PET1))

Subjects_RA$Group[Subjects_RA$Group == "Ctrl-all+RA"] <- "Ctrl-RA"
Subjects_RA$Group <- droplevels(Subjects_RA$Group)


PSQI_RA <- merge(Subjects_RA, PSQI_out, by.x = c("Subject", "PET1"), by.y = c("Subject", "Date"))

anova(lme(PSQI_total ~ Group, data = PSQI_RA, random = list(~1|Subject), na.action = na.exclude))

intervals(lme(PSQI_total ~ Group, data = PSQI_RA,
          random = list(~1|Subject), na.action = na.exclude), which = "fixed")

ggplot(PSQI_RA, aes(x=Group, y=PSQI_total)) +
  geom_boxplot(outlier.size = 0) +
  scale_fill_npg() +
  theme_minimal()+
  theme(legend.position="top") +
  xlab("Pollen season") +
  ylab("PSQI")


# Investigate asthma symptoms
Astma_symptoms <- read_delim("~/Desktop/RAALLPET/Astma_symptoms.csv", "\t", escape_double = FALSE, trim_ws = TRUE)
# Change date variable to POSIXct format
Astma_symptoms$Date <- as.Date(paste("20", substr(as.character(Astma_symptoms$Date), 1,2), "-", 
                                          substr(as.character(Astma_symptoms$Date), 3,4), "-", 
                                          substr(as.character(Astma_symptoms$Date), 5,6), 
                                          sep = ""))


Astma_symptoms$Total <- Astma_symptoms$ACQ1 + Astma_symptoms$ACQ2 + Astma_symptoms$ACQ3 + Astma_symptoms$ACQ4 + Astma_symptoms$ACQ5
Astma_symptoms$Total_minus_sleep <- Astma_symptoms$ACQ1 + Astma_symptoms$ACQ2 + Astma_symptoms$ACQ4 + Astma_symptoms$ACQ5

Sleep_Astma_symptoms <- merge(Astma_symptoms, data_sleep_measures, by.x = c("Subject", "Date"), by.y = c("Subject", "PET.date"))


Sleep_Astma_symptoms$Group <- droplevels(Sleep_Astma_symptoms$Group.y)

# Relevel factors
Sleep_Astma_symptoms$Pollen_status <- relevel(Sleep_Astma_symptoms$Pollen_status, ref = "OUT")


lme_astma_TST <- lme(TST_Zeo ~ Total, data = subset(Sleep_Astma_symptoms, Group == "Allergy"),
                        random = list(~1|Subject), na.action = na.exclude) 

anova(lme_astma_TST, type = "marginal")
intervals(lme_astma_TST, which = "fixed")

lme_astma_SQ <- lme(SleepQuality ~ Total, data = subset(Sleep_Astma_symptoms, Group == "Allergy"),
                     random = list(~1|Subject), na.action = na.exclude) 

anova(lme_astma_SQ, type = "marginal")
intervals(lme_astma_SQ, which = "fixed")

lme_astma_SQ_minus_sleep <- lme(SleepQuality ~ Total_minus_sleep, data = subset(Sleep_Astma_symptoms, Group == "Allergy"),
                    random = list(~1|Subject), na.action = na.exclude) 

anova(lme_astma_SQ_minus_sleep, type = "marginal")
intervals(lme_astma_SQ_minus_sleep, which = "fixed")

lme_astma_SQ_IN <- lme(SleepQuality ~ Total, data = subset(Sleep_Astma_symptoms, Group == "Allergy" & Pollen_status == "IN"),
                       random = list(~1|Subject), na.action = na.exclude)
anova(lme_astma_SQ_IN, type = "marginal")
intervals(lme_astma_SQ_IN, which = "fixed")

lme_astma_SWS <- lme(Deep_Sleep_abs_num ~ Total, data = subset(Sleep_Astma_symptoms, Group == "Allergy"),
                     random = list(~1|Subject), na.action = na.exclude) 

anova(lme_astma_SWS, type = "marginal")
intervals(lme_astma_SWS, which = "fixed")
