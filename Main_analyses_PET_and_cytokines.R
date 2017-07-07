require(gdata)
require(ggplot2)
require(nlme)
require(plyr)
require(gridExtra)
require(cowplot)
require(readr)
require(htmlTable)

setwd("~/Desktop/RAALLPET")
source('Utils/SummarisingFunctions.R', chdir = T)
source('Utils/Multiplot.R', chdir = T)


data_TNF = read.xls ("RAALLPET_Serumprover_MesoscaleReadout_Allergy_170221.xls", sheet = 1, header = TRUE)
data_IL6 = read.xls ("RAALLPET_Serumprover_MesoscaleReadout_Allergy_170221.xls", sheet = 6, header = TRUE)
data_IL5 = read.xls ("RAALLPET_Serumprover_MesoscaleReadout_Allergy_170221.xls", sheet = 11, header = TRUE)
data_IFN = read.xls ("RAALLPET_Serumprover_MesoscaleReadout_Allergy_170221.xls", sheet = 10, header = TRUE)
data_IL4 = read.xls ("RAALLPET_Serumprover_MesoscaleReadout_Allergy_170221.xls", sheet = 7, header = TRUE)
data_IL8 = read.xls ("RAALLPET_Serumprover_MesoscaleReadout_Allergy_170221.xls", sheet = 5, header = TRUE)


ListOfPairs <- read_delim("~/Desktop/RAALLPET/ListOfPairs.csv", ";", escape_double = FALSE, trim_ws = TRUE)

# Count no in detection range, TNF
count(data_TNF$In.detection.range. == "Yes")
# 59/60 in detectable range

# Count no in detection range, IL-6
count(data_IL6$In.detection.range. == "Yes")
# 42/60 in detectable range

# Count no in detection range, IL-5
count(data_IL5$In.detection.range. == "Yes")
# 35/60 in detectable range

# Count no in detection range, IFN
count(data_IFN$In.detection.range. == "Yes")
# 60/60 in detectable range

# Count no in detection range, IL-8
count(data_IL8$In.detection.range. == "Yes")
# 60/60 in detectable range

# Replace values under detection limit with the detection limit value
# TNF
for(i in 1:length(data_TNF$Sample.ID)){
  if(data_TNF$In.detection.range.[[i]] == "No"){
    data_TNF$Measured.Concentration[i] <- data_TNF$Detection.limit[i]
  }  
}

# IL-6
for(i in 1:length(data_IL6$Sample.ID)){
  if(data_IL6$In.detection.range.[[i]] == "No"){
    data_IL6$Measured.Concentration[i] <- data_IL6$Detection.limit[i]
  }  
}

# IL-5
for(i in 1:length(data_IL5$Sample.ID)){
  if(data_IL5$In.detection.range.[[i]] == "No"){
    data_IL5$Measured.Concentration[i] <- data_IL5$Detection.limit[i]
  }  
}

# IFN was all above detection limit

# Fix calculated.measures (2*measured)
data_TNF$Calculated.concentration <- 2*data_TNF$Measured.Concentration
data_IL6$Calculated.concentration <- 2*data_IL6$Measured.Concentration
data_IL5$Calculated.concentration <- 2*data_IL5$Measured.Concentration

# Histogram of TNF alpha
H1 <- ggplot(data_TNF, aes(x=Calculated.concentration)) + 
  geom_histogram(binwidth=0.1, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Calculated.concentration)),
             color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="blue") +
  ggtitle("TNF-alpha")

# Log10 transform TNF alpha
data_TNF$TNF_log_concentration <- log10(data_TNF$Calculated.concentration)

H1b <- ggplot(data_TNF, aes(x=TNF_log_concentration)) + 
  geom_histogram(binwidth=0.01, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(TNF_log_concentration)),
             color="blue", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="blue") +
  ggtitle("TNF-alpha, log transformed")

# Histogram of IL-6
H2 <- ggplot(data_IL6, aes(x=Calculated.concentration)) + 
  geom_histogram(binwidth=0.05, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Calculated.concentration)),
             color="red", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="red") +
  ggtitle("IL-6")


# Log10 transform IL-6
data_IL6$IL6_log_concentration <- log10(data_IL6$Calculated.concentration)

H3 <- ggplot(data_IL6, aes(x=IL6_log_concentration)) + 
  geom_histogram(binwidth=0.05, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(IL6_log_concentration)),
             color="red", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="red") +
  ggtitle("IL-6 log transformed")

# Histogram of IL-5
H4 <- ggplot(data_IL5, aes(x=Calculated.concentration)) + 
  geom_histogram(binwidth=0.05, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Calculated.concentration)),
             color="green", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="green") +
  ggtitle("IL-5")


# Log10 transform IL-5
data_IL5$IL5_log_concentration <- log10(data_IL5$Calculated.concentration)

H5 <- ggplot(data_IL5, aes(x=IL5_log_concentration)) + 
  geom_histogram(binwidth=0.05, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(IL5_log_concentration)),
             color="green", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="green") +
  ggtitle("IL-5 log transformed")

# Histogram of IFN-gamma
H6 <- ggplot(data_IFN, aes(x=Calculated.concentration)) + 
  geom_histogram(binwidth=0.5, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Calculated.concentration)),
             color="violet", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="violet") +
  ggtitle("IFN-gamma")


# Log10 transform IFN-gamma
data_IFN$IFN_log_concentration <- log10(data_IFN$Calculated.concentration)

H7 <- ggplot(data_IFN, aes(x=IFN_log_concentration)) + 
  geom_histogram(binwidth=0.02, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(IFN_log_concentration)),
             color="violet", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="violet") +
  ggtitle("IFN-gamma log transformed")

# Sqrt transform IFN-gamma
data_IFN$Sqrt <- sqrt(data_IFN$Calculated.concentration)

H8 <- ggplot(data_IFN, aes(x=Sqrt)) + 
  geom_histogram(binwidth=0.02, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Sqrt)),
             color="violet", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="violet") +
  ggtitle("IFN-gamma sqrt transformed")

# Histogram of IL-8
H9 <- ggplot(data_IL8, aes(x=Calculated.concentration)) + 
  geom_histogram(binwidth=0.2, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(Calculated.concentration)),
             color="orange", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="orange") +
  ggtitle("IL-8")


# Log10 transform IFN-gamma
data_IL8$IL8_log_concentration <- log10(data_IL8$Calculated.concentration)

H10 <- ggplot(data_IL8, aes(x=IL8_log_concentration)) + 
  geom_histogram(binwidth=0.02, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(IL8_log_concentration)),
             color="orange", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="orange") +
  ggtitle("IL-8 log transformed")


multiplot(H1, H1b, H2, H3, H4, H5, H6, H7, H9, H10, cols = 2)

# Combine data to be used in 1 data frame.
names(data_TNF)[names(data_TNF) == 'Calculated.concentration'] <- 'TNF_concentration'
names(data_IL5)[names(data_IL5) == 'Calculated.concentration'] <- 'IL5_concentration'
names(data_IL6)[names(data_IL6) == 'Calculated.concentration'] <- 'IL6_concentration'
names(data_IFN)[names(data_IFN) == 'Calculated.concentration'] <- 'IFN_concentration'
names(data_IL8)[names(data_IL8) == 'Calculated.concentration'] <- 'IL8_concentration'

data_TNFIL5 <- merge(data_TNF, data_IL5, by = c("Sample.ID", "Group", "Season"))
data_IL6IFN <- merge(data_IL6, data_IFN, by = c("Sample.ID", "Group", "Season"))
data_combined <- merge(data_TNFIL5, data_IL6IFN, by = c("Sample.ID", "Group", "Season"))
data_combined <- merge(data_combined, data_IL8, by = c("Sample.ID", "Group", "Season"))
data_cytokines <- subset(data_combined, select = c("Sample.ID", "Group", "Season", "TNF_concentration", 
                                                   "TNF_log_concentration", "IL5_concentration", 
                                                   "IL5_log_concentration", "IL6_concentration",
                                                   "IL6_log_concentration", "IFN_concentration", 
                                                   "IFN_log_concentration", "IL8_concentration",
                                                   "IL8_log_concentration"))

# Rename season
levels(data_cytokines$Season)[levels(data_cytokines$Season)=="UT"] <- "OUT"

# Relevel factors with logical ref
data_cytokines$Group <- relevel(data_cytokines$Group, ref = "Ctrl")
data_cytokines$Season <- relevel(data_cytokines$Season, ref = "OUT")

# Add matching list
data_cytokines <- merge(data_cytokines, ListOfPairs, by.x = "Sample.ID", by.y = "Subject")

# Add deviation coding, so that main effect represent true main effect and not simple main effect
contrasts(data_cytokines$Group) <- rbind(-.5, .5)
colnames(contrasts(data_cytokines$Group)) <- levels(data_cytokines$Group)[2]

contrasts(data_cytokines$Season) <- rbind(-.5, .5)
colnames(contrasts(data_cytokines$Season)) <- levels(data_cytokines$Season)[2]

# Combine IL-5 and IFN to look at shifts
data_cytokines$IFN_IL5_ratio <- data_cytokines$IFN_log_concentration/data_cytokines$IL5_log_concentration



# Make function to summarise effects on cytokines

summariseCytokineRow <- function(measurevar) {
  
  f <- reformulate("Group + Season + Group*Season", measurevar)
  model <- lme(f, data = data_cytokines,
               random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)
  
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

XTNF <- summariseCytokineRow("TNF_concentration")
XlogIL6 <- summariseCytokineRow("IL6_log_concentration")
XlogIL5 <- summariseCytokineRow("IL5_log_concentration")
XlogIFN <- summariseCytokineRow("IFN_log_concentration")
XlogIL8 <- summariseCytokineRow("IL8_log_concentration")
XIFN_IL5_ratio <- summariseCytokineRow("IFN_IL5_ratio")


# Function to show htmlTable in viewer.
viewHtmlTable <- function(htmlText) {
  tf <- tempfile(fileext = ".html")
  writeLines(htmlText, tf)
  getOption("viewer")(tf)
}


# Make table with cytokine effects
viewHtmlTable(htmlTable(
  x        = rbind(c("Intercept", "Group (allergic vs controls)", " ", "Season (in vs out)", 
                     " ", "Group*season", " "),
                   c("Estimate + CI", "Estimate + CI", "p", "Estimate + CI", 
                     "p", "Estimate + CI", "p"),
                   XTNF, XlogIL6, XlogIL5, 
                   XlogIFN, XlogIL8, XIFN_IL5_ratio
  ),
  caption  = paste("Table 4. Cytokine"),
  label    = "Table4",
  rowlabel = "Variables",
  rnames = c("Variable", " ", "TNF-", "log(IL-6)", "log(IL-5)", 
             "log(IFN-)", "log(IL-8)", "log(IFN)/log(Il-5)"),
  rgroup   = c("", "Cytokines"),
  n.rgroup = c(2, 6),
  ctable   = TRUE,
))


# Main effect of group and season, TNF
summary_TNF <- summarySEwithin(data=data_cytokines, measurevar = "TNF_concentration", 
                               betweenvars="Group", withinvars="Season", idvar="Sample.ID", 
                               na.rm=FALSE)


dodge = position_dodge(width=0.2)
TNF_main <- ggplot(data_cytokines, aes(Season, TNF_concentration, group=Sample.ID, colour = factor(Group))) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_TNF, aes(Season, TNF_concentration, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("#D55E00", "#0072B2"), 
                     breaks=c("All", "Ctrl"),
                     labels=c("Allergy", "Control")) +
  xlab("Pollen season") +
  ylab("TNF-alpha") +
  theme(legend.position= "none")

lme_TNF <- lme(TNF_concentration ~ Group*Season, data = data_cytokines,
                random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_TNF, type = "marginal")
intervals(lme_TNF, which = "fixed")

# Post hoc
lme_TNF_all <- lme(TNF_concentration ~ Season, data = subset(data_cytokines, Group == "All"),
               random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_TNF_all, type = "marginal")
intervals(lme_TNF_all)

lme_TNF_ctrl <- lme(TNF_concentration ~ Season, data = subset(data_cytokines, Group == "Ctrl"),
                   random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_TNF_ctrl, type = "marginal")
intervals(lme_TNF_ctrl)

lme_TNF_out <- lme(TNF_concentration ~ Group, data = subset(data_cytokines, Season == "OUT"),
                    random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_TNF_out, type = "marginal")
intervals(lme_TNF_out)

# Main effect of group and season, IL-6
summary_IL6 <- summarySEwithin(data=data_cytokines, measurevar = "IL6_log_concentration", 
                               betweenvars="Group", withinvars="Season", idvar="Sample.ID", 
                               na.rm=FALSE)

dodge = position_dodge(width=0.2)
IL6_main <- ggplot(data_cytokines, aes(Season, IL6_log_concentration, group=Sample.ID, colour = factor(Group))) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_IL6, aes(Season, IL6_log_concentration, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("#D55E00", "#0072B2"), 
                     breaks=c("All", "Ctrl"),
                     labels=c("Allergy", "Control")) +
  xlab("Pollen season") +
  ylab("log(IL-6)") +
  theme(legend.position="none")

lme_IL6 <- lme(IL6_log_concentration ~ Group*Season, data = data_cytokines,
               random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_IL6, type = "marginal")
summary(lme_IL6)
intervals(lme_IL6)

# Main effect of group and season, IL-5
summary_IL5 <- summarySEwithin(data=data_cytokines, measurevar = "IL5_log_concentration", 
                               betweenvars="Group", withinvars="Season", idvar="Sample.ID", 
                               na.rm=FALSE)

dodge = position_dodge(width=0.2)
IL5_main <- ggplot(data_cytokines, aes(Season, IL5_log_concentration, group=Sample.ID, colour = factor(Group))) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_IL5, aes(Season, IL5_log_concentration, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("#D55E00", "#0072B2"), 
                     breaks=c("All", "Ctrl"),
                     labels=c("Allergy", "Control")) +
  xlab("Pollen season") +
  ylab("log(IL-5)") +
  theme(legend.position="none")

lme_IL5 <- lme(IL5_log_concentration ~ Group*Season, data = data_cytokines,
               random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_IL5, type = "marginal")
intervals(lme_IL5, which = "fixed")

# Post hoc
lme_IL5_all <- lme(IL5_log_concentration ~ Season, data = subset(data_cytokines, Group == "All"),
               random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_IL5_all, type = "marginal")
intervals(lme_IL5_all)

lme_IL5_ctrl <- lme(IL5_log_concentration ~ Season, data = subset(data_cytokines, Group == "Ctrl"),
                   random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_IL5_ctrl, type = "marginal")
intervals(lme_IL5_ctrl)

lme_IL5_out <- lme(IL5_log_concentration ~ Group, data = subset(data_cytokines, Season == "OUT"),
                    random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_IL5_out, type = "marginal")
intervals(lme_IL5_out, which = "fixed")


# Main effect of group and season, IFN
summary_IFN <- summarySEwithin(data=data_cytokines, measurevar = "IFN_log_concentration", 
                               betweenvars="Group", withinvars="Season", idvar="Sample.ID", 
                               na.rm=FALSE)

dodge = position_dodge(width=0.2)
IFN_main <- ggplot(data_cytokines, aes(Season, IFN_log_concentration, group=Sample.ID, colour = factor(Group))) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_IFN, aes(Season, IFN_log_concentration, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("#D55E00", "#0072B2"), 
                     breaks=c("All", "Ctrl"),
                     labels=c("Allergy", "Control")) +
  xlab("Pollen season") +
  ylab("log(IFN-gamma)") +
  theme(legend.position="none")

lme_IFN <- lme(IFN_log_concentration ~ Group*Season, data = data_cytokines,
               random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_IFN)
intervals(lme_IFN, which = "fixed")

grid.arrange(TNF_main, IL6_main, IL5_main, IFN_main, 
             top=textGrob("Effect of group and pollen season on cytokines",gp=gpar(fontsize=15,font=3)))

# Main effect of group and season, IL8
summary_IL8 <- summarySEwithin(data=data_cytokines, measurevar = "IL8_log_concentration", 
                               betweenvars="Group", withinvars="Season", idvar="Sample.ID", 
                               na.rm=FALSE)

dodge = position_dodge(width=0.2)
IL8_main <- ggplot(data_cytokines, aes(Season, IL8_log_concentration, group=Sample.ID, colour = factor(Group))) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_IL8, aes(Season, IL8_log_concentration, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("#D55E00", "#0072B2"), 
                     breaks=c("All", "Ctrl"),
                     labels=c("Allergy", "Control")) +
  xlab("Pollen season") +
  ylab("log(IL-8)") +
  theme(legend.position="none")

lme_IL8 <- lme(IL8_log_concentration ~ Group*Season, data = data_cytokines,
               random = list(~1|Sample.ID, ~1|Pair), na.action = na.exclude)

anova(lme_IL8)
summary(lme_IL8)
intervals(lme_IL8, which = "fixed")


# Combine IL-5 and IFN to look at shifts
data_cytokines$IFN_IL5_ratio <- data_cytokines$IFN_log_concentration/data_cytokines$IL5_log_concentration


IFN_IL5_log_hist <- ggplot(data_cytokines, aes(x=IFN_IL5_ratio)) + 
  geom_histogram(binwidth=0.02, color="black", fill="white") +
  geom_vline(aes(xintercept=mean(IFN_IL5_ratio)),
             color="orange", linetype="dashed", size=1)+
  geom_density(alpha=.5, fill="orange") +
  ggtitle("log IFN-gamma/log IL-5 ratio")

# Main effect of group and season, Il-5/IFN ratio
dodge = position_dodge(width=0.2)
IFN_IL5_main <- ggplot(data_cytokines, aes(Season, IFN_IL5_ratio, group=Sample.ID, colour = factor(Group))) +
  geom_jitter(position=dodge, size = 2) +
  geom_line(position=dodge, size=0.5) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl"),
                     labels=c("Allergy", "Control")) +
  ggtitle("Effect of pollen season in patients and controls on log IFN-gamma/log IL-5 ratio")

lme_IFN_IL5 <- lme(IFN_IL5_ratio ~ Group*Season, data = data_cytokines,
               random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_IFN_IL5, type = "marginal")
intervals(lme_IFN_IL5, which = "fixed")

multiplot(IFN_IL5_log_hist, IFN_IL5_main, cols = 2)

# Post hoc
lme_IFN_IL5_all <- lme(IFN_IL5_ratio ~ Season, data = subset(data_cytokines, Group == "All"),
                   random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_IFN_IL5_all, type = "marginal")
intervals(lme_IFN_IL5_all)

lme_IFN_IL5_ctrl <- lme(IFN_IL5_ratio ~ Season, data = subset(data_cytokines, Group == "Ctrl"),
                       random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_IFN_IL5_ctrl, type = "marginal")
intervals(lme_IFN_IL5_ctrl)

lme_IFN_IL5_out <- lme(IFN_IL5_ratio ~ Group, data = subset(data_cytokines, Season == "OUT"),
                        random = ~ 1|Sample.ID, na.action = na.exclude)

anova(lme_IFN_IL5_out, type = "marginal")
intervals(lme_IFN_IL5_out)


# Count Il-4
count(subset(data_IL4, data_IL4$Group == "All" & data_IL4$Season == "IN")$In.detection.range. == "Yes")
count(subset(data_IL4, data_IL4$Group == "Ctrl" & data_IL4$Season == "IN")$In.detection.range. == "Yes")

count(subset(data_IL4, data_IL4$Group == "All" & data_IL4$Season == "UT")$In.detection.range. == "Yes")
count(subset(data_IL4, data_IL4$Group == "Ctrl" & data_IL4$Season == "UT")$In.detection.range. == "Yes")

# Similar in patients and controls

save(data_cytokines, file = "Data_cytokines_allergy.RData")
write.csv2(data_cytokines, file = "Data_cytokines_allergy.csv", row.names = F)

### PET data

load("Data_PET_allergy.RData")
Data <- Data_export

Data <- merge(Data, ListOfPairs)


levels(Data$Pollen_status)[levels(Data$Pollen_status)=="Out_season"] <- "OUT"
levels(Data$Pollen_status)[levels(Data$Pollen_status)=="In_season"] <- "IN"


# VT från 2TCM med 3exp metabolit-korrektion kan användas för primäranalyser. Extrahera dessa
Data_subset <- subset(Data, Model == "2TCM" & Metabolite == "3exp" & Parameter == "VT")


# 2 set beroende på length 63 eller 93. 93 fattas för en fp
Data_93 <- subset(Data_subset, Length == 93)
Data_63 <- subset(Data_subset, Length == 63)


# Check effect of length
dodge = position_dodge(width=0.5)
P1 <- ggplot(subset(Data_subset, Pollen_status == "IN"), aes(Length, GM, group=Subject, ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, color="red", fill = "red", size = 3) +
  geom_line(position=dodge, size=0.5) +
  ggtitle("Pollen season")


P2 <- ggplot(subset(Data_subset, Pollen_status == "OUT"), aes(Length, GM, group=Subject, ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, color="red", fill = "red", size = 3) +
  geom_line(position=dodge, size=0.5) +
  ggtitle("Out of pollen season")

multiplot(P1, P2, cols = 2)


lme_length <- lme(GM ~ Length + Pollen_status + Group + Genotype, data = Data_subset,
                  random = ~ 1|Subject, na.action = na.exclude) 
anova(lme_length)
intervals(lme_length)

# For 93 compared to 63 there was an increase in 0.27175021 for GM. For the one subject where 93 was not
# obtained this was added to the value at 63 as an interpolation

Data_93$GM[51] <- Data_63$GM[51] + 0.27175021



# Divide in 2 data sets depending on season
Data_inseason <- subset(Data_93, Pollen_status == "IN")
Data_outseason <- subset(Data_93, Pollen_status == "OUT")

# Divide in 2 data sets depending on genotype
Data_HAB <- subset(Data_93, Genotype == "G/G")
Data_MAB <- subset(Data_93, Genotype == "A/G")


# Plot data per genotype
summary_genotype <- summarySE(Data_93, measurevar="GM", groupvars="Genotype")
ggplot()+
  geom_jitter(aes(Genotype, GM), data = Data_93, colour = I("red"), 
              position = position_jitter(width = 0.05)) +
  geom_crossbar(data=summary_genotype,aes(x=Genotype,ymin=GM, ymax=GM,y=GM), width = 0.5) +
  ggtitle("Effect of genotype")

# Plot data per sex
summary_sex <- summarySE(Data_93, measurevar="GM", groupvars="Sex")
ggplot()+
  geom_jitter(aes(Sex, GM), data = Data_93, colour = I("red"), 
              position = position_jitter(width = 0.05)) +
  geom_crossbar(data=summary_sex,aes(x=Sex,ymin=GM, ymax=GM,y=GM), width = 0.5) +
  ggtitle("Effect of sex")

# Plot out and in season
ggplot()+
  geom_jitter(aes(Pollen_status, GM, colour = factor(Genotype)), data = Data_93, 
              position = position_jitter(width = 0.05)) +
  scale_color_manual(name = "Genotype", values=c("red", "blue"), 
                     breaks=c("A/G", "G/G"),
                     labels=c("A/G", "G/G"))

# Plot out and in season for patients/controls
P3 <- ggplot()+
  geom_jitter(aes(Pollen_status, GM, colour = factor(Group)), data = Data_HAB, 
              position = position_jitter(width = 0.05)) +
  scale_color_manual(name = "Group", values=c("red", "blue"), 
                     breaks=c("Allergi", "Kontroll-allergi"),
                     labels=c("Allergy", "Control")) +
  ggtitle("High affinity binders")

P4 <- ggplot()+
  geom_jitter(aes(Pollen_status, GM, colour = factor(Group)), data = Data_MAB, 
              position = position_jitter(width = 0.05)) +
  scale_color_manual(name = "Group", values=c("red", "blue"), 
                     breaks=c("Allergi", "Kontroll-allergi"),
                     labels=c("Allergy", "Control")) +
  ggtitle("Mixed affinity binders")

multiplot(P3, P4, cols = 2)

summary_season <- summarySEwithin(data=Data_93, measurevar = "GM", betweenvars="Group", 
                                  withinvars="Pollen_status", idvar="Subject", na.rm=FALSE)

dodge = position_dodge(width=0.2)
GM_plot <- ggplot(data = Data_93, aes(Pollen_status, GM, group=Subject, colour = factor(Group), 
                           ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_season, aes(Pollen_status, GM, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("#D55E00", "#0072B2"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  xlab("Pollen season") +
  ylab("Grey matter (VT)") 

# Add deviation coding, so that main effect represent true main effect and not simple main effect

contrasts(Data_93$Group) <- rbind(-.5, .5)
colnames(contrasts(Data_93$Group)) <- levels(Data_93$Group)[2]

contrasts(Data_93$Pollen_status) <- rbind(-.5, .5)
colnames(contrasts(Data_93$Pollen_status)) <- levels(Data_93$Pollen_status)[2]

# Main analysis of PET data

lme_main <- lme(GM ~ Group*Pollen_status + Genotype, data = Data_93,
                random = list(~1|Subject, ~1|Pair), na.action = na.exclude) 

anova(lme_main, type = "marginal")
intervals(lme_main, which = "fixed")

# Adjust for sex

lme_main_adj <- lme(GM ~ Group*Pollen_status + Genotype + Sex, data = Data_93,
                    random = ~ 1|Subject, na.action = na.exclude) 
anova(lme_main_adj, type = "marginal")
intervals(lme_main_adj, which = "fixed")



save(Data_93, file = "PET_VT_93_2TCM_3exp.RData")
save(Data_63, file = "PET_VT_63_2TCM_3exp.RData")

ggdraw() +
  draw_plot(GM_plot, 0, 2/3, 0.5, 1/3) +
  draw_plot(TNF_main, 0.5, 2/3, 0.5, 1/3) +
  draw_plot(IL6_main, 0, 1/3, .5, 1/3) +
  draw_plot(IL5_main, .5, 1/3, .5, 1/3) +
  draw_plot(IFN_main, 0, 0, .5, 1/3) +
  draw_plot(IL8_main, .5, 0, .5, 1/3) +
  draw_plot_label(c("A", "B", "C", "D", "E", "F"), c(0, 0.5, 0, 0.5, 0, 0.5), c(1, 1, 2/3, 2/3, 1/3, 1/3), size = 15)


