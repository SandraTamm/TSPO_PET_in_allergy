require(gdata)
require(ggplot2)
require(nlme)
require(plyr)
require(gridExtra)
require(cowplot)

source('Utils/SummarisingFunctions.R', chdir = T)
source('Utils/Multiplot.R', chdir = T)

setwd("~/Desktop/RAALLPET")
load("PET_VT_63_2TCM_3exp.RData")

# Gray matter

summary_GM <- summarySEwithin(data=Data_63, measurevar = "GM", betweenvars="Group", 
                                  withinvars="Pollen_status", idvar="Subject", na.rm=FALSE)

dodge = position_dodge(width=0.2)
GM_plot <- ggplot(data = Data_63, aes(Pollen_status, GM, group=Subject, colour = factor(Group), 
                                      ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_GM, aes(Pollen_status, GM, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Healthy subjects")) +
  theme(legend.position= "top") +
  xlab("Pollen season") +
  ylab("Grey matter") 

# Lateral Frontal Cortex
summary_LFC <- summarySEwithin(data=Data_63, measurevar = "X.LFC", betweenvars="Group", 
                              withinvars="Pollen_status", idvar="Subject", na.rm=FALSE)

dodge = position_dodge(width=0.2)
LFC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.LFC, group=Subject, colour = factor(Group), 
                                      ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_LFC, aes(Pollen_status, X.LFC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Lateral Frontal \nCortex") 

# Medial Frontal Cortex
summary_MFC <- summarySEwithin(data=Data_63, measurevar = "X.MFC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=FALSE)

dodge = position_dodge(width=0.2)
MFC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.MFC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_MFC, aes(Pollen_status, X.MFC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Medial Frontal \nCortex") 

# Orbito Frontal Cortex
summary_OFC <- summarySEwithin(data=Data_63, measurevar = "X.OFC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
OFC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.OFC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_OFC, aes(Pollen_status, X.OFC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Orbital Frontal \nCortex") 


# Anterior cingulate cortex
summary_ACC <- summarySEwithin(data=Data_63, measurevar = "X.ACC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
ACC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.ACC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_ACC, aes(Pollen_status, X.ACC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Anterior Cingulate \nCortex") 


# Insula
summary_INS <- summarySEwithin(data=Data_63, measurevar = "X.INS", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
INS_plot <- ggplot(data = Data_63, aes(Pollen_status, X.INS, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_INS, aes(Pollen_status, X.INS, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Insula") 

# Lateral Temporal Cortex
summary_LTC <- summarySEwithin(data=Data_63, measurevar = "X.LTC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
LTC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.LTC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_LTC, aes(Pollen_status, X.LTC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Lateral Temporal \nCortex") 

# Medial Temporal Cortex. Check name
summary_MTC <- summarySEwithin(data=Data_63, measurevar = "X.MTC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
MTC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.MTC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_MTC, aes(Pollen_status, X.MTC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Medial Temporal \nCortex") 


# Sensory motor cortex
summary_SMC <- summarySEwithin(data=Data_63, measurevar = "X.SMC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
SMC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.SMC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_SMC, aes(Pollen_status, X.SMC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Sensory Motor Cortex") 

# Temporal Pole
summary_TP <- summarySEwithin(data=Data_63, measurevar = "X.TP", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
TP_plot <- ggplot(data = Data_63, aes(Pollen_status, X.TP, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_TP, aes(Pollen_status, X.TP, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Temporal Pole") 

# Lateral Parietal Cortex
summary_LPC <- summarySEwithin(data=Data_63, measurevar = "X.LPC", betweenvars="Group", 
                              withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
LPC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.LPC, group=Subject, colour = factor(Group), 
                                      ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_LPC, aes(Pollen_status, X.LPC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Lateral Parietal \nCortex") 


# Posterior cingulate cortex
summary_PCC <- summarySEwithin(data=Data_63, measurevar = "X.PCC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
PCC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.PCC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_PCC, aes(Pollen_status, X.PCC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Posterior Cingulate \nCortex") 


# Medial Inferior Occipital Cortex
summary_MIOC <- summarySEwithin(data=Data_63, measurevar = "X.MIOC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
MIOC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.MIOC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_MIOC, aes(Pollen_status, X.MIOC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Medial Inferior \nOccipital Cortex") 


# Lateral Occipital Cortex
summary_LOC <- summarySEwithin(data=Data_63, measurevar = "X.LOC", betweenvars="Group", 
                                withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
LOC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.LOC, group=Subject, colour = factor(Group), 
                                        ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_LOC, aes(Pollen_status, X.LOC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Lateral Occipital \nCortex") 

# Hippocampus
summary_HIP <- summarySEwithin(data=Data_63, measurevar = "X.HIP", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
HIP_plot <- ggplot(data = Data_63, aes(Pollen_status, X.HIP, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_HIP, aes(Pollen_status, X.HIP, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Hippocampus") 

# Parahippocampal gyrus
summary_PHIP <- summarySEwithin(data=Data_63, measurevar = "X.PHIP", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
PHIP_plot <- ggplot(data = Data_63, aes(Pollen_status, X.PHIP, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_PHIP, aes(Pollen_status, X.PHIP, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Parahipppocampal \ngyrus") 


# Medial Cingulate Cortex
summary_MCC <- summarySEwithin(data=Data_63, measurevar = "X.MCC", betweenvars="Group", 
                                withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
MCC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.MCC, group=Subject, colour = factor(Group), 
                                        ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_MCC, aes(Pollen_status, X.MCC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Medial Cingulate \nCortex") 

# Medial Parietal Cortex
summary_MPC <- summarySEwithin(data=Data_63, measurevar = "X.MPC", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
MPC_plot <- ggplot(data = Data_63, aes(Pollen_status, X.MPC, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_MPC, aes(Pollen_status, X.MPC, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Medial Parietal \nCortex") 

# Amygdala
summary_AMG <- summarySEwithin(data=Data_63, measurevar = "AMG", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
AMG_plot <- ggplot(data = Data_63, aes(Pollen_status, AMG, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_AMG, aes(Pollen_status, AMG, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Amygdala") 

# Caudate
summary_CAU <- summarySEwithin(data=Data_63, measurevar = "CAU", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
CAU_plot <- ggplot(data = Data_63, aes(Pollen_status, CAU, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_CAU, aes(Pollen_status, CAU, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Caudate") 

# Putamen
summary_PUT <- summarySEwithin(data=Data_63, measurevar = "PUT", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
PUT_plot <- ggplot(data = Data_63, aes(Pollen_status, PUT, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_PUT, aes(Pollen_status, PUT, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Putamen") 

# Pallidum
summary_PAL <- summarySEwithin(data=Data_63, measurevar = "PAL", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
PAL_plot <- ggplot(data = Data_63, aes(Pollen_status, PAL, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_PAL, aes(Pollen_status, PAL, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Pallidum") 


# Thalamus
summary_THA <- summarySEwithin(data=Data_63, measurevar = "THA", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
THA_plot <- ggplot(data = Data_63, aes(Pollen_status, THA, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_THA, aes(Pollen_status, THA, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Thalamus") 


# White Matter
summary_WM <- summarySEwithin(data=Data_63, measurevar = "WM", betweenvars="Group", 
                               withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
WM_plot <- ggplot(data = Data_63, aes(Pollen_status, WM, group=Subject, colour = factor(Group), 
                                       ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_WM, aes(Pollen_status, WM, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("White Matter") 

# Cerebellum
summary_CER <- summarySEwithin(data=Data_63, measurevar = "X.CER", betweenvars="Group", 
                              withinvars="Pollen_status", idvar="Subject", na.rm=TRUE)

dodge = position_dodge(width=0.2)
CER_plot <- ggplot(data = Data_63, aes(Pollen_status, X.CER, group=Subject, colour = factor(Group), 
                                      ymin = 0, ymax = 7)) +
  geom_jitter(position=dodge, size = 0) +
  geom_line(position=dodge, size=0.6, linetype="dotted") +
  geom_line(data = summary_CER, aes(Pollen_status, X.CER, group=Group), size = 1.2) +
  scale_color_manual(name = "Group", values=c("black", "red"), 
                     breaks=c("All", "Ctrl-all"),
                     labels=c("Allergy", "Control")) +
  theme(legend.position="none") +
  xlab("Pollen season") +
  ylab("Cerebellum") 


multiplot(GM_plot, LFC_plot, MFC_plot, OFC_plot, ACC_plot, INS_plot, LTC_plot, MTC_plot, 
          SMC_plot, TP_plot, LPC_plot, PCC_plot, MIOC_plot, LOC_plot, HIP_plot, PHIP_plot, 
          MCC_plot, MPC_plot, AMG_plot, CAU_plot, PUT_plot, PAL_plot, THA_plot, 
          WM_plot, CER_plot, cols = 5)



