source('Utils/SummarisingFunctions.R', chdir = T)


VT_BP_data <- read_excel("~/Dropbox/RAALLPET_article/Revison_BBI/ALL_2TCM_input_function_SA_mass_fP_Included_subjects_170915.xlsx", 
              sheet = "VT_BPnd")

SA_mass_data <- read_excel("~/Dropbox/RAALLPET_article/Revison_BBI/ALL_2TCM_input_function_SA_mass_fP_Included_subjects_170915.xlsx", 
                           sheet = "SA_mass_etc")


k1 <- summarySEwithin(data = VT_BP_data, measurevar = "K1", withinvars = "Pollen_status", 
                        betweenvars = "Group", na.rm = T)[1:4,]

k1$k1_mean_se <- paste(round(k1$K1, digits = 2), " (", round(k1$se, digits = 2), ")", sep = "")

k2 <- summarySEwithin(data = VT_BP_data, measurevar = "k2", withinvars = "Pollen_status", 
                      betweenvars = "Group", na.rm = T)[1:4,]

k2$k2_mean_se <- paste(round(k2$k2, digits = 2), " (", round(k2$se, digits = 2), ")", sep = "")

k3 <- summarySEwithin(data = VT_BP_data, measurevar = "k3", withinvars = "Pollen_status", 
                      betweenvars = "Group", na.rm = T)[1:4,]

k3$k3_mean_se <- paste(round(k3$k3, digits = 2), " (", round(k3$se, digits = 2), ")", sep = "")

k4 <- summarySEwithin(data = VT_BP_data, measurevar = "k4", withinvars = "Pollen_status", 
                      betweenvars = "Group", na.rm = T)[1:4,]

k4$k4_mean_se <- paste(round(k4$k4, digits = 2), " (", round(k4$se, digits = 2), ")", sep = "")

R2 <- summarySEwithin(data = VT_BP_data, measurevar = "R2", withinvars = "Pollen_status", 
                      betweenvars = "Group", na.rm = T)[1:4,]

R2$R2_mean_se <- paste(round(R2$R2, digits = 2), " (", round(R2$se, digits = 2), ")", sep = "")


AUC <- summarySEwithin(data = VT_BP_data, measurevar = "AUC", withinvars = "Pollen_status", 
                       betweenvars = "Group", na.rm = T)[1:4,]

AUC$AUC_mean_se <- paste(round(AUC$AUC, digits = 0), " (", round(AUC$se, digits = 0), ")", sep = "")

SA_mass_data$SA <- as.numeric(SA_mass_data$`PET spec act (GBq/umol)`)


SA <- summarySEwithin(data = SA_mass_data, measurevar = "SA", withinvars = "Pollen_status", 
                       betweenvars = "Group", na.rm = T)

SA$SA_mean_se <- paste(round(SA$SA, digits = 0), " (", round(SA$se, digits = 0), ")", sep = "")

SA_mass_data$IM <- as.numeric(SA_mass_data$`PET inj mass (ugm)`)

IM <- summarySEwithin(data = SA_mass_data, measurevar = "IM", withinvars = "Pollen_status", 
                      betweenvars = "Group", na.rm = T)

IM$IM_mean_se <- paste(round(IM$IM, digits = 2), " (", round(IM$se, digits = 2), ")", sep = "")


FF <- summarySEwithin(data = SA_mass_data, measurevar = "Free fraction (%)", withinvars = "Pollen_status", 
                      betweenvars = "Group", na.rm = T)

FF$FF_mean_se <- paste(round(FF$`Free fraction (%)`, digits = 2), " (", round(FF$se, digits = 2), ")", sep = "")


All_variables <- cbind(k1, k2, k3, k4, AUC, R2, SA, IM, FF, all = T)

All_variables <- All_variables[,c("Group", "Pollen_status", "k1_mean_se","k2_mean_se","k3_mean_se", "k4_mean_se", "SA_mean_se", 
"AUC_mean_se", "IM_mean_se", "FF_mean_se")]

# Test difference 
anova(lme(AUC ~ Group*Pollen_status, data = VT_BP_data,
    random = list(~1|Subject), na.action = na.exclude))

SA_mass_data$FF <- SA_mass_data$`Free fraction (%)`

anova(lme(FF ~ Group*Pollen_status, data = SA_mass_data,
          random = list(~1|Subject), na.action = na.exclude))