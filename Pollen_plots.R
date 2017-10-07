require(ggplot2)
require(gridExtra)
require(cowplot)
source('Utils/Multiplot.R', chdir = T)


load("PET_VT_93_2TCM_3exp.RData")
levels(Data_93$Group)[levels(Data_93$Group) == 'All'] <- 'Allergy'
levels(Data_93$Group)[levels(Data_93$Group) == 'Ctrl-all'] <- 'Control'

cbbPalette <- c( "#D55E00", "#000000", "#009E73", "#56B4E9", "#CC79A7", "#F0E442", "#0072B2", "#CC79A7")

# 2012
Pollen_data_2012 <- read_excel("~/Desktop/RAALLPET/Pollen_data.xlsx", sheet = 1)
Pollen_data_2012 <- Pollen_data_2012[ , c(1,3,6:7)]
names(Pollen_data_2012)[names(Pollen_data_2012) == 'Birch'] <- 'Pollen: Birch'
names(Pollen_data_2012)[names(Pollen_data_2012) == 'Grass'] <- 'Pollen: Grass'
names(Pollen_data_2012)[names(Pollen_data_2012) == 'Mugwort'] <- 'Pollen: Mugwort'

Pollen_data_2012$Date <- as.Date(Pollen_data_2012$Date)

Data_long_2012 <- melt(Pollen_data_2012, id.vars = "Date")

Plot_2012 <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2013-01-01")), 
             aes(x = PET.date, y = 500, colour = factor(Group))) +
  geom_line(data = Data_long_2012, aes(x = Date, y = value, colour = factor(variable)))+
  scale_colour_manual(values=cbbPalette)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-01"))), linetype = 4)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-01"))), linetype = 4)+ 
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")

Plot_2012_birch <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2013-01-01")), 
             aes(x = PET.date, y = 500, colour = factor(Group))) +
  ylim(0, 4000)+
  geom_line(data = subset(Data_long_2012, variable == "Pollen: Birch"), aes(x = Date, y = value),
            colour = "#009E73")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-01"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-01"))), linetype = 3)+ 
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 100, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 1000, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Birch 2012")

Plot_2012_grass <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2013-01-01")), 
             aes(x = PET.date, y = 20, colour = factor(Group))) +
  geom_line(data = subset(Data_long_2012, variable == "Pollen: Grass"), aes(x = Date, y = value),
            colour = "#56B4E9")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-01"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-01"))), linetype = 3)+ 
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 30, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 80, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Grass 2012")


Plot_2012_mugwort <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2013-01-01")), 
             aes(x = PET.date, y = 5, colour = factor(Group))) +
  geom_line(data = subset(Data_long_2012, variable == "Pollen: Mugwort"), aes(x = Date, y = value),
            colour = "#CC79A7")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-04-01"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2012-08-01"))), linetype = 3)+
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 30, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 80, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Mugwort 2012")



# 2013
Pollen_data_2013 <- read_excel("~/Desktop/RAALLPET/Pollen_data.xlsx", sheet = 2)

Pollen_data_2013 <- Pollen_data_2013[ , c(1,3,6:7)]
names(Pollen_data_2013)[names(Pollen_data_2013) == 'Birch'] <- 'Pollen: Birch'
names(Pollen_data_2013)[names(Pollen_data_2013) == 'Grass'] <- 'Pollen: Grass'
names(Pollen_data_2013)[names(Pollen_data_2013) == 'Mugwort'] <- 'Pollen: Mugwort'

Pollen_data_2013$Date <- as.Date(Pollen_data_2013$Date)

Data_long_2013 <- melt(Pollen_data_2013, id.vars = "Date")

Plot_2013 <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-01-01") & PET.date > as.Date("2012-12-30")), 
             aes(x = PET.date, y = 50, colour = factor(Group))) +
  geom_line(data = Data_long_2013, aes(x = Date, y = value, colour = factor(variable)))+
  scale_colour_manual(values=cbbPalette)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-03-30"))), linetype = 4)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-09-01"))), linetype = 4)+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")

Plot_2013_birch <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-01-01") & PET.date > as.Date("2012-12-30")), 
             aes(x = PET.date, y = 50, colour = factor(Group))) +
  ylim(0, 100)+
  geom_line(data = subset(Data_long_2013, variable == "Pollen: Birch"), aes(x = Date, y = value),
            colour = "#009E73")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-03-30"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-09-01"))), linetype = 3)+ 
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 100, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 1000, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Birch 2013*")

Plot_2013_grass <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-01-01") & PET.date > as.Date("2012-12-30")), 
             aes(x = PET.date, y = 20, colour = factor(Group))) +
  geom_line(data = subset(Data_long_2013, variable == "Pollen: Grass"), aes(x = Date, y = value),
            colour = "#56B4E9")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-03-30"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-09-01"))), linetype = 3)+ 
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 30, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 80, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Grass 2013")


Plot_2013_mugwort <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-01-01") & PET.date > as.Date("2012-12-30")), 
             aes(x = PET.date, y = 5, colour = factor(Group))) +
  geom_line(data = subset(Data_long_2013, variable == "Pollen: Mugwort"), aes(x = Date, y = value),
            colour = "#CC79A7")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-03-30"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2013-09-01"))), linetype = 3)+
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 30, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 80, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Mugwort 2013")


# 2014
Pollen_data_2014 <- read_excel("~/Desktop/RAALLPET/Pollen_data.xlsx", sheet = 3)
Pollen_data_2014 <- Pollen_data_2014[ , c(1,3,6:7)]
names(Pollen_data_2014)[names(Pollen_data_2014) == 'Birch'] <- 'Pollen: Birch'
names(Pollen_data_2014)[names(Pollen_data_2014) == 'Grass'] <- 'Pollen: Grass'
names(Pollen_data_2014)[names(Pollen_data_2014) == 'Mugwort'] <- 'Pollen: Mugwort'

Pollen_data_2014$Date <- as.Date(Pollen_data_2014$Date)

Data_long_2014 <- melt(Pollen_data_2014, id.vars = "Date")

Plot_2014 <- ggplot()+
  geom_point(data = subset(Data_93, PET.date > as.Date("2013-12-30")), 
             aes(x = PET.date, y = 500, colour = factor(Group))) +
  geom_line(data = Data_long_2014, aes(x = Date, y = value, colour = factor(variable)))+
  scale_colour_manual(values=cbbPalette)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-04-01"))), linetype = 4)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-08-01"))), linetype = 4)+ 
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")


Plot_2014_birch <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-12-12") & PET.date > as.Date("2013-12-30")), 
             aes(x = PET.date, y = 500, colour = factor(Group))) +
  ylim(0, 4000)+
  geom_line(data = subset(Data_long_2014, variable == "Pollen: Birch"), aes(x = Date, y = value),
            colour = "#009E73")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-04-01"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-08-01"))), linetype = 3)+ 
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 100, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 1000, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Birch 2014")

Plot_2014_grass <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-12-12") & PET.date > as.Date("2013-12-30")), 
             aes(x = PET.date, y = 20, colour = factor(Group))) +
  geom_line(data = subset(Data_long_2014, variable == "Pollen: Grass"), aes(x = Date, y = value),
            colour = "#56B4E9")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-04-01"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-08-01"))), linetype = 3)+ 
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 30, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 80, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Grass 2014")


Plot_2014_mugwort <- ggplot()+
  geom_point(data = subset(Data_93, PET.date < as.Date("2014-12-12") & PET.date > as.Date("2013-12-30")), 
             aes(x = PET.date, y = 5, colour = factor(Group))) +
  geom_line(data = subset(Data_long_2014, variable == "Pollen: Mugwort"), aes(x = Date, y = value),
            colour = "#CC79A7")+
  scale_color_manual(name = "Group", 
                     breaks=c("Allergy", "Control"),
                     labels=c("Allergy", "Healthy subjects"), values = c("#D55E00", "#0072B2")) +
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-04-01"))), linetype = 3)+
  geom_vline(aes(xintercept = as.numeric(as.Date("2014-08-01"))), linetype = 3)+
  geom_hline(aes(yintercept= 10, linetype = "Moderate"), colour= 'red') +
  geom_hline(aes(yintercept= 30, linetype = "High"), colour= 'blue') +
  geom_hline(aes(yintercept= 80, linetype = "Very high"), colour= 'violet') +
  scale_linetype_manual(name = "Pollen levels", values = c(2, 2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red", "violet"))))+
  ylab(expression(paste("Pollen/", m^3, "/24h")))+
  xlab("PET date")+
  ggtitle("Mugwort 2014")




ggdraw() +
  draw_plot(Plot_2012_birch, 0, 2/3, 1/3, 1/3) +
  draw_plot(Plot_2013_birch, 1/3, 2/3, 1/3, 1/3) +
  draw_plot(Plot_2014_birch, 2/3, 2/3, 1/3, 1/3) +
  draw_plot(Plot_2012_grass, 0, 1/3, 1/3, 1/3) +
  draw_plot(Plot_2013_grass, 1/3, 1/3, 1/3, 1/3) +
  draw_plot(Plot_2014_grass, 2/3, 1/3, 1/3, 1/3) +
  draw_plot(Plot_2012_mugwort, 0, 0, 1/3, 1/3) +
  draw_plot(Plot_2013_mugwort, 1/3, 0, 1/3, 1/3) +
  draw_plot(Plot_2014_mugwort, 2/3, 0, 1/3, 1/3) 
