###################################################################
###################################################################
# MASCOT-GLM parameter estimation script

# This script plots predictor indicator probability
###################################################################
###################################################################
library(ggplot2)
# needed to get the node heights
library(phytools)
# needed to read the trees
library(ape)
library(coda)
library(grid)
library(gridExtra)
library(reshape2)
library (tidyverse)
library("ggpubr")
theme_set(
  theme_bw() +
    theme(legend.position = "top")
)

# clear workspace
rm(list = ls())


col0 <- rgb(red=0.0, green=0.4470,blue=0.7410)
col1 <- rgb(red=0.8500, green=0.3250,blue=0.0980)
col2 <- rgb(red=0.9290, green=0.6940,blue=0.1250)
col3 <- rgb(red=0.3010, green=0.7450,blue=0.9330)
col4 <- rgb(red=0.4660, green=0.6740,blue=0.1880)


# Set the directory to the directory of the file
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)


t <- read.table("trial_daily_2_combo.log", header=TRUE, sep="\t")
names(t)[names(t) == "NeGLM.scaler.obs_weekly_num_cases.1week"] <- "NeGLM.scaler.obs_weekly_num_cases_1week"
names(t)[names(t) == "NeGLM.scaler.Mov.Within.Dep"] <- "NeGLM.scaler.MovWithinDept"
names(t)[names(t) == "migrationGLM.scaler.Mov.Between.Dep"] <- "migrationGLM.scaler.MovBetDepts"

t.labels = labels(t)
migration.predictors = data.frame(Sample=t$Sample)
ne.predictors = data.frame(Sample=t$Sample)

for (i in seq(1,length(t.labels[[2]]))){
  if (startsWith(t.labels[[2]][i], 'migrationGLM.scaler')){
    name = strsplit(t.labels[[2]][i], split="\\.")[[1]][3]
    migration.predictors[,name] = t[, t.labels[[2]][i]]
  }
  if (startsWith(t.labels[[2]][i], 'NeGLM.scaler')){
    name = strsplit(t.labels[[2]][i], split="\\.")[[1]][3]
    ne.predictors[,name] = t[, t.labels[[2]][i]]
  }
}

########Migration predictors
############################

# make a dataframe with the support for incidence predictors
dat.mig = data.frame(x=c("Density of chicken farms", 
                         "Density of duck farms",
                         "Density of humans", 
                         "Movement between depts", 
                         "Geographic distance",
                         "Shared borders", 
                         "Case num./dept", 
                         "4 Jan. (Before v After)",
                         "14 Feb. (Before v After)", 
                         "21 Feb. (Before v After)", 
                         "Tarn as contributor"), 
                     y=c(
                       length(which(migration.predictors[,"ChickenFarmDensityAtOrigin"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"DuckFarmDensityAtOrigin"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"HumanDensityAtOrigin"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"MovBetDepts"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"DeptsGreatCircleDist"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"DeptsSharedBorder"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"IncidenceAtOrigin"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"RegionalDepopulation4Jan"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"RegionalDepopulation14Feb"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"RegionalDepopulation21Feb"]!=0))/length(migration.predictors$Sample),
                       length(which(migration.predictors[,"TarnSource"]!=0))/length(migration.predictors$Sample)),
                     
                     direction=c(
                       median(migration.predictors[which(migration.predictors[,"ChickenFarmDensityAtOrigin"]!=0),"ChickenFarmDensityAtOrigin"])>0,
                       median(migration.predictors[which(migration.predictors[,"DuckFarmDensityAtOrigin"]!=0),"DuckFarmDensityAtOrigin"])>0,
                       median(migration.predictors[which(migration.predictors[,"HumanDensityAtOrigin"]!=0),"HumanDensityAtOrigin"])>0,
                       median(migration.predictors[which(migration.predictors[,"MovBetDepts"]!=0),"MovBetDepts"])>0,
                       median(migration.predictors[which(migration.predictors[,"DeptsGreatCircleDist"]!=0),"DeptsGreatCircleDist"])>0,
                       median(migration.predictors[which(migration.predictors[,"DeptsSharedBorder"]!=0),"DeptsSharedBorder"])>0,
                       median(migration.predictors[which(migration.predictors[,"IncidenceAtOrigin"]!=0),"IncidenceAtOrigin"])>0,
                       median(migration.predictors[which(migration.predictors[,"RegionalDepopulation4Jan"]!=0),"RegionalDepopulation4Jan"])>0,
                       median(migration.predictors[which(migration.predictors[,"RegionalDepopulation14Feb"]!=0),"RegionalDepopulation14Feb"])>0,
                       median(migration.predictors[which(migration.predictors[,"RegionalDepopulation21Feb"]!=0),"RegionalDepopulation21Feb"])>0,
                       median(migration.predictors[which(migration.predictors[,"TarnSource"]!=0),"TarnSource"])>0))


ind_mig <- ggplot(dat.mig) + 
  geom_bar(aes(x= x,y=y), stat="identity", colour="black") +
  xlab("") +
  scale_x_discrete(expand = c(0.05, 0), limits = c("Tarn as contributor",
                                                   "Density of chicken farms", 
                                                     "Density of duck farms",
                                                     "Density of humans", 
                                                     "Movement between depts", 
                                                     "Geographic distance",
                                                     "Case num./dept", 
                                                   "Shared borders", 
                                                   "21 Feb. (Before v After)", 
                                                   "14 Feb. (Before v After)", 
                                                   "4 Jan. (Before v After)")) +
  scale_y_continuous(# Features of the first axis
    name = "Posterior probability",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~., name="Bayes Factor (K)", 
                         breaks = c(0.131,0.2255, 0.32, 0.44),
                         labels = c("3.2","Substantial", "10", "Strong")),
    limits = c(0,0.5)
  ) +
  geom_hline(yintercept = c(0.131, 0.32), color="black", linetype="dashed", size = 1.0) +
  coord_flip() +
  #ggtitle("Viral Ne predictors") +      # All font sizes
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  theme(text = element_text(size = 35)) +  theme(axis.ticks.x=element_blank())

ggsave(plot= ind_mig ,paste("H5N8MigIndicators_trial_daily_2_combo.png", sep=""),width= 30, height= 17)



##Organising data for coefficients plot

migration.predictors1 <-migration.predictors
migration.predictors1[migration.predictors1 == 0] <- NA #converting 0s to NA
migration.predictors1[1, 1] <-0 #Converting the first value to 0


#preparing dataframe for boxplot #Renaming variables
migration.predictors1 <- migration.predictors1 %>% 
  rename("Density of chicken farms" = "ChickenFarmDensityAtOrigin", 
         "Density of duck farms" = "DuckFarmDensityAtOrigin",
         "Density of humans" = "HumanDensityAtOrigin", 
         "Movement between depts" = "MovBetDepts", 
         "Geographic distance" = "DeptsGreatCircleDist",
         "Shared borders" = "DeptsSharedBorder", 
         "Case num./dept" = "IncidenceAtOrigin", 
         "4 Jan. (Before v After)" = "RegionalDepopulation4Jan",
         "14 Feb. (Before v After)" = "RegionalDepopulation14Feb", 
         "21 Feb. (Before v After)" = "RegionalDepopulation21Feb", 
         "Tarn as contributor" = "TarnSource")  

dat.mig2<- melt(migration.predictors1,id.vars='Sample', 
                measure.vars=c("Tarn as contributor",
                               "Density of chicken farms", 
                               "Density of duck farms",
                               "Density of humans", 
                               "Movement between depts", 
                               "Geographic distance",
                               "Case num./dept", 
                               "Shared borders", 
                               "21 Feb. (Before v After)", 
                               "14 Feb. (Before v After)", 
                               "4 Jan. (Before v After)"))

#Plotting coefficients as boxplots and kernel distribution as violin plot
coeff_mig <- ggplot(dat.mig2) +
  xlab("") +
  geom_violin(aes(x= variable, y=value, fill=variable), width = 1.5, trim = FALSE) + 
  geom_boxplot(aes(x= variable, y=value), width = 0.4) +
  ylim(-2.5, 2.5) +
  geom_hline(yintercept = 0, color="black", linetype="dashed", size = 1)+
  ylab("Coefficients") +
  #ggtitle("Viral migration predictors") +
 theme_bw() + theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  theme(text = element_text(size = 35), legend.position = "none") + # All font sizes
  coord_flip()

ggsave(plot = coeff_mig, paste("H5N8MigCoefficients_trial_daily_2_combo.png", sep=""),width=30, height=17)


########Ne predictors
############################

# make a dataframe for Ne predictors
dat = data.frame(x= c("Daily case num.",
                      "Sampling delay of a week",
                      "Density of duck farms",
                      "Density of chicken farms",
                      "Density of humans",
                      "Movement within Dept"),
                 y= c(length(which(ne.predictors[,"obs_weekly_num_cases"]!=0))/length(ne.predictors$Sample),
                      length(which(ne.predictors[,"obs_weekly_num_cases_1week"]!=0))/length(ne.predictors$Sample),
                      length(which(ne.predictors[,"DuckFarmDen"]!=0))/length(ne.predictors$Sample),
                      length(which(ne.predictors[,"ChickenFarmDen"]!=0))/length(ne.predictors$Sample),
                      length(which(ne.predictors[,"HumanDen"]!=0))/length(ne.predictors$Sample),
                      length(which(ne.predictors[,"MovWithinDept"]!=0))/length(ne.predictors$Sample)),
                  direction = c(median(ne.predictors[which(ne.predictors[,"obs_weekly_num_cases"]!=0),"obs_weekly_num_cases"])>0,
                                median(ne.predictors[which(ne.predictors[,"obs_weekly_num_cases_1week"]!=0),"obs_weekly_num_cases_1week"])>0,
                                median(ne.predictors[which(ne.predictors[,"DuckFarmDen"]!=0),"DuckFarmDen"])>0,
                                median(ne.predictors[which(ne.predictors[,"ChickenFarmDen"]!=0),"ChickenFarmDen"])>0,
                                median(ne.predictors[which(ne.predictors[,"HumanDen"]!=0),"HumanDen"])>0,
                                median(ne.predictors[which(ne.predictors[,"MovWithinDept"]!=0),"MovWithinDept"])>0))


#Plot indicators and Ne predictors
ind_ne <- ggplot(dat) + 
  geom_bar(aes(x=x,y=y), stat="identity", colour="black") +
  xlab("") +
  scale_x_discrete(expand = c(0.05, 0), limits = c("Sampling delay of a week",
                                                   "Density of chicken farms",
                                                   "Density of humans",
                                                   "Density of duck farms",
                                                   "Movement within Dept",
                                                   "Daily case num.")) +
  scale_y_continuous(# Features of the first axis
    name = "Posterior probability",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~., name="Bayes Factor(K)", 
                         breaks = c(0.23, 0.3525, 0.475, 0.6875, 0.9, 0.99),  #Bayes factors and midpoints between two Bayes Factor cutoffs
                         labels = c("3.2", "Substantial","10", "Strong", "100", "Decisive")),
    limits = c(0,1)
  ) +
  geom_hline(yintercept = c(0.23, 0.475, 0.9), color="black", linetype="dashed", size = 1.0) + #Bayes factor cutoffs
  coord_flip() +
  #ggtitle("Viral Ne predictors") +      # All font sizes
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )  +
  theme(text = element_text(size = 35)) +  theme(axis.ticks.x=element_blank())

ggsave(plot= ind_ne ,paste("H5N8NeIndicators_trial_daily_2_combo.png", sep=""),width= 25, height= 12)


##Organising data for coefficients plot

ne.predictors1 <-ne.predictors
ne.predictors1[ne.predictors1 == 0] <- NA #cobverting 0s to NA
ne.predictors1[1, 1] <-0 #Converting the first value to 0

#preparing dataframe for boxplot
#Renaming variables
ne.predictors1 <- ne.predictors1 %>% 
  rename("Daily case num." = "obs_weekly_num_cases",
         "Sampling delay of a week" = "obs_weekly_num_cases_1week",
         "Density of duck farms" = "DuckFarmDen",
         "Density of chicken farms" = "ChickenFarmDen",
         "Density of humans" = "HumanDen",
         "Movement within Dept" = "MovWithinDept")

dat2<- melt(ne.predictors1,id.vars='Sample', measure.vars=c("Sampling delay of a week",
                                                            "Density of chicken farms",
                                                            "Density of humans",
                                                            "Density of duck farms",
                                                            "Movement within Dept",
                                                            "Daily case num."))

#Plotting coefficients as boxplots and kernel distribution as violin plot
coeff_ne <- ggplot(dat2) +
  xlab("") +
  geom_violin(aes(x=variable, y=value, fill=variable), width = 1, trim = FALSE) + 
  geom_boxplot(aes(x=variable, y=value), width = 0.4) +
  ylim(-2.5, 2.5) +
  geom_hline(yintercept = 0, color="black", linetype="dashed", size = 1)+
  ylab("Coefficients") +
  #ggtitle("Viral Ne predictors") +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  ) +
  theme(text = element_text(size = 35), legend.position = "none") + # All font sizes
  coord_flip()

ggsave(plot= coeff_ne ,paste("H5N8NeCoefficients_trial_daily_2_combo.png", sep=""),width= 25, height= 12)

##########################################################################################
#########################################################################################

figure3 <- ggarrange(ind_mig, coeff_mig, ind_ne, coeff_ne,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
figure3

