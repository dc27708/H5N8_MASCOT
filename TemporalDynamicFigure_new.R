####Temporal dynamics of H5N8 2016-17 Figure
############################################

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)
library(TTR)
library(tidyquant)

# Read in data
flu <- read_csv("H5N8Outbreak.csv")

flu$DATE_SUSP <- as.Date(flu$DATE_SUSP,format="%d/%m/%Y")

remove <- c("DEUX-SEVRES", "HAUTE-GARONNE")

filter_dept <- filter(flu, !NOM_DEPT %in% remove)

summary(filter_dept)


by_date_new <- group_by(filter_dept, DATE_SUSP, NOM_DEPT)

tempd_new <- summarise(by_date_new,
                   count = n(),
                   DATE_SUSP
) 

#removing duplicates
tempd_new_dup <- tempd_new[! duplicated(tempd_new), ]


sum(tempd_new_dup$count)


ggplot(tempd_new_dup, aes(x = DATE_SUSP , y = count, fill = NOM_DEPT)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Départements", 
                    labels = c("Aveyron", 
                               "Gers", 
                               "Hautes-Pyrenees",
                               "Landes", 
                               "Lot-et-Garonne", 
                               "Pyrenees-Atlantiques",
                               "Tarn"),
                    values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", 
                      "#E6AB02", "#A6761D")) +
  xlab("Dates of suspicion (confirmed cases)") +
  ylab("Number of infected farms") +
  #ggtitle("(A) Daily spatio-temporal dynamics") + 
  theme_bw() +  # Black and white theme
theme(legend.position = c(0.2, 0.7)) +
  theme(legend.text=element_text(size=20), legend.title = element_text(size = 20), plot.title = element_text(size = 20)) +
  scale_x_date(breaks = 
                 as.Date(c("2016-11-28", "2017-01-04", "2017-02-01", 
                           "2017-02-14", "2017-02-21", "2017-03-23")),
    labels = date_format("%d-%m-%Y")) +
geom_vline(xintercept = as.Date(c("2017-01-04", "2017-02-14", "2017-02-21")),
           color="black", linetype="dashed", size = 1.0) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))
#+
#  theme(panel.background = element_blank(),
#        panel.grid.major = element_blank(), 
#        panel.grid.minor = element_blank(),
#        axis.line = element_line(colour = "black"),
#        panel.border = element_rect(colour = "black", fill=NA, size=2))

##########################################################################
##########################################################################

####Subsetting Landes and Gers from case count data
keep1 <- c("GERS")
keep2 <- c("LANDES")

tempd_gers<- filter(tempd_new_dup, NOM_DEPT %in% keep1)
tempd_landes<- filter(tempd_new_dup, NOM_DEPT %in% keep2)

# Read in estimated Ne data
GersNedat <- read_csv("GersNe1.csv")

###Reordering rows to flip the data
GersNeReord <- as.data.frame(GersNedat[(120:0), ])

###adding a column with dates
GersNeDated <- GersNeReord %>% 
  mutate(date = seq(from = as.Date("2016-11-28"), to = as.Date("2017-03-27"), by = 'day'),
         Dept = "GersNe")
###Removing last four extra rows
GersNeDated <- GersNeDated[-c(117:120),]

GersFinal <- GersNeDated %>%  select(median, CI_Lower, CI_Upper, mean, date, Dept)
colnames(GersFinal) <- c("MedianNe","CI_Lower","CI_Upper", "MeanNe", "Date", "Dept" )

####Joining Case number df with Ne df
joined_Gers <- left_join(GersFinal, tempd_gers, 
                           by = c("Date" = "DATE_SUSP"))

#joined_Gers$MeanNe <-as.numeric(joined_Gers$MeanNe)
#joined_Gers$MedianNe <-as.numeric(joined_Gers$MedianNe)


summary(joined_Gers) #To set MedianNe 0 - 0.5 and count 0 - 7, scale count by multiplying 0.5 / 8 to fit range of MeanNe

###Gers Ne and Case count plot overlaid
joined_Gers %>% ggplot() + 
  geom_bar(mapping = aes(x = Date, y = count * 0.5/7), stat = "identity") +
    scale_y_continuous(
    sec.axis = sec_axis(~ . * 7/0.5 , name = "Number of infected farms (Gers)")) +
  geom_ribbon(aes(x = Date, ymin = CI_Lower, ymax = CI_Upper),    # shadowing cnf intervals
              fill = "steelblue2", alpha=0.4) +
  geom_line(mapping = aes(x = Date, y = MedianNe, colour = "#CC0033"), size = 1, show.legend = FALSE) +
  ylab("Viral effective population size (Gers)") +
#  theme(axis.title = element_text(size = 14, face = "bold")
#  ) +
  scale_x_date(name = "Months (2016-17)", 
               breaks = seq.Date(as.Date("2016-11-01"), 
                                 as.Date("2017-3-30"), by = "1 month"), 
               labels = function(date){return(month(date, label = TRUE))}) +
  theme_bw() +
  theme(text = element_text(size=24), axis.text.x = element_text(angle = 90, hjust = 1))

# Read in estimated Landes Ne data
LandesNedat <- read_csv("LandesNe1.csv")

####Convert wide to long data form ---- Landes
#LandesNe_long <- LandesNedat %>%
#  tibble::rownames_to_column() %>%  
#  pivot_longer(-rowname) %>% 
#  pivot_wider(names_from=rowname, values_from=value) 

#colnames(LandesNe_long) <- LandesNe_long[1,]
#LandesNe_long <- LandesNe_long[-1,]

###Reordering rows to flip the data
LandesNeReord <- as.data.frame(LandesNedat[(120:0), ])

LandesNeDated <- LandesNeReord %>% 
  mutate(date = seq(from = as.Date("2016-11-28"), to = as.Date("2017-03-27"), by = 'day'), 
         Dept = "LandesNe")

LandesNeDated <- LandesNeDated[-c(117:120),]

LandesFinal <- LandesNeDated %>%  select(median, CI_Lower, CI_Upper, mean, date, Dept)
colnames(LandesFinal) <- c("MedianNe","CI_Lower","CI_Upper", "MeanNe", "Date", "Dept" )

####Joining Case number df with Ne df
joined_Landes <- left_join(LandesFinal, tempd_landes, 
                           by = c("Date" = "DATE_SUSP"))

#joined_Landes$MeanNe <-as.numeric(joined_Landes$MeanNe)


summary(joined_Landes) #To set MeanNe 0 - 3 and count 0 - 19, scale count by multiplying 3 / 20 to fit range of MeanNe



###Landes Ne and Case count plot overlaid
joined_Landes %>% ggplot() + 
  geom_bar(mapping = aes(x = Date, y = count * 3 / 19), stat = "identity") + 
  scale_y_continuous(
    sec.axis = sec_axis(~ . * 19 / 3 , name = "Number of infected farms (Landes)")) +
    geom_ribbon(aes(x = Date, ymin = CI_Lower, ymax = CI_Upper),    # shadowing cnf intervals
              fill = "steelblue2", alpha=0.4) +
  geom_line(mapping = aes(x = Date, y = MedianNe, colour = "#CC0033"), size = 1, show.legend = FALSE) +
  ylab("Viral effective population size (Landes)") +
  scale_x_date(name = "Months (2016-17)", breaks = seq.Date(as.Date("2016-11-01"), as.Date("2017-3-30"),
                                                            by = "1 month"), labels = function(date){return(month(date, label = TRUE))}) +
  theme(legend.position= c(0.2, 0.8)) +
  theme_bw() +
  theme(text = element_text(size=24),
        axis.text.x = element_text(angle = 90, hjust = 1))