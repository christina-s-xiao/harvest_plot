# Loading relevant packages
library(dplyr)

# Loading the data
data<-read_excel("harvest_plot_tutorial_csreview.xlsx")
data$direction_effect<-data$change
data2 = select(data, 1:3, 5:9)


# Converting variables to relevant formats
data2$outcome_mode<-as.factor(data2$outcome_mode)
data2$stat_sig<-factor((data2$stat_sig), levels = c("Yes", "No", "N/A"))
data2$quality<-factor((data2$quality), levels = c("low", "moderate", "high"))
data2$direction_effect<-factor((data2$direction_effect), levels = c("Decrease", "No change", "Increase"))
data2$cs_category<-factor((data2$cs_category), levels = c("Carrot", "Carrot and stick", "Stick"))
data2$quality<-as.numeric(data2$quality)



# Saving the file as an RDS file
saveRDS(data2, file = "harvest_plot_data.rds")
my_data<-readRDS(file = "harvest_plot_data.rds") # Read the new RDS file
