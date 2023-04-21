# Testing methods of sorting studies into the correct positions

# Load the R packages
library(readxl)
library(data.table)

# Load the data
data <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/Carrots and sticks/Harvest plot/harvest_plot_tutorial_csreview.xlsx")

# Convert variables to factors
names <- c('quality' ,'outcome_mode', 'change','stat_sig','cs_category')
data[,names] <- lapply(data[,names] , factor)
str(data)

# Change the order of the factors
data$quality <- factor(data$quality, levels = c("high", "moderate", "low"))
data$stat_sig <- factor(data$stat_sig, levels = c("Yes", "No", "N/A"))
data$change_order <- factor(data$change, levels = c("Decrease", "No change", "Increase"))

data <- data %>%
  mutate(quality_value = case_when(
    quality == "high"   ~ 3,
    quality == "moderate" ~ 2,
    quality == "low"    ~ 1
  ))

### Set to data table to make compatible with data.table package
setDT(data)

# Subset the data to focus on driving outcomes
drive <- data[data$outcome_mode == 'Driving', ]

################################################################
#                   Christina's attempt                        #
################################################################

# The following code first specifies a custom ordering function for the no change outcome, followed by an if else statement
# specifying the order for the decrease and increase change outcomes. 

# First we need to determine the maximum number of rows:
max_rows <- drive[, .(max_rows = max(.N)), by = .(change, cs_category)]
maximum_n <- as.numeric(max(max_rows$max_rows))

# Custom ordering function for the no change studies:
order_no_change <- function(n) {
  v <- numeric(n)
  
  for (i in 1:n) {
    if (i == 1) {
      v[i] = floor(maximum_n/2)
    } else {
      if ((i %% 2) == 0) {
        v[i] = v[i - 1] + i - 1
      } else {
        v[i] = v[i - 1] - i + 1
      }
    }
  }
  
  return(v)
}

# Apply the custom ordering function based on the change value
drive[order(quality, stat_sig, -n_interventions), position := {
  n <- .N
  change_value <- change[1]
  
  if (change_value == "Decrease") {
    seq_len(n)
  } else if (change_value == "Increase") {
    seq(maximum_n, maximum_n - n + 1)
  } else {
    order_no_change(n)
  }
}, by = .(cs_category, change)]

# Plot to see if it works:
cbPalette <- c("#264653","#2a9d8f","#f4a261")
drive_a<- ggplot(drive, aes(fill=stat_sig, y=quality_value, x=Position, label = study_id)) + 
  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values=cbPalette, na.translate=FALSE) 
drive_b<- drive_a + facet_grid(cs_category~change_order,  
                               switch = "y", space='free_x') + theme_minimal(base_size = 12) 
drive_c <- drive_b +  theme(strip.placement = "outside")+ scale_y_continuous("", limits=c(0,3.5)) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=12,  face = "bold"), 
        legend.text=element_text(size=10)) +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())
drive_d <- drive_c + guides(fill = guide_legend("Significant", reverse = FALSE, nrow = 3)) +
  theme(legend.position="right") + labs(title="Driving")
drive_final <- drive_d +  geom_text(aes(label=n_interventions), position=position_dodge(width=0.9), vjust=-0.4)

drive_final

