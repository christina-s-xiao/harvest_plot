
# Load the data
#data <- read_excel("~/Library/CloudStorage/OneDrive-UniversityofCambridge/Carrots and sticks/Harvest plot/harvest_plot_tutorial_csreview.xlsx")
data <- read_excel("C:/Users/rap59/OneDrive - University of Cambridge/200-299_research/201_collaborate&develop/201.17_harvest_blog/harvest_plot_tutorial_csreview.xlsx")

data$change_order <- factor(data$change, levels = c("Decrease", "No change", "Increase"))

data <- data %>%
  mutate(quality_value = case_when(
    quality == "high"   ~ 3,
    quality == "moderate" ~ 2,
    quality == "low"    ~ 1
  ))

data$stat_sig <- factor(data$stat_sig, levels = c("Yes", "No", "N/A"))


drive <- data[data$outcome_mode == 'Driving', ]

drive = drive %>%
  group_by(cs_category, change_order) %>%
  arrange(desc(quality_value), stat_sig, desc(n_interventions), .by_group = TRUE)


c = count(drive, cs_category, outcome_mode, .drop = TRUE)
c

n = max(c$n)

for(i in 1:n){
  
  if(i == 1){
    v = vector()
    v[i] = floor(n/2)
  }else{
    if((i %% 2) == 0){
      v[i] = v[i-1] + i-1
    }else{
      v[i] = v[i-1] -i+1
    }
  }
  rm(i)
}

decrease_ord = as.vector(1:n)
no_change_ord = v
increase_ord = as.vector(n:1)


l = list()
for(i in 1:nrow(c)){
  
  if(c$change_order[i]=="Decrease"){
    l[[i]] = decrease_ord[1:c$n[i]]
  }
  
  if(c$change_order[i]=="No change"){
    l[[i]] = no_change_ord[1:c$n[i]]
  }
  
  if(c$change_order[i]=="Increase"){
    l[[i]] = increase_ord[1:c$n[i]]
  }
}
l


l = l[!c$n==0]
drive$position = unlist(l)

all.equal(drive$position, drive$position)

cbPalette <- c("#264653","#2a9d8f","#f4a261")
drive_a<- ggplot(drive, aes(fill=stat_sig, y=quality_value, x=position, label = study_id)) + 
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
