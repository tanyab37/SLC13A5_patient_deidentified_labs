```{r load libraries}
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
```

```{r read in your files}
setwd()

#ct = ifelse(grepl("analyte_value", labs), "numeric", "guess")
#read in your files
labs <- read_excel('Filename',
                   "clinical_lab_studies",
                   col_names = T)
labcorp <-read_excel('Filename',
                     'Sheet1',
                     col_names = T)
```

```{r boxplot, echo=FALSE}
lab_boxfxn2 <- function(l, u, data_file, lab_testing = 'analyte of interest', lab_unit = 'specific unit', plot_title = "title", y_unit = 'unit')
{#labref <- ref_file %>% filter(analyte == lab_testing)
  aoi <- data_file %>% filter(analyte == lab_testing &
                                analyte_unit == lab_unit) #filter for your analyte
  # aoi$patient_uuid <- factor(aoi$patient_uuid, levels = c()) #choose the order you want the labels to be
  aoi$analyte_value <- as.numeric(aoi$analyte_value) #change value to numeric
  aoi_plot <- ggplot(data = aoi, aes(x = analyte, y = n_only))+
    geom_boxplot(data = aoi, aes(x = analyte, y = analyte_value), size = 2, outlier.shape = NA, color = 'gray')+
    geom_jitter(data = aoi, aes(x=analyte, y=high_low, fill = patient_uuid), shape = 24, size = 3, width = 0.3, show.legend = F)+
    geom_jitter(data = aoi, aes(fill = patient_uuid), shape = 21, size = 3, width = 0.3, show.legend = F)+  
    scale_fill_manual(name = "Patient ID", 
                      values = c("186421b7-b4af-4957-bab8-3606c8d200d9" = "firebrick1" , 
                                 "5490aae6-8da2-4725-b443-7ea6629e76c2" = "orange1", 
                                 "63ddbe2a-b873-473b-95ac-5ccdbf97d205" = "bisque1", 
                                 "814ef3e2-31b6-4ff9-a78a-39f8431c59e3" ="forestgreen", 
                                 "849da59f-bb4b-4fda-86c6-50872319cd3e"="green3", 
                                 "89f773f3-f555-4fb1-8dcb-4f13fd650f01" = "olivedrab1",
                                 "a059aa7a-172b-4adf-87f0-7e5ccb1dc25e" = "slategray1", 
                                 "a07c9c6b-c5fd-4019-9aaf-b929b75578e4" ="turquoise1", 
                                 "a3f6df54-85e4-4d24-9257-06fcf285ab66" ="deepskyblue", 
                                 "a6e06826-5f25-433a-a6b5-90e488e56316" = "royalblue1", 
                                 "abd39c87-d605-4fdb-8af4-7144e7cc0f2a" ="blueviolet", 
                                 "bf85ec84-fd82-4cec-8e1d-a6df7affe3f5" ="purple1", 
                                 "ccaa1b61-1222-4227-9ee0-faf2573a4f22" ="mediumpurple1", 
                                 "d33fd465-ab47-4dd6-a529-cb298f0f6f1f" = "magenta1", 
                                 "faa5b9a7-a5d6-401d-9417-ff7a73f701e1"="lavenderblush"),
                      labels = c("186421b7-b4af-4957-bab8-3606c8d200d9" = "Patient 1", 
                                 "5490aae6-8da2-4725-b443-7ea6629e76c2" = "Patient 2", 
                                 "63ddbe2a-b873-473b-95ac-5ccdbf97d205" ="Patient 3", 
                                 "814ef3e2-31b6-4ff9-a78a-39f8431c59e3" ="Patient 4", 
                                 "849da59f-bb4b-4fda-86c6-50872319cd3e" = "Patient 5", 
                                 "89f773f3-f555-4fb1-8dcb-4f13fd650f01" ="Patient 6", 
                                 "a059aa7a-172b-4adf-87f0-7e5ccb1dc25e" ="Patient 7", 
                                 "a07c9c6b-c5fd-4019-9aaf-b929b75578e4" ="Patient 8", 
                                 "a3f6df54-85e4-4d24-9257-06fcf285ab66" ="Patient 9", 
                                 "a6e06826-5f25-433a-a6b5-90e488e56316" ="Patient 10", 
                                 "abd39c87-d605-4fdb-8af4-7144e7cc0f2a" ="Patient 11", 
                                 "bf85ec84-fd82-4cec-8e1d-a6df7affe3f5" ="Patient 12", 
                                 "ccaa1b61-1222-4227-9ee0-faf2573a4f22" ="Patient 13", 
                                 "d33fd465-ab47-4dd6-a529-cb298f0f6f1f" ="Patient 14", 
                                 "faa5b9a7-a5d6-401d-9417-ff7a73f701e1" ="Patient 15"))+
    ylim(l,u)+ #this needs to be changed per lab tested
    labs(title = plot_title,
         #subtitle = "by patient",
         #x = lab_testing,
         y = y_unit) +
    guides(col=guide_legend(nrow=2, byrow = TRUE))+
    theme_bw()+
    theme_classic()+
    theme(axis.title.x=element_blank())+
    #theme(legend.position = "bottom")+
    # axis.text.x = element_blank())+
    theme(plot.title = element_text(hjust = 0.5), 
          plot.subtitle = element_text(hjust = 0.5),
          text = element_text(size = 20))
  
  return(aoi_plot)}

```





