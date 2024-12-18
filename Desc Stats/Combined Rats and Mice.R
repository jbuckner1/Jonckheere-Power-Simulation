library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
rm(list=ls())
Rats <- read_xlsx("combined_rats_v2.xlsx")
Mice <- read_xlsx("DLH_Combined_Mice_Data.xlsx")

#M_SodiumMeta <- read_excel("M_Sodium Metavanadate_Individual_Animal_Organ_Weight_Data.xlsx",
                    #       sheet= "Data")
#M_TrisChl <- read_excel("M_tris(2-Chloroisopropy1)phosphate _Individual_Animal_Organ_Weight_Data.xlsx",
                   #     sheet="Data")
#M_VanSul <-  read_excel("M_Vanadyl Sulfate _Individual_Animal_Organ_Weight_Data.xlsx",
                   #     sheet ="Data")

#Begin grabbing descriptive statistics 
cbPalette <- c("#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")     
ggplot(Mice_control, aes(x = Sex, y=..count.., fill = Sex)) + 
  geom_histogram()+
  labs(title = 'Control Groups Terminal Body Weight')+
  scale_fill_manual(values = cbPalette)+
  theme_bw()




# Calculate summary statistics for each chemical
summary_stats <- Mice %>% 
  group_by(Chemical,Group, Sex, Selection, Concentration) %>%  # Group by Chemical
  summarise(
    mean_liver_weight = mean(Liver),
    sd_liver_weight = sd(Liver),
    std_error_liver_weight = sd(Liver)/sqrt(length(Liver)),
    mean_lung_weight = mean(Lungs),
    sd_lung_weight = sd(Lungs),
    std_error_lungs_weight = sd(Lungs)/sqrt(length(Lungs)),
    mean_relative_liver_weight = mean(Liver_Relative_Weight),
    sd_relative_liver_weight = sd(Liver_Relative_Weight),
    std_error_relative_liver_weight =sd(Liver_Relative_Weight)/sqrt(length(Liver_Relative_Weight)),
    mean_relative_lung_weight = mean(Lung_Relative_Weight),
    sd_relative_lung_weight = sd(Lung_Relative_Weight),
    std_error_relative_lung_weight= sd(Lung_Relative_Weight)/sqrt(length(Lung_Relative_Weight)),
    mean_terminal_weight = mean(`Terminal Body Weight`),
    sd_terminal_weight = sd(`Terminal Body Weight`),
    std_error_terminal_weight= sd(`Terminal Body Weight`)/sqrt(length(`Terminal Body Weight`)),
    .groups = 'drop'  # Drop grouping after summarising
  )
#FOR MICE
summary_stats <- Rats %>% 
  group_by(Chemical,Group, Sex, Selection, Concentration) %>%  # Group by Chemical
  summarise(
    mean_liver_weight = mean(Liver),
    sd_liver_weight = sd(Liver),
    std_error_liver_weight = sd(Liver)/sqrt(length(Liver)),
    mean_lung_weight = mean(Lungs),
    sd_lung_weight = sd(Lungs),
    std_error_lungs_weight = sd(Lungs)/sqrt(length(Lungs)),
    mean_relative_liver_weight = mean(Liver_Relative_Weight),
    sd_relative_liver_weight = sd(Liver_Relative_Weight),
    std_error_relative_liver_weight =sd(Liver_Relative_Weight)/sqrt(length(Liver_Relative_Weight)),
    mean_relative_lung_weight = mean(Lung_Relative_Weight),
    sd_relative_lung_weight = sd(Lung_Relative_Weight),
    std_error_relative_lung_weight= sd(Lung_Relative_Weight)/sqrt(length(Lung_Relative_Weight)),
    mean_terminal_weight = mean(Terminal_Body_Weight),
    sd_terminal_weight = sd(Terminal_Body_Weight),
    std_error_terminal_weight= sd(Terminal_Body_Weight)/sqrt(length(Terminal_Body_Weight)),
    .groups = 'drop'  # Drop grouping after summarising
  )

# View results
View(summary_stats)
print(summary_stats)
writexl::write_xlsx(summary_stats, "MiceSummaryStats.xlsx")
#Check corectness
#-------------------------------------------------------------------------------
#Distribution Liver Weight
ggplot(Mice, aes(x = as.factor(Group), y = Liver, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x") +  # Facet by Chemical, allow independent x-axes for each chemical
  labs(
    title = "Distribution of Liver Weight by Group for Each Chemical",
    x = "Group",
    y = "Liver Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )

#-------------------------------------------------------------------------------
#Distribution Lung weight
ggplot(Mice, aes(x = as.factor(Group), y = Lungs, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x") +  # Facet by Chemical, allow independent x-axes for each chemical
   labs(
    title = "Distribution of Lung Weight by Group for Each Chemical",
    x = "Group",
    y = "Lung Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )

#-------------------------------------------------------------------------------
#Distribution of Terminal Body Weight
ggplot(Mice, aes(x = as.factor(Group), y = `Terminal Body Weight`, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Terminal weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x") +  
  labs(
    title = "Distribution of Terminal Body Weight by Group for Each Chemical",
    x = "Group",
    y = "Terminal Body Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )


#-------------------------------------------------------------------------------
#Distribution of Relative Liver weight
ggplot(Mice, aes(x = as.factor(Group), y = Liver_Relative_Weight, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x") +  # Facet by Chemical, allow independent x-axes for each chemical
  labs(
    title = "Distribution of Relative Liver Weight by Group for Each Chemical",
    x = "Group",
    y = "Relative Liver Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )

#-------------------------------------------------------------------------------
#Distribution of Relative Lung weight
ggplot(Mice, aes(x = as.factor(Group), y = Lung_Relative_Weight, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x") +  # Facet by Chemical, allow independent x-axes for each chemical
  labs(
    title = "Distribution of Relative Lung Weight by Group for Each Chemical",
    x = "Group",
    y = "Relative Lung Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )

#-------------------------------------------------------------------------------
#VISUALIZATION FOR CONTOL GROUPS
Mice_control <- Mice %>% 
  filter(Group == "1")

#Control group boxplots for Liver weight
ggplot(Mice_control, aes(x = as.factor(Group), y = Liver, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x")+  # Facet by Chemical, allow independent x-axes for each chemical
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  labs(
    title = "Distribution of Liver Weight by Group for control Each Chemical",
    x = "Group",
    y = "Control Groups Liver Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )

#-------------------------------------------------------------------------------
#Control groups boxplot for lung weight
ggplot(Mice_control, aes(x = as.factor(Group), y = Lungs, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x")+  # Facet by Chemical, allow independent x-axes for each chemical
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  labs(
    title = "Distribution of Lung Weight for control Group for Each Chemical",
    x = "Group",
    y = "Control Groups Lung Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )

#-------------------------------------------------------------------------------
#Control group boxplot for Terminal Body Weight
ggplot(Mice_control, aes(x = as.factor(Group), y = `Terminal Body Weight`, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x")+  # Facet by Chemical, allow independent x-axes for each chemical
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  labs(
    title = "Distribution of Terminal Body Weight for control Group for Each Chemical",
    x = "Group",
    y = "Control Groups Terminal Body Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )
#------------------------------------------------------------------------------
#Control group boxplot for Relative Liver Weight
ggplot(Mice_control, aes(x = as.factor(Group), y = Liver_Relative_Weight, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x")+  # Facet by Chemical, allow independent x-axes for each chemical
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  labs(
    title = "Distribution of Relative Liver Weight for control Group for Each Chemical",
    x = "Group",
    y = "Control Groups Relative Liver Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )
#-------------------------------------------------------------------------------
#Control group boxplot for Relative Lung Weight
ggplot(Mice_control, aes(x = as.factor(Group), y = Lung_Relative_Weight, fill = Sex)) +
  geom_boxplot() +  # Creates boxplots for Liver weight by Concentration and Sex
  facet_wrap(~ Chemical, scales = "free_x")+  # Facet by Chemical, allow independent x-axes for each chemical
  scale_fill_viridis(discrete = TRUE, option = "C") + 
  labs(
    title = "Distribution of Relative Lung Weight for control Groups",
    x = "Group",
    y = "Control Groups Lung Weight"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    panel.spacing = unit(2, "lines"),  # Increase space between facets for clarity
    panel.border = element_rect(colour = "black", fill = NA, size = 1),  # Add black border around each facet
    strip.background = element_rect(colour = "black", fill = "gray90")  # Add a background and border to facet labels
  )
