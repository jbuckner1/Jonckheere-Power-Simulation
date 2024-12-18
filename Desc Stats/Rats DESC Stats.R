library(readxl)
library(ggplot2)
library(dplyr)
library(openxlsx)
cbPalette <- c("#E69F00","#56B4E9","#009E73","#D55E00","#F0E442","#0072B2","#CC79A7","#999999")

rats_import <- read_xlsx("combined_rats_v2.xlsx")
#View(rats)


rats <- rats_import %>% 
  mutate(Concentration = as.numeric(Concentration),
         Terminal_Body_Weight = as.numeric(Terminal_Body_Weight),
         Liver = as.numeric(Liver),
         Lungs = as.numeric(Lungs))

rats_control <- rats %>%
  filter(Group == 1)

rats <- rats %>% 
  mutate(Group=recode(Group,
                      "1"= "F1-1",
                      "2"= "F1-2",
                      "3"= "F1-3",
                      "4"= "F1-4",
                      "5"= "F1-5",
                      "6"= "F1-6"))

rats_female <- rats %>%
  filter(Sex == 'Female')

rats_male <- rats %>%
  filter(Sex == 'Male')

################################################################################

# Desc Stats by Chemical ---------------------------------------
#summary(rats)
desc_stats_chem <- rats %>%
  group_by(Chemical,Group, Sex) %>%  # Group by Chemical
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
    .groups = 'drop'  # Drop grouping after summarizing
  )

################################################################################
################################################################################

#Tables

################################################################################


#Terminal body weight by sex (only control group) 
terminalbw_control_desc_stats <- rats_control %>%
  group_by(Sex) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Terminal Body Wt" = mean(Terminal_Body_Weight),
    "Standard Deviation: Control Terminal Body Wt" = sd(Terminal_Body_Weight),
    "Standard Error: Control Terminal Body Wt" = sd(Terminal_Body_Weight)/sqrt(length(Terminal_Body_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(terminalbw_control_desc_stats)

#Terminal body weight by sex and group
terminalbw_grouped_desc_stats <- rats %>%
  group_by(Sex,Group) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Terminal Body Wt" = mean(Terminal_Body_Weight),
    "Standard Deviation: Terminal Body Wt" = sd(Terminal_Body_Weight),
    "Standard Error: Terminal Body Wt" = sd(Terminal_Body_Weight)/sqrt(length(Terminal_Body_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(terminalbw_grouped_desc_stats)

#Terminal body weight by sex and chemical (only control group) 
terminalbw_control_grouped_desc_stats <- rats_control %>%
  group_by(Sex,Chemical) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Terminal Body Wt" = mean(Terminal_Body_Weight),
    "Standard Deviation: Control Terminal Body Wt" = sd(Terminal_Body_Weight),
    "Standard Error: Control Terminal Body Wt" = sd(Terminal_Body_Weight)/sqrt(length(Terminal_Body_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(terminalbw_control_grouped_desc_stats)


################################################################################


#Liver weight by sex (only control group) 
liver_control_desc_stats <- rats_control %>%
  group_by(Sex) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Liver Wt" = mean(Liver),
    "Standard Deviation: Control Liver Wt" = sd(Liver),
    "Standard Error: Control Liver Wt" = sd(Liver)/sqrt(length(Liver)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(liver_control_desc_stats)

#Liver weight by sex and group
liver_grouped_desc_stats <- rats %>%
  group_by(Sex,Group) %>%  # Group by Chemical, Sex
  summarise(
    "N" = n(),
    "Mean: Liver Wt" = mean(Liver),
    "Standard Deviation: Liver Wt" = sd(Liver),
    "Standard Error: Liver Wt" = sd(Liver)/sqrt(length(Liver)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(liver_grouped_desc_stats)

#Liver weight by sex and chemical (only control group) 
liver_control_grouped_desc_stats <- rats_control %>%
  group_by(Sex,Chemical) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Liver Wt" = mean(Liver),
    "Standard Deviation: Control Liver Wt" = sd(Liver),
    "Standard Error: Control Liver Wt" = sd(Liver)/sqrt(length(Liver)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(liver_control_grouped_desc_stats)


################################################################################


#Relative Liver weight by sex by chemical
relativeliver_desc_stats <- rats %>%
  group_by(Chemical,Sex) %>%  # Group by Chemical, Sex
  summarise(
    "N" = n(),
    "Mean: Relative Liver Wt" = mean(Liver_Relative_Weight),
    "Standard Deviation: Relative Liver Wt" = sd(Liver_Relative_Weight),
    "Standard Error: Relative Liver Wt" = sd(Liver_Relative_Weight)/sqrt(length(Liver_Relative_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(relativeliver_desc_stats)

#Relative Liver weight by sex (only control group) 
relative_liver_control_desc_stats <- rats_control %>%
  group_by(Sex) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Relative Liver Wt" = mean(Liver_Relative_Weight),
    "Standard Deviation: Control Relative Liver Wt" = sd(Liver_Relative_Weight),
    "Standard Error: Control Relative Liver Wt" = sd(Liver_Relative_Weight)/sqrt(length(Liver_Relative_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(relative_liver_control_desc_stats)


################################################################################


#Lung weight by sex (only control group) 
lung_control_desc_stats <- rats_control %>%
  group_by(Sex) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Lung Wt" = mean(Lungs),
    "Standard Deviation: Control Lung Wt" = sd(Lungs),
    "Standard Error: Control Lung Wt" = sd(Lungs)/sqrt(length(Lungs)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(lung_control_desc_stats)

#Lung weight by sex and group
lung_grouped_desc_stats <- rats %>%
  group_by(Sex,Group) %>%  # Group by Chemical, Sex
  summarise(
    "N" = n(),
    "Mean: Lungs Wt" = mean(Lungs),
    "Standard Deviation: Lungs Wt" = sd(Lungs),
    "Standard Error: Lungs Wt" = sd(Lungs)/sqrt(length(Lungs)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(lung_grouped_desc_stats)

#Lung weight by sex (only control group) 
lung_control_grouped_desc_stats <- rats_control %>%
  group_by(Sex,Chemical) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Lung Wt" = mean(Lungs),
    "Standard Deviation: Control Lung Wt" = sd(Lungs),
    "Standard Error: Control Lung Wt" = sd(Lungs)/sqrt(length(Lungs)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(lung_control_grouped_desc_stats)


################################################################################


#Relative Lung weight by sex by chemical
relativelung_desc_stats <- rats %>%
  group_by(Chemical,Sex) %>%  # Group by Chemical, Sex
  summarise(
    "N" = n(),
    "Mean: Relative Lung Wt" = mean(Lung_Relative_Weight),
    "Standard Deviation: Relative Lung Wt" = sd(Lung_Relative_Weight),
    "Standard Error: Relative Lung Wt" = sd(Lung_Relative_Weight)/sqrt(length(Lung_Relative_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(relativelung_desc_stats)


#Relative Lung weight by sex (only control group) 
relative_lung_control_desc_stats <- rats_control %>%
  group_by(Sex) %>%  # Group by Sex
  summarise(
    "N" = n(),
    "Mean: Control Relative Lung Wt" = mean(Lung_Relative_Weight),
    "Standard Deviation: Control Relative Lung Wt" = sd(Lung_Relative_Weight),
    "Standard Error: Control Relative Lung Wt" = sd(Lung_Relative_Weight)/sqrt(length(Lung_Relative_Weight)),
    .groups = 'drop'  # Drop grouping after summarizing
  )
View(relative_lung_control_desc_stats)
