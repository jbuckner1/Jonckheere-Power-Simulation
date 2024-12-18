library(readxl)
library(dplyr)
library(ggplot2)
library(viridis)
rm(list=ls())

Mice <- read_xlsx("DLH_Combined_Mice_Data.xlsx")


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


Mice_control <- Mice %>% 
  filter(Group == "1")


Mice_Lung_stats <- Mice %>%
  group_by(Chemical,Sex) %>%  # Group by Chemical
  summarise(
    "Count" = n(),
    "Mean" = mean(Lungs),
    "Standard Deviation" = sd(Lungs),
    "Standard Error" = sd(Lungs)/sqrt(length(Lungs)),
    .groups = 'drop'  # Drop grouping after summarizing
  )


Mice_Liver_stats <- Mice %>%
  group_by(Chemical,Sex) %>%  # Group by Chemical
  summarise(
    "Count" = n(),
    "Mean" = mean(Liver),
    "Standard Deviation" = sd(Liver),
    "Standard Error" = sd(Liver)/sqrt(length(Liver)),
    .groups = 'drop'  # Drop grouping after summarizing
  )


cbPalette <- c("#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999") 


#--------------BOXPLOT FOR VARIABLES GROUPED BY SEX-----------------------------


#Boxplot for Mice Terminal body
ggplot(Mice, aes(x = Sex, y=`Terminal Body Weight`, fill = Sex)) + 
  geom_boxplot()+
  labs(title = 'Mice Terminal Body Weight by Sex')+
  scale_fill_manual(values = cbPalette)+
  theme_bw()+
  theme(legend.position="none")

Mice_terminal <- Mice %>% 
  group_by(Sex) %>% 
  summarize(mean=sd(`Terminal Body Weight`)/sqrt(length(`Terminal Body Weight`)))


#Boxplot for Liver#Boxplot for LiverMice
ggplot(Mice, aes(x = Sex, y = Liver, fill = Sex)) + 
  geom_boxplot() +
  facet_wrap(~ Chemical, scales = "fixed")+
  labs(title = 'Mice Liver Weight by Sex', 
       x = 'Sex', 
       y = 'Liver Weight') +
  scale_fill_manual(values = cbPalette) +  # Apply custom color palette
  theme_bw()

#Boxplot for Lung weight
ggplot(Mice, aes(x = Sex, y = Lungs, fill = Sex)) + 
  geom_boxplot() +
  facet_wrap(~ Chemical, scales = "fixed")+
  labs(title = 'Mice Lung Weight by Sex', 
       x = 'Sex', 
       y = 'Lung Weight') +
  scale_fill_manual(values = cbPalette) +  # Apply custom color palette
  theme_bw()


#Boxplot for relative Liver weight
ggplot(Mice, aes(x = Sex, y = Liver_Relative_Weight, fill = Sex)) + 
  geom_boxplot(color = "black") +  # Create a boxplot
  facet_wrap(~ Chemical, scales = "fixed")+
  labs(title = 'Mice Liver Relative Weight by Sex', 
       x = 'Sex', 
       y = 'Liver_Relative_Weight') +
  scale_fill_manual(values = cbPalette) +  # Apply custom color palette
  theme_bw()


#Boxplot for Lung relative Weight
ggplot(Mice, aes(x = Sex, y = Lung_Relative_Weight, fill = Sex)) + 
  geom_boxplot(color = "black") +  # Create a boxplot
  facet_wrap(~ Chemical, scales = "fixed")+
  labs(title = 'Mice Lung Relative Weight by Sex', 
       x = 'Sex', 
       y = 'Lung_Relative_Weight') +
  scale_fill_manual(values = cbPalette) +  # Apply custom color palette
  theme_bw()


#-------------------------------------------------------------------------------
# Create Histograms to display the distribution of the Mice
#Lungs weight   
ggplot(Mice_control, aes(x = Lungs, fill = Sex))+
  scale_fill_manual(values = cbPalette)+
  geom_histogram(color = "black")+ 
  facet_wrap(~Sex)+
  labs(title = 'Mice Control Group Lung Weight by Sex')+
  theme_bw()


#Liver Weight
ggplot(Mice_control, aes(x = Liver, fill = Sex))+
  scale_fill_manual(values = cbPalette)+
  geom_histogram(color = "black")+ 
  facet_wrap(~Sex)+
  labs(title = 'Mice Control Group Liver Weight by Sex')+
  theme_bw()



#Histogram of Relative Lung Weight for each chemical and by sex
ggplot(Mice_control, aes(x = Lung_Relative_Weight, fill = Sex))+
  scale_fill_manual(values = cbPalette)+
  geom_histogram(color = "black")+ 
  facet_wrap(~Sex)+
  labs(title = 'Mice Control Group Lung Relative Weight by Sex')+
  theme_bw()


#Histogram of Relative Liver weight
ggplot(Mice_control, aes(x = Liver_Relative_Weight, fill = Sex))+
  scale_fill_manual(values = cbPalette)+
  geom_histogram(color = "black")+ 
  facet_wrap(~Sex)+
  labs(title = 'Mice Control Group Liver Relative Weight by Sex')+
  theme_bw()

#Terminal body weight histogram
ggplot(Mice_control, aes(x = `Terminal Body Weight`, fill = Sex))+
  scale_fill_manual(values = cbPalette)+
  geom_histogram(color = "black")+ 
  facet_wrap(~Sex)+
  labs(title = 'Mice Control Group Terminal Body Weight by Sex')+
  theme_bw()








#-------------------------------------------------------------------------------
#### NEW BOXPLOT GRAPHS FOR DESCRIPTIVE STATISTICS

####    MAKE FEMALE AND MALE SUBGROUPS FOR BOX PLOTS
Mice <- Mice %>% 
  mutate(Group=recode(Group,
    "1"= "F1-1",
    "2"= "F1-2",
    "3"= "F1-3",
    "4"= "F1-4",
    "5"= "F1-5",
    "6"= "F1-6"))
         


Mice_female <- Mice %>% 
  filter(Sex=="Female")

Mice_male <- Mice %>% 
  filter(Sex=="Male")


#-------------------------------------------------------------------------------
#####NEW BOX PLOT GRAPHS FOR DESCRIPTIVE STATISTICS
#TERMINAL BODY WEIGHT BY CHEMICAL, CONTROL GROUP
ggplot(Mice_control, aes(x = Sex, y = `Terminal Body Weight`, fill = Chemical)) + 
    geom_boxplot(color = "black") +  # Create a boxplot
    scale_fill_manual(values = cbPalette) +  # Apply custom color palette
    labs(title = 'Terminal Body Weights by Chemical', 
         subtitle = "Control Data",
         x = 'Sex', 
         y = 'Terminal Body Weight (g)') +
    theme_bw()

#FEMALE TERMINAL BODY WEIGHT BY CHEMICAL, CONTROL AND TREATED DATA, grouped by treatment 
ggplot(Mice_female, aes(x=Group, y=`Terminal Body Weight`, fill=Chemical))+
  geom_boxplot(color="black") +
  scale_fill_manual(values=cbPalette) +
  labs(title= "Female Terminal Body Weights by Chemical",
       subtitle="Control and Treated Data",
       x = "Treatment Group",
       y= "Terminal Body Weight (g)") +
  theme_bw()


#MALE TERMINAL BODY WEIGHTS BY CHEMICAL, CONTROL AND TREATED DATA, grouped by treatment 
ggplot(Mice_male, aes(x=Group, y=`Terminal Body Weight`, fill=Chemical))+
  geom_boxplot(color="black") +
  scale_fill_manual(values=cbPalette) +
  labs(title= "Male Terminal Body Weights by Chemical",
       subtitle="Control and Treated Data",
       x = "Treatment Group",
       y= "Terminal Body Weight (g)") +
  theme_bw()

#LIVER WEIGHT BY CHEMICAL, CONTROL GROUP
ggplot(Mice_control, aes(x = Sex, y = Liver, fill = Chemical)) + 
  geom_boxplot(color = "black") +  # Create a boxplot
  scale_fill_manual(values = cbPalette) +  # Apply custom color palette
  labs(title = 'Liver Weights by Chemical', 
       subtitle = "Control Data",
       x = 'Sex', 
       y = 'Liver Weight (g)') +
  theme_bw()


#FEMALE LIVER WEIGHT BY CHEMICAL, CONTROL AND TREATED DATA, grouped by treatment 
ggplot(Mice_female, aes(x=Group, y=Liver, fill=Chemical))+
  geom_boxplot(color="black") +
  scale_fill_manual(values=cbPalette) +
  labs(title= "Female Liver Weights by Chemical",
       subtitle="Control and Treated Data",
       x = "Treatment Group",
       y="Liver Weight (g)") +
  theme_bw()


#MALE LIVER WEIGHT BY CHEMICAL, CONTROL AND TREATED DATA, grouped by treatment 
ggplot(Mice_male, aes(x=Group, y=Liver, fill=Chemical))+
  geom_boxplot(color="black") +
  scale_fill_manual(values=cbPalette) +
  labs(title= "Male Liver Weights by Chemical",
       subtitle="Control and Treated Data",
       x = "Treatment Group",
       y= "Liver Weight (g)") +
  theme_bw()

#LUNG WEIGHT BY CHEMICAL, CONTROL GROUP
ggplot(Mice_control, aes(x = Sex, y = Lungs, fill = Chemical)) + 
  geom_boxplot(color = "black") +  # Create a boxplot
  scale_fill_manual(values = cbPalette) +  # Apply custom color palette
  labs(title = 'Lung Weights by Chemical',
       subtitle = "Control Data",
       x = 'Sex', 
       y = 'Lung Weight (g)') +
  theme_bw()

#FEMALE LUNG WEIGHT BY CHEMICAL, CONTROL AND TREATED DATA, grouped by treatment 
ggplot(Mice_female, aes(x=Group, y=Lungs, fill=Chemical))+
  geom_boxplot(color="black") +
  scale_fill_manual(values=cbPalette) +
  labs(title= "Female Lung Weights by Chemical",
       subtitle="Control and Treated Data",
       x = "Treatment Group",
       y= "Lung Weight (g)") +
  theme_bw()

#MALE LUNG WEIGHT BY CHEMICAL, CONTROL AND TREATED DATA, grouped by treamment 
ggplot(Mice_male, aes(x=Group, y=Lungs, fill=Chemical))+
  geom_boxplot(color="black") +
  scale_fill_manual(values=cbPalette) +
  labs(title= "Male Lung Weights by Chemical",
       subtitle="Control and Treated Data",
       x = "Treatment Group",
       y= "Lung Weight (g)") +
  theme_bw()
