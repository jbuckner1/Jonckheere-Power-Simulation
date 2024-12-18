# STS Practicum: DLH
# Simulating Data Sets
# Variables for sex, organ weight, and effect/scenario
# male liver weight

# dose group = trt
# effect = scenario
# load packages ----------------------------------------------------------------

library(ggplot2)
library(clinfun)
library(dplyr)
set.seed(2342)
Scenario <- 0
means <- 0

all_sim <- NULL


# male liver weight -------------------------------------------------------------

sim_mliver <- NULL
Sex <- 0 #Male
end_point <- "Liver Weight"
Mean <-17.188355 
SD <- 1.91288 



for(Scenario in c(0, .05, .15, .3)){
  for(trt in 0:3){
    means <- Mean*(1 - ((trt/3)*Scenario))
    stdev <- SD*(1 + ((trt/3)))
    weight <- data.frame(wt = rnorm(10*1000, means, SD),
                         Sex = Sex,
                         Scenario = Scenario,
                         trt = trt,
                         Mean = means,
                         stdev = stdev,
                         end_point = end_point,
                         animal_num = seq(1:10)
    )
    weight$iteration <- rep(seq(1:1000), 10)
    sim_mliver <- rbind(sim_mliver, weight) # Binds the weight data (last rnorm) onto previous ones
  }
}


# liver weight (f) -----------------------------------------------------------------------------

sim_fliver <- NULL
Sex <- 1 #Female
end_point <- "Liver Weight"
Mean <- 9.599439 
SD <- 1.203888  
for(Scenario in c(0, .05, .15, .3)){
  for(trt in 0:3){
    means <- Mean*(1 - ((trt/3)*Scenario))
    stdev <- SD*(1 + ((trt/3)))
    weight <- data.frame(wt = rnorm(10*1000, means, SD),
                         Sex = Sex,
                         Scenario = Scenario,
                         trt = trt,
                         Mean = means,
                         stdev = stdev,
                         end_point = end_point,
                         animal_num = seq(1:10)
    )
    weight$iteration <- rep(seq(1:1000), 10)
    sim_fliver <- rbind(sim_fliver, weight) # Binds the weight data (last rnorm) onto previous ones
  }
}


# terminal body weight (m) ---------------------------------------------------------------------
sim_mterminal <- NULL
Sex <- 0 #Male
end_point <- "Terminal Body Weight"
Mean <-436.9475 
SD <- 41.02570 
sim_mterminal <-NULL

for(Scenario in c(0, .05, .15, .3)){
  for(trt in 0:3){
    means <- Mean*(1 - ((trt/3)*Scenario))
    stdev <- SD*(1 + ((trt/3)))
    weight <- data.frame(wt = rnorm(10*1000, means, SD),
                         Sex = Sex,
                         Scenario = Scenario,
                         trt = trt,
                         Mean = means,
                         stdev = stdev,
                         end_point = end_point,
                         animal_num = seq(1:10)
    )
    weight$iteration <- rep(seq(1:1000), 10)
    sim_mterminal <- rbind(sim_mterminal, weight) # Binds the weight data (last rnorm) onto previous ones
  }
}



# terminal body weight (f) ---------------------------------------------------------------------

Sex <- 1 #female
end_point <- "Terminal Body Weight"
Mean <- 263.2289
SD <-  21.58574 
sim_fterminal<- NULL


for(Scenario in c(0, .05, .15, .3)){
  for(trt in 0:3){
    means <- Mean*(1 - ((trt/3)*Scenario))
    stdev <- SD*(1 + ((trt/3)))
    weight <- data.frame(wt = rnorm(10*1000, means, SD),
                         Sex = Sex,
                         Scenario = Scenario,
                         trt = trt,
                         Mean = means,
                         stdev = stdev,
                         end_point = end_point,
                         animal_num = seq(1:10)
    )
    weight$iteration <- rep(seq(1:1000), 10)
    sim_fterminal <- rbind(sim_fterminal, weight) # Binds the weight data (last rnorm) onto previous ones
  }
}





# lung weight (m) ---------------------------------------------------------


sim_mlung <- NULL
Sex <- 0 #male
end_point <- "Lung Weight"
Mean <- 2.273875 
SD <- .2692828   

for(Scenario in c(0, .05, .15, .3)){
  for(trt in 0:3){
    means <- Mean*(1 - ((trt/3)*Scenario))
    stdev <- SD*(1 + ((trt/3)))
    weight <- data.frame(wt = rnorm(10*1000, means, SD),
                         Sex = Sex,
                         Scenario = Scenario,
                         trt = trt,
                         Mean = means,
                         stdev = stdev,
                         end_point = end_point,
                         animal_num = seq(1:10)
    )
    weight$iteration <- rep(seq(1:1000), 10)
    sim_mlung <- rbind(sim_mlung, weight) # Binds the weight data (last rnorm) onto previous ones
  }
}



# lung weight (f) ---------------------------------------------------------

sim_flung <- NULL
Sex <- 1 #female
end_point <- "Lung Weight"
Mean <- 1.850887  
SD <- .3053128    
for(Scenario in c(0, .05, .15, .3)){
  for(trt in 0:3){
    means <- Mean*(1 - ((trt/3)*Scenario))
    stdev <- SD*(1 + ((trt/3)))
    weight <- data.frame(wt = rnorm(10*1000, means, SD),
                         Sex = Sex,
                         Scenario = Scenario,
                         trt = trt,
                         Mean = means,
                         stdev = stdev,
                         end_point = end_point,
                         animal_num = seq(1:10)
    )
    weight$iteration <- rep(seq(1:1000), 10)
    sim_flung <- rbind(sim_flung, weight) # Binds the weight data (last rnorm) onto previous ones
  }
}


# binding all datasets
rats_sim <- rbind(sim_fliver, sim_flung, sim_fterminal, sim_mterminal, sim_mliver, sim_mlung) %>% 
  select(animal_num, end_point, Sex, Scenario, trt,iteration, wt, Mean, stdev)

#saving as a csv ------------------------------------------------------------------

write.csv(rats_sim, "rats_sim_1000.csv")
#saving as a txt ------------------------------------------------------------------
#define file name
sink("rats_sim_1000.txt")

print(rats_sim)

#close the external connection
sink()
# Jonckheeres Test -----------------------------------------------------------------

#applying jonkheeres test for each scenario (40 obs each)
jpvals<- rats_sim%>%
  summarize(.by=c(Sex, Scenario, end_point, iteration),
            pval = jonckheere.test(wt, trt)$p.value)

# if pvals are <= 0.05, jpvals$testres is TRUE, else if FALSE

jpvals$testres <- jpvals$pval <= 0.05

# calculating power ------------------------------------------------------------

power_calc <- jpvals %>%
  group_by(Sex,Scenario, end_point) %>%
  summarize(TotRej = sum(testres),
            Power = TotRej/n())
write.csv(power_calc, "rats_sim_1000_power.csv")

# graphing p-vals -----------------------------------------------------------------

cbPalette <- c("#E69F00","#56B4E9","#009E73",
               "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

ggplot(jpvals, aes(x = pval))+
  geom_histogram(fill = '#68228B', color = "white")+ 
  labs(title = 'P-Values', 
       x = "P-Values")+
  theme_bw()+
  geom_vline(xintercept = 0.05)
