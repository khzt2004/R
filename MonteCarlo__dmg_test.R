library(MonteCarlo)
library(tidyverse)
library(psych)

# cl <- makeCluster(3, type = "SOCK") 
dmg_per_shot <- 12000
crit_dmg <- 0.99
shots_per_min <- 240
n <- 100

critdmg_test<-function(no_obs, prob){
  dmg_per_shot <- ifelse(prob == 1,
                         (1+crit_dmg)*shots_per_min*dmg_per_shot,
                         shots_per_min*dmg_per_shot)
  return(list("dmg_per_shot"=dmg_per_shot))
  
}

 param_list=list(no_obs=seq(1,n),
                 prob = sample(c(0,1), size=n, replace=TRUE, prob=c((1-0.147),0.147)))

MC_result<-MonteCarlo(func=critdmg_test, 
                      nrep=1, 
                      param_list=param_list,
                      max_grid = 100000
                      # ncpus=3
                      )

df<-MakeFrame(MC_result)
head(df)


########## dataframe method #############

crit_pct <- 0.19
prob <- sample(c(0,1), size=1000000, replace=TRUE, prob=c((1-crit_pct),crit_pct))
prob_table <- data_frame(prob)
prob_table <- prob_table %>% 
  mutate(dmg_per_shot = dmg_per_shot,
         crit_dmg = crit_dmg,
         shots_per_min = shots_per_min) %>% 
  mutate(final_dmgpershot = ifelse(prob == 1,
                                   (1+crit_dmg)*shots_per_min*dmg_per_shot,
                                   shots_per_min*dmg_per_shot))

ggplot(data=prob_table, aes(prob_table$final_dmgpershot)) + geom_histogram()
describe(prob_table)

