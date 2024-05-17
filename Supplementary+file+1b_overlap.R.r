#Analysis for Patterns of spatial and temporal association between Zootoca vivipara, Anguis fragilis, 
#Vipera berus and Natrix helvetica at artificial refuges:
#OVERLAP



###Packages###
library(readxl)
library(rlang)
library(mosaic)
library(writexl)

#files
species_pairs <- c("Zv_Vb", "Zv_Nh", "Nh_Vb", "Af_Zv", "Af_Vb", "Af_Nh")
path <- "C:/Users/michaels.c/OneDrive - Zoological Society of London/Rick/Rick_Ovlp.xlsx" #user to update appropriately

Zv_Vb <- read_xlsx(path = path, sheet = "Zv_Vb")
Zv_Nh <- read_xlsx(path = path, sheet = "Zv_Nh")
Nh_Vb <- read_xlsx(path = path, sheet = "Nh_Vb")
Af_Zv <- read_xlsx(path = path, sheet = "Af_Zv")
Af_Vb <- read_xlsx(path = path, sheet = "Af_Vb")
Af_Nh <- read_xlsx(path = path, sheet = "Af_Nh")

data_list <- list(Zv_Vb, Zv_Nh, Nh_Vb, Af_Zv, Af_Vb, Af_Nh)

B <- 9999

#sequence is: 
#1. calculate mean observed count, 
#2. simulate observed counts under null hypotheses
#3. compare proportions
#4. calculate p value

#create table#

p_val_table_Ovlp <- data.frame(Species_Pair = species_pairs, Observed = NA, Expected = NA, p = NA, Direction = NA)

for (i in 1:length(data_list)) {
  
  df <- data_list[[i]] %>% mutate(sim = NA)
  Observed <- df$Observed
  # mean no. actually observed to overlap/cohabit
  mean_Obs_Ovlp <- mean(Observed)
  
 sim <- vector(mode = "numeric", length = nrow(df)) 
 sim_obs_Ovlp <- vector(mode = "numeric", length = B)
 mean_sim <- vector(mode = "numeric", length = B)
 #create 
  #simulated distribution of average observed counts under null hypothesis
  for (a in 1: length(sim_obs_Ovlp))  {
    
    for(j in 1:nrow(df)){
    sim[j] <- rbinom(1, df$no_tiles[j], df$y_ovlp[j])
    
    mean_sim[a] <- mean(sim)
    
    }
  sim_obs_Ovlp[a] <- mean_sim[a]
  }
 
 
 
    ##p_val - tow tailed, so multiply by 2
  
  prop_Ovlp<- prop(~mean_Obs_Ovlp <= sim_obs_Ovlp) 
  
  if(prop_Ovlp < 0.5) { 
    p_Ovlp <- prop_Ovlp*2
  } else {
    p_Ovlp <- (1-prop_Ovlp)*2
  }
  
  p_val_table_Ovlp$p[i] <- p_Ovlp
  p_val_table_Ovlp$Direction[i] <- ifelse(mean(sim_obs_Ovlp) > mean_Obs_Ovlp & p_val_table_Ovlp$p[i] < 0.05, "Lower", 
                                         ifelse(mean(sim_obs_Ovlp) < mean_Obs_Ovlp & p_val_table_Ovlp$p[i] < 0.05, "Higher", "-"))
  p_val_table_Ovlp$Observed[i] <- round(mean_Obs_Ovlp, digits = 3)
  p_val_table_Ovlp$Expected[i] <- round(mean(sim_obs_Ovlp), digits = 3)
 
}

p_val_table_Ovlp <- p_val_table_Ovlp %>% 
  mutate(p = round(as.numeric(p), digits = 3))%>%
  mutate(p = ifelse(p == 0, "<0.0001", p)) %>%
  mutate(Species_Pair = gsub("_", "/", Species_Pair))%>%
  rename(`Species Pair` = Species_Pair)


###export##
path_exp <- "C:/Users/michaels.c/OneDrive - Zoological Society of London/Rick/" #user to update appropriately
write_xlsx(p_val_table_Ovlp, path = paste0(path_exp, "p_val_table_Ovlp.xlsx"))
  
