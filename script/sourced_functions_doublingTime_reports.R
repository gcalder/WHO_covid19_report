# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#        WHO COVID19 REPORT       #
#         SOURCED SCRIPT          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# LOADED PACKAGED

# FUNCTIONS USED IN Dt CALCULATIONS & CI
# data.cleaner()
# compute.td.m1.v2()
# sim.epi()
# Td.lapply()


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(readxl); library(ggplot2); library(dplyr); library(tidyr); library(rdrop2); library(Rmisc); library(lubridate);library("shiny"); library("rsconnect"); library(tidyverse); library(plotly);library(yarrr); library(tidyverse);library(knitr);library(RColorBrewer);library(plyr);library(utils);library(httr);library(tidyverse);library(magrittr);library(sf);library(RColorBrewer);library(classInt);library(geojson);library(cartography);library(magick);library(geojsonio)


# Function to iterate smoothing out of negative values!
iterate_cleaning <- function(time_series_data){
  while(sum(time_series_data$num_new_obs < 0) != 0){ 
    
    time_series_data <- data.cleaner(time_series_data)
    
  }
  time_series_data
}

data.cleaner<- function(df){
  
  df$date<- as.Date(df$date); df.clean<- df
  
  # Flag problematic datapoints
  negs<- df$date[which(df$num_new_obs < 0)];
  flags<- c(negs, negs - 1);
  df.clean$flag<- ifelse(df.clean$date %in% flags, 'flag', 'ok')
  
  # Identify each "streak"" to be able to identify groups of days to average over
  df.clean$streak<- NA;
  streak<- 1;
  df.clean$streak[1]<- streak
  for(i in 2:nrow(df.clean)){
    
    if(df.clean$flag[i] == df.clean$flag[i-1]){
      df.clean$streak[i] = streak
      
    }else{
      
      streak = streak + 1
      df.clean$streak[i] = streak
    }
  }
  
  # Compute average of num_new_obs per "streak". Only those over flagged days are releavant
  df.clean <-
    df.clean %>%
    group_by(streak) %>%
    mutate(mean_streak = round(mean(num_new_obs), 0),
           midpoint = min(date) + (max(date) - min(date)) / 2) %>%
    ungroup()
  
  # Replacement timepoints
  replacement <-
    df.clean %>%
    filter(flag == 'flag') %>%
    select(-num_new_obs,-date) %>%
    group_by(midpoint) %>%
    distinct(mean_streak,.keep_all=TRUE) %>%
    ungroup() %>%
    rename(date = midpoint, num_new_obs = mean_streak)
  
  # Remove the flagged days, replace with new datapoints
  df.out <-
    df.clean %>%
    filter(flag != 'flag')  %>%
    select(-mean_streak,-midpoint) %>%
    rbind(replacement) %>%
    arrange(date) %>%
    select(-flag,-streak)
  
  return(df.out)
  
} # input: a dataframe which includes the column names date and num_new_obs (other columns will be ignored)

vector_of_lists_to_matrix <- function(vector_of_lists){
  sim_matrix <- matrix(nrow=length(vector_of_lists),ncol = length(vector_of_lists[[1]]))
  for(i in 1:length(vector_of_lists)){
    sim_matrix[i,] = vector_of_lists[[i]]
  }
  sim_matrix
}
matrix_to_vector_of_lists <- function(matrix){
  vector_of_lists <- vector(mode="list",length = nrow(matrix))
  for(i in 1: nrow(matrix)){
    vector_of_lists[[i]] <- matrix[i,]
  }
  vector_of_lists
}

sim_cum_calc_per_pop <- function(sim_obs_vector_of_lists,population){
  sim_obs_matrix <- vector_of_lists_to_matrix(sim_obs_vector_of_lists)
  sim_cum_obs_matrix <- apply(sim_obs_matrix,2,cumsum)
  sim_cum_obs_matrix_per_10k = sim_cum_obs_matrix / population[1]
  matrix_to_vector_of_lists(sim_cum_obs_matrix_per_10k)
}

compute.td.m1.v2<- function(dat, user.t1, user.t2){
  
  # Format data
  dat<- filter(dat, cum_num_obs > 0)
  
  # Compute Td
  user.t1<- as.Date(user.t1); user.t2<- as.Date(user.t2)
  nb.days<- as.numeric(user.t2 - user.t1)
  user.n.t1<- dat %>% filter(date == user.t1) %>% select(cum_num_obs)
  user.n.t2<- dat %>% filter(date == user.t2) %>% select(cum_num_obs)
  Td<- round(nb.days/(log2(user.n.t2[[1]]/user.n.t1[[1]])), 1) #same as nb.days * (log(2)/(log(user.n.t2[[1]]/user.n.t1[[1]])))
  
  Td = ifelse(length(Td) > 0, Td, NA) # If cases/deaths have not started yet or started after t1 specified, the Td cannot be computed. Td will be numeric(0), if that is the case make the function spits out an NA.
  
  return(Td)
  
} # input: cbind(date, numNewCases)

compute.td<- function(seven_day_separation){
  Td<- round(7/(log2(seven_day_separation[2]/seven_day_separation[1])), 1)
  ifelse(seven_day_separation[2]==0|seven_day_separation[1]==0, NA, Td)
}


Td.lapply<- function(cum_num_obs,date, t1, t2){
  cum_num_obs_matrix <- vector_of_lists_to_matrix(cum_num_obs[date == t2 | date == t1])
  obs_doubling_time <- apply(cum_num_obs_matrix,2,compute.td)
  list(obs_doubling_time)
}



    epidemic.diff<- function(data, focal.country, vs.country){
  
  a1 = NA
  
  data2 = data[,c('date', focal.country, vs.country)]
  data3 = data2[intersect(which(!is.na(data2[,2])), which(!is.na(data2[,3]))), ]
  
  data = data3
  ahead.country = names(which.max(tail(data,1)[2:3]))
  behind.country = names(which.min(tail(data,1)[2:3]))
  
  
  if(focal.country == ahead.country){
    
    a <- data[nrow(data),behind.country]
    for (i in 1:nrow(data)) {
      if (data[i,ahead.country] < a &
          data[i+1,ahead.country] > a){
        a1 <-nrow(data)-i - (a-data[i,ahead.country])/(data[i+1,ahead.country]-data[i,ahead.country])
      }
    }
    
    diff = round(a1[[1]], 1)
    
  }else if(focal.country != ahead.country){
    
    a <- data[nrow(data),behind.country]
    for (i in 1:nrow(data)) {
      if (data[i,ahead.country] < a &
          data[i+1,ahead.country] > a){
        a1 <-nrow(data)-i - (a-data[i,ahead.country])/(data[i+1,ahead.country]-data[i,ahead.country])
      }
    }
    
    diff = round(-a1[[1]], 1)
    
  }
  
  return(diff)
  
}







