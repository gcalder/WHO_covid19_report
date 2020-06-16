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

eight_day_pos_window <- function(positive){
  postive_window <- vector(length = length(positive))
  i <- 1
  j <- 1
  
  while(j < length(positive) + 1){
    postive_window[j] <- (sum(positive[i:j]) == length(positive[i:j]))
    j = j + 1
    if(j > 8) i = i + 1
  }
  
  return(postive_window)
}

rpois_error <- function(iterations, observation, positive){
  if(positive) return(rpois(iterations,observation))
  else return(NA)
}

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

sim_cum_calc_per_pop <- function(sim_obs_vector_of_lists,population,positive){
  if(positive){
    sim_obs_matrix <- vector_of_lists_to_matrix(sim_obs_vector_of_lists)
    sim_cum_obs_matrix <- apply(sim_obs_matrix,2,cumsum)
    sim_cum_obs_matrix_per_10k = sim_cum_obs_matrix / population[1]
    return(matrix_to_vector_of_lists(sim_cum_obs_matrix_per_10k))
  }
  else return(NA)
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


Td.lapply<- function(cum_num_obs,date, t1, t2, positive,iteration){
  has_changed_observations <- tibble(date=date,positive=positive) %>%
    filter(date >=t1, date <=t2) %>%
    pull(positive)
  
  if(sum(has_changed_observations) == length(has_changed_observations)){
    cum_num_obs_matrix <- vector_of_lists_to_matrix(cum_num_obs[date == t2 | date == t1])
    obs_doubling_time <- apply(cum_num_obs_matrix,2,compute.td)
    return(list(obs_doubling_time))
  }
  else return(list(rep(NA,iteration)))
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

# function to extract all the relevant summary information for the text below the country specific graphs
country_summary_text_function <- function(i, WHO_latest_day_cases_and_deaths_simulated, WHO_cases_and_deaths_doubling_time) {
  currrent_country_latest_cases <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_case_obs)
  
  currrent_country_latest_cases_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_case_obs)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_deaths <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_deaths_obs)
  
  currrent_country_latest_deaths_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_deaths_obs)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_cases_per_pop <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_case_obs_per_10k)
  
  currrent_country_latest_cases_per_pop_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_case_obs_per_10k)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  currrent_country_latest_deaths_per_pop <- WHO_latest_day_cases_and_deaths_simulated %>%
    filter(country == i) %>%
    pull(last_day_deaths_obs_per_10k)
  
  currrent_country_latest_deaths_per_pop_rank <- WHO_latest_day_cases_and_deaths_simulated %>%
    arrange(desc(last_day_deaths_obs_per_10k)) %>%
    mutate(rank = 1:length(WHO_latest_day_cases_and_deaths_simulated$country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  current_country_cases_dt <- WHO_cases_and_deaths_doubling_time  %>%
    filter(country == i) %>%
    pull(cases_doubling_time)
  
  current_country_cases_dt_rank <- WHO_cases_and_deaths_doubling_time %>%
    filter(cases_doubling_time > -1) %>%
    ungroup() %>%
    arrange(cases_doubling_time) %>%
    mutate(rank = 1:length(country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  current_country_deaths_dt <- WHO_cases_and_deaths_doubling_time %>%
    filter(country == i) %>%
    pull(deaths_doubling_time)
  
  current_country_deaths_dt_rank <- WHO_cases_and_deaths_doubling_time %>%
    filter(deaths_doubling_time > -1) %>%
    ungroup() %>%
    arrange(deaths_doubling_time) %>%
    mutate(rank = 1:length(country)) %>%
    filter(country == i) %>%
    pull(rank)
  
  if(current_country_cases_dt %in% c(Inf,-1,NA) | current_country_deaths_dt %in% c(Inf,-1,NA)){
    
    if(current_country_cases_dt %in% c(Inf,-1,NA) & current_country_deaths_dt %in% c(Inf,-1,NA)){
      if(current_country_cases_dt %in% c(Inf,NA)) sentence_3 = " The doubling time of reported cases cannot be calculated as no new cases have been reported in last 7 days."
      else sentence_3 = " The doubling time of reported cases cannot be calculated as some cases have been redacted in last 7 days."
      if(current_country_deaths_dt %in% c(Inf,NA)) sentence_4 = " The doubling time of reported deaths cannot be calculated as no new deaths have been reported in last 7 days."
      else sentence_4 = " The doubling time of reported deaths cannot be calculated as some deaths have been redacted in last 7 days."
    }
    else if(current_country_cases_dt %in% c(Inf,-1,NA)){
      if(current_country_cases_dt %in% c(Inf,NA)) sentence_3 = " The doubling time of reported cases cannot be calculated as no new cases have been reported in last 7 days."
      else sentence_3 = " The doubling time of reported cases cannot be calculated as some cases have been redacted in last 7 days."
      sentence_4 = paste0(" The doubling time of reported deaths over the last 7 days is ", current_country_deaths_dt, " days (", toOrdinal(current_country_deaths_dt_rank),").")
    }
    else{
      if(current_country_deaths_dt %in% c(Inf,NA)) sentence_4 = " The doubling time of reported deaths cannot be calculated as no new deaths have been reported in last 7 days."
      else sentence_4 = " The doubling time of reported deaths cannot be calculated as some deaths have been redacted in last 7 days."
      sentence_3 = paste0(" The doubling time of reported cases over the last 7 days  is ", current_country_cases_dt, " days (", toOrdinal(current_country_cases_dt_rank), ").")
    }
    
  }
  else {
    sentence_3 = paste0(" The doubling times of reported cases and deaths over the last 7 days are ", current_country_cases_dt, " days (", toOrdinal(current_country_cases_dt_rank), ") and ", current_country_deaths_dt, " days (", toOrdinal(current_country_deaths_dt_rank),").")
    sentence_4 = NULL
  }
  tibble(
    sentence_1 = paste0(i, " has ", currrent_country_latest_cases, " reported case(s) (", toOrdinal(currrent_country_latest_cases_rank), " in the region) and ", currrent_country_latest_deaths, " reported death(s) (", toOrdinal(currrent_country_latest_deaths_rank), ")."),
    sentence_2 = paste0(" Cumulative counts per 10,000 population of reported cases and deaths are ", signif(currrent_country_latest_cases_per_pop,2), " (", toOrdinal(currrent_country_latest_cases_per_pop_rank), ") and ", signif(currrent_country_latest_deaths_per_pop,2), " (", toOrdinal(currrent_country_latest_deaths_per_pop_rank), ") respectively."),
    sentence_3 = sentence_3,
    sentence_4 = sentence_4
    )
}






