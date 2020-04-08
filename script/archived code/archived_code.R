# ARCHIVED CODE


# Producing Aligned curves dataset used in figure 6




aligned_curves<- data.frame(day_since_start = c(0, cumsum(as.numeric(diff(d[rowSums(d[,-1]) > 0,]$date)))))
for(c in 2:ncol(d)){
  d.temp = d[,c(1, c)]
  
  if(max(d.temp[,2]) == 0){
    
    d.temp.trim.f = data.frame(day_since_start = 0, cumCases = NA) 
    colnames(d.temp.trim.f)[2]<- colnames(d.temp)[2]
    aligned_curves<- 
      left_join(aligned_curves, d.temp.trim.f, by = 'day_since_start')
  }else{
    d.temp.trim = d.temp[which(d.temp[,2] > 0), ]
    d.temp.trim$day_since_start = c(0, cumsum(as.numeric(diff(d.temp.trim$date))))
    d.temp.trim.f<- d.temp.trim[,c(3,2)]
    aligned_curves<- 
      left_join(aligned_curves, d.temp.trim.f, by = 'day_since_start')
  }
  
}



# working on cumNum cases, raw --> plot = log of cumCases, not per 10k pop

View(d[rowSums(d[,-1]) > 0,])


aligned_curves$date<- as.Date(d[rowSums(d[,-1]) > 0, 1])

aligned_curves.focal<- aligned_curves[aligned_curves$date > (today - 7),]


# RESTART FAN

d.last.7<- d[d$date > (today - 7),]
aligned_curves<- data.frame(day_since_start = c(0, cumsum(as.numeric(diff(d.last.7$date)))))
for(c in 2:ncol(d.last.7)){
  d.temp = d.last.7[,c(1, c)]
  
  if(max(d.temp[,2]) == 0){
    
    d.temp.trim.f = data.frame(day_since_start = 0, cumCases = NA) 
    colnames(d.temp.trim.f)[2]<- colnames(d.temp)[2]
    aligned_curves<- 
      left_join(aligned_curves, d.temp.trim.f, by = 'day_since_start')
  }else{
    d.temp.trim = d.temp[which(d.temp[,2] > 0), ]
    d.temp.trim$day_since_start = c(0, cumsum(as.numeric(diff(d.temp.trim$date))))
    d.temp.trim.f<- d.temp.trim[,c(3,2)]
    aligned_curves<- 
      left_join(aligned_curves, d.temp.trim.f, by = 'day_since_start')
  }
  
}

aligned.f<- cbind(dat = d.last.7$date,
      aligned_curves)




diff(aligned.f$Algeria)



plot(log10(Algeria) ~ dat, data = aligned.f, type = 'l')

xseq.fbc.5<- aligned.f$dat

ymax.fbc.5<- round_any(max(aligned.f[,-c(1,2)], na.rm = TRUE), 10)
ymax.fbc.5<- round_any(max(aligned.f[,-c(1,2)], na.rm = TRUE), 10)


yseq.fbc.5 = seq(0, ymax.fbc.5, 10) 




plot('', xlim = range(xseq.fbc.5), ylim = range(), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
axis(2, at = yseq.raw.cases, font = y.font.fbc, cex.axis = y.cex.fbc)

abline(v = xseq.fbc, h = yseq.raw.cases, col = 'lightgrey', lty = 'dotted')
for(c2 in 2:ncol(d)){
  lines(d[,c2] ~ d$date, lwd = 1.5, col = 'grey')
} # Draw lines + overplot focal country line
lines(d[,c] ~ d$date, lwd = 3)


# NEW v2.0


      















# Last day DEATHS & CI, raw ----
dat.cumsumraw.deaths<- vector('list', length = length(regions))

for(i in 1:length(sims.store.deaths)){
  
  latest<- sims.store.deaths[[i]] %>%
    #mutate(region = names(sims.store.deaths[i])) %>%
    rename(cumNumDeaths_raw = observed) %>%
    tail(., 1)
  
  latest[,2:(ncol(latest)-1)]<- latest[,2:(ncol(latest)-1)] * ((pops[pops$country == latest$country,'popsize'])/10000)
  
  
  cumNumDeaths_raw.sim.last<- as.numeric(latest[,which(substr(colnames(latest), 1, 1) == 'V')])
  
  ci.low.cumNumDeaths_raw<- quantile(cumNumDeaths_raw.sim.last, c(0.05), method = 6, na.rm = TRUE)[[1]]
  ci.upp.cumNumDeaths_raw<- quantile(cumNumDeaths_raw.sim.last, c(0.95), method = 6, na.rm = TRUE)[[1]]
  
  dat.cumsumraw.deaths[[i]]<-
    latest %>%
    select(date, country, cumNumDeaths_raw) %>%
    mutate(ci.low = ci.low.cumNumDeaths_raw,
           ci.upp = ci.upp.cumNumDeaths_raw)
}

dat.cumsumraw.deaths.df<- do.call('rbind', dat.cumsumraw.deaths) %>%
  select(country, cumNumDeaths_raw, ci.low, ci.upp) %>%
  arrange(-cumNumDeaths_raw) %>%
  rename(country = country,
         `Cum. Deaths` = cumNumDeaths_raw)

dat.cumsumraw.deaths.df$`Cum. Deaths`<- formatC(dat.cumsumraw.deaths.df$`Cum. Deaths`, digits = 0, format = "f")
dat.cumsumraw.deaths.df$ci.low<- formatC(dat.cumsumraw.deaths.df$ci.low, digits = 0, format = "f")
dat.cumsumraw.deaths.df$ci.upp<- formatC(dat.cumsumraw.deaths.df$ci.upp, digits = 0, format = "f")

dat.cumsumraw.deaths.df<- dat.cumsumraw.deaths.df %>%
  left_join(who.tab, by = 'country') %>%
  select(full_name, `Cum. Deaths`, ci.low, ci.upp) %>%
  rename(Country = full_name)


dat.cumsumraw.deaths.df$`Cum. Deaths`[dat.cumsumraw.deaths.df$`Cum. Deaths` == 'NA']<- 0


# Last day DEATHS & CI, 10k ----


dat.cumsumraw.deaths10k<- vector('list', length = length(regions))

for(i in 1:length(sims.store.deaths)){
  
  latest<- sims.store.deaths[[i]] %>%
    #mutate(region = names(sims.store.deaths[i])) %>%
    rename(cumNumDeaths_raw = observed) %>%
    tail(., 1)
  
  #latest[,2:(ncol(latest)-1)]<- latest[,2:(ncol(latest)-1)] * ((pops[pops$country == latest$country,'popsize'])/10000)
  
  
  cumNumDeaths_raw.sim.last<- as.numeric(latest[,which(substr(colnames(latest), 1, 1) == 'V')])
  
  ci.low.cumNumDeaths_raw<- quantile(cumNumDeaths_raw.sim.last, c(0.05), method = 6, na.rm = TRUE)[[1]]
  ci.upp.cumNumDeaths_raw<- quantile(cumNumDeaths_raw.sim.last, c(0.95), method = 6, na.rm = TRUE)[[1]]
  
  dat.cumsumraw.deaths10k[[i]]<-
    latest %>%
    select(date, country, cumNumDeaths_raw) %>%
    mutate(ci.low = ci.low.cumNumDeaths_raw,
           ci.upp = ci.upp.cumNumDeaths_raw)
}

dat.cumsumraw.deaths10k.df<- do.call('rbind', dat.cumsumraw.deaths10k) %>%
  select(country, cumNumDeaths_raw, ci.low, ci.upp) %>%
  arrange(-cumNumDeaths_raw) %>%
  rename(country = country,
         `Cum. Deaths per 10k pop.` = cumNumDeaths_raw)

dat.cumsumraw.deaths10k.df$`Cum. Deaths per 10k pop.`<- formatC(dat.cumsumraw.deaths10k.df$`Cum. Deaths per 10k pop.`, digits = 4, format = "f")
dat.cumsumraw.deaths10k.df$ci.low<- formatC(dat.cumsumraw.deaths10k.df$ci.low, digits = 4, format = "f")
dat.cumsumraw.deaths10k.df$ci.upp<- formatC(dat.cumsumraw.deaths10k.df$ci.upp, digits = 4, format = "f")

dat.cumsumraw.deaths10k.df<- dat.cumsumraw.deaths10k.df %>%
  left_join(who.tab, by = 'country') %>%
  select(full_name, `Cum. Deaths per 10k pop.`, ci.low, ci.upp) %>%
  rename(Country = full_name)



dat.cumsumraw.deaths10k.df$`Cum. Deaths per 10k pop.`[dat.cumsumraw.deaths10k.df$`Cum. Deaths per 10k pop.` == '   NA']<- 0

















# HEATNMAP FULL CODE ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   WHO report - analysis script     #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# This R script runs all the analyses for the WHO report
# It outputs:
#   A csv file with all the Td computed
#   PNGs file of the maps (individual and combined)
#   The R session as an RData object
# The automated report (Rmarkdown doc) then load this RData object to run plots commands in it



# 1) SET UP ----

setwd('/Users/s1687811/Documents/GitHub/WHO_covid19_report/') 
today<- Sys.Date() - 2  # Set date as to that of the data to fetch.
its = 10 # Number of iterations for the poisson error simulation (bootstrap), Set to 1000. Or 10 for a quick test.
set.seed(as.numeric(today)) # setting seed allows repeatability of poisson error simulations. Use the date as a reference point for the seed.

source('/Users/s1687811/Documents/GitHub/covid19/script/sourced_functions_doublingTime_reports.R') # Source several functions used in the below script. See README file for details. 

time_window<- 7 # Time window over which doubling time is calculated
t2.define<- today
t1.define<- t2.define - time_window

library(geojsonio)
library(utils)
library(httr)
library(tidyverse)
library(magrittr)
library(sf)
library(RColorBrewer)
library(classInt)
library(geojson)
library(cartography)
library(magick)

# LOADING DATA ----

# WHO list of countries & their popsize.
# Various encoding of the countries name used for different purpose (match various data sources, for maps, shortenned for figures etc.)
who.info.tab<-
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'population counts per country')[,-1] %>%
  rename(ISO3 = countryterritoryCode,
         popsize = pop) %>%
  left_join(read.csv('input_files/WHO_country_list.csv', stringsAsFactors = FALSE), by = 'ISO3') %>%
  as.data.frame()


# All columns in d are WHO countries.
# After modifying "Côte d'Ivoire" into "Cote d'Ivoire" (without the ^), then the variable "country" of the who.info.tab is the one that matches the countries name of the data
d<- # Get cumulative case counts data
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() #%>%
#select(c(date, which(colnames(.) %in% who.info.tab$country)))

d.long<- d %>% gather('country', 'cumNumCases', 2:ncol(d)) # long version, more practical for certain analyses/figures

d.deaths<-  # Get cumulative deaths  data
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative deaths') %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame()

d.deaths.long<-  d.deaths %>% gather('country', 'cumNumDeaths', 2:ncol(d.deaths))

d.10k.log.long<- # Cumulative cases per 10k, log10()-ed
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases per 10k popula') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  gather('country', 'cumcases_10k', 2:ncol(.)) %>%
  na_if(0) %>%
  mutate(cumcases_10k_log = log10(cumcases_10k)) %>%
  select(-cumcases_10k)

d.10k.log<- # Wide version of the above
  d.10k.log.long %>%
  spread(country, cumcases_10k_log)

d.deaths.10k.log.long<- # Cumulative deaths per 10k population
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative deaths per 10k popul') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  gather('country', 'cumdeaths_10k', 2:ncol(.)) %>%
  na_if(0) %>%
  mutate(cumdeaths_10k_log = log10(cumdeaths_10k)) %>%
  select(-cumdeaths_10k)

d.deaths.10k.log<- # Wide version of the above
  d.deaths.10k.log.long %>%
  spread(country, cumdeaths_10k_log)


who_data<- read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'data for map') 




# Map with countries in WHO-Africa for front page.
# africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
# africa@data %<>% left_join(who_data, by=c("ISO_A3"="countryterritoryCode"))
# africa@data$WHOCountry <- ifelse(is.na(africa@data$location),0,1)
# png(file = "input_files/WHO_Africa.png", width=1082, height=960, pointsize=22)
# typoLayer(spdf = africa, var = "WHOCountry", col = c("darksalmon", "white"), legend.pos = "n")
# dev.off()


# DOUBLING TIMES: Incidence----
# This sections computes
# For incidence
# the Doubling Time (Td) between t1 and t2 for each country
# And simulate the epicurves (poisson error bootstraping)
# To re-estimate Td on each simulation
# To obtain a CI of the Td

# Re-define t1 and t2 to the closests dates as possible as those defined by the user (this is just in case there are missing days in dataset. If not missing days, this is no effect)
t2<-  d$date[which.min(abs(d$date-t2.define))]
t1<- d$date[which.min(abs(d$date-(t1.define)))]

# Dataframe which will be filled throughout this script and write as csv at the end. Collects all Td & 95%CI.
Td.report<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())

regions<- colnames(d)[-1] # Using variable "region" instead of "country" because re-using code from Scotland health board report

# To store output of each simulations, in order to compute the cumulative confidence interval for the histogram plot. Declare a list with as many items as there is are countries, and will keep the dataframes with appended simulations in each element of the list.
sims.store<- vector('list', length = length(regions))


for(r in 1:length(regions)){
  
  d.clean<- # Get data for focal country, rename cumulative cases columns "cumNumCases", to pass it through sim.epi() function
    d[,c(1, which(colnames(d) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%
    as.data.frame()
  
  
  # Simulate poisson error. Call function sim.epi() from the sourced R script
  # Output: original dataframe extended with numNewCases (= number of new cases instead of cumulative) + one column per simulation, with variable name being "V.". Simulations output are number of new cases.
  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE)
  
  
  # Get the normalised per 10k pop. version of the epidemic to compute Td over 10k pops. 
  # and format the df correctly to feed in the compute.td() function (from sourced script) 
  d.clean.sim.cum<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>% # Simulated data are number of new cases. Keep the 'numNewCases' version of the original (observed) data to treat everything the same way
    mutate_all(~ cumsum(.) * (10000/(who.info.tab[who.info.tab$country == regions[r], 'popsize']))) %>% # 10k pop normalisation
    rename(cumNumCases = numNewCases) %>% # 'numNewCases' (observed) now is back to 'cumNumCases'
    cbind(date = d.clean.sim$date) %>% # append back the dates
    select(c(ncol(.), seq(1, ncol(.)-1))) # re-order columns to fit in the compute.td() function
  
  
  d.clean.sim.cum.list<- # Transform into a list to sapply compute.td() on each dataset
    d.clean.sim.cum %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.cum.list,
               Td.lapply, # Function from sourced script. Simply applies compute.td() on a list of epidemic data
               dates = d.clean.sim.cum$date,
               t1 = t1, t2 = t2)
  
  # Observed and bootstrap distribution of Tds
  Td.obs<- as.numeric(Tds['cumNumCases']) # First element of the list of Tds is the observed data
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')]) # And those are the simulated = bootstrap distribution
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] # NAs come from cases where there were still zero cases observed at t1 in a simulated dataset. Normal to happen when still very low number of cases observed. Becomes less of a problem as number of cases rise.
  
  # Get CI from distribution of Td
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]] # choice of method based on "https://amstat.tandfonline.com/doi/full/10.1080/00031305.2015.1089789#.Xoc0yZNKjBI", see section 4.6
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  # Update the doubling time report dataframe with the death doubling time & 95%CI
  Td.report<- rbind(Td.report,
                    data.frame(variable = paste(regions[r]),
                               Td.obs = Td.obs[[1]],
                               ci.low = ci.low,
                               ci.upp = ci.upp,
                               t1 = t1,
                               t2 = t2)
  )
  
  # Store simulation, for later purpose in script
  d.clean.sim.cum$country = regions[r] # Append name of country to make sure we keep track
  sims.store[[r]]<- d.clean.sim.cum
  colnames(sims.store[[r]])[2]<- 'observed'
  
} 


Td.report<-
  Td.report %>%
  rename(country = variable)


Td.report.clean<-  # Formatted version to output in the report
  Td.report %>%
  left_join(who.info.tab, by = 'country') %>%
  select(full_name, Td.obs, ci.low, ci.upp) %>%
  rename(Country = full_name,
         `Incidence doubling time (days)` = Td.obs)


# At present (early April) there are still very few cases in most countries.
# This causes issue either to compute Td (divisions by zero) or with the poisson error simulation (poisson process with 0 mean most of the time = generate only zero data)
# Identify all countries for which there were an issue arising from this, causing numerical issue either in Td computation or in the 95%CI computation, and set both Td and CIs to NA, as these are not reliable.
# Do this for the reported table only. For the figure, these will get trimed out anyway ...? # TOREVIEW
has.pb<- which(
  Td.report.clean$ci.upp %in% c(NA, 'NA', Inf, 'Inf') |
    Td.report.clean$ci.low %in% c(NA, 'NA', Inf, 'Inf') |
    Td.report.clean$`Incidence doubling time (days)` %in% c(NA, 'NA', Inf, 'Inf')
) # identify countrys with issue


Td.report.clean[has.pb,2:4]<- 'NA' # Set entire row as NA

Td.report.clean2<- # For better information convey, arrange by doubling Time
  Td.report.clean %>%
  arrange(as.numeric(`Incidence doubling time (days)`))




# DOUBLING TIMES: Deaths----
# This sections works exactly as the previous one but for deaths.
# The output is stored in the Td.report.deaths tables and sims.store.deaths list

t2<-  d.deaths$date[which.min(abs(d.deaths$date-t2.define))]
t1<- d.deaths$date[which.min(abs(d.deaths$date-(t1.define)))]
Td.report.deaths<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())
regions<- colnames(d.deaths)[-1]
sims.store.deaths<- vector('list', length = length(regions))


for(r in 1:length(regions)){
  
  d.clean<-
    d.deaths[,c(1, which(colnames(d.deaths) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>% # naming 'cumNumCases' because that's how sim.epi() takes variable as argument. But it is indeed DEATHS (cases of deaths ...). A bit confusing, sim.epi() function may be improved to make it general and avoid forcing the variable be named 'cumNumCases' ...
    as.data.frame()
  
  
  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE) # Simulate poisson error on raw DEATHS
  
  d.clean.sim.cum<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (1000/(who.info.tab[who.info.tab$country == regions[r], 'popsize']))) %>%
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  
  d.clean.sim.cum.list<-
    d.clean.sim.cum %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.cum.list,
               Td.lapply,
               dates = d.clean.sim.cum$date,
               t1 = t1, t2 = t2)
  
  # Observed and bootstrap distribution of Tds
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] 
  
  # Get CI from distribution of Td
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  Td.report.deaths<- rbind(Td.report.deaths,
                           data.frame(variable = paste(regions[r]),
                                      Td.obs = Td.obs[[1]],
                                      ci.low = ci.low,
                                      ci.upp = ci.upp,
                                      t1 = t1,
                                      t2 = t2)
  )
  
  
  d.clean.sim.cum$country = regions[r]
  sims.store.deaths[[r]]<- d.clean.sim.cum
  colnames(sims.store.deaths[[r]])[2]<- 'observed'
  
} 


Td.report.deaths <-
  Td.report.deaths %>%
  rename(country = variable)

Td.report.deaths.clean<- 
  Td.report.deaths %>%
  left_join(who.info.tab, by = 'country') %>%
  select(full_name, Td.obs, ci.low, ci.upp) %>%
  rename(Country = full_name,
         `Deaths doubling time (days)` = Td.obs)

has.pb<- which(
  Td.report.deaths.clean$ci.upp %in% c(NA, 'NA', Inf, 'Inf') |
    Td.report.deaths.clean$ci.low %in% c(NA, 'NA', Inf, 'Inf') |
    Td.report.deaths.clean$`Deaths doubling time (days)` %in% c(NA, 'NA', Inf, 'Inf')
)


Td.report.deaths.clean[has.pb,2:4]<- 'NA'

Td.report.deaths.clean2<- 
  Td.report.deaths.clean %>%
  arrange(as.numeric(`Deaths doubling time (days)`))





# The TWO NEXT SECTIONS are to collect the Last day incidence of the observed data but ALSO OF THE SIMULATED DATA
# that, to be able to report a 95%CI on the observed data
# Done on the raw number of cases and the per 10k pop 

# Last day Incidence & CI, raw ----

dat.cumsumraw<- vector('list', length = length(regions))
for(i in 1:length(sims.store)){
  
  latest<- sims.store[[i]] %>% # for data of country i, stored in item y of the simulation list
    rename(cumNumCase_raw = observed) %>%
    tail(., 1) # Get the last row = last date
  
  # Transform back to be RAW number of cases (NOT per 10k)
  latest[,2:(ncol(latest)-1)]<- latest[,2:(ncol(latest)-1)] * ((who.info.tab[who.info.tab$country == unique(sims.store[[i]]$country), 'popsize'])/10000)
  
  # Extract only the simulations, compute 95%CI from it
  cumNumCases_raw.sim.last<- as.numeric(latest[,which(substr(colnames(latest), 1, 1) == 'V')]) 
  ci.low.cumNumCases_raw<- quantile(cumNumCases_raw.sim.last, c(0.05), method = 6, na.rm = TRUE)[[1]]
  ci.upp.cumNumCases_raw<- quantile(cumNumCases_raw.sim.last, c(0.95), method = 6, na.rm = TRUE)[[1]]
  
  dat.cumsumraw[[i]]<- # Store it
    latest %>%
    select(date, country, cumNumCase_raw) %>%
    mutate(ci.low = ci.low.cumNumCases_raw,
           ci.upp = ci.upp.cumNumCases_raw)
}
# Do some formatting to output a nice and clean table in the report
dat.cumsumraw.df<- do.call('rbind', dat.cumsumraw) %>%
  select(country, cumNumCase_raw, ci.low, ci.upp) %>%
  arrange(-cumNumCase_raw) %>%
  rename(country = country,
         `Cum. Incidence` = cumNumCase_raw)
dat.cumsumraw.df$`Cum. Incidence`<- formatC(dat.cumsumraw.df$`Cum. Incidence`, digits = 0, format = "f") # Format numbers to be with 1 digit, even those with 0 digits (e.g. 4 --> 4.0)
dat.cumsumraw.df$ci.low<- formatC(dat.cumsumraw.df$ci.low, digits = 0, format = "f")
dat.cumsumraw.df$ci.upp<- formatC(dat.cumsumraw.df$ci.upp, digits = 0, format = "f")
dat.cumsumraw.df<- dat.cumsumraw.df %>%
  left_join(who.info.tab, by = 'country') %>%
  select(full_name, `Cum. Incidence`, ci.low, ci.upp) %>%
  rename(Country = full_name)
dat.cumsumraw.df$`Cum. Incidence`[dat.cumsumraw.df$`Cum. Incidence` == 'NA']<- 0


# Last day Incidence & CI 10k ----
# Doing the exact same thing but without transforming back into raw number of cases (keep normalised per 10k.)
# Unsure which one will be required to be plotted in the end... Now both are available.
dat.cumsum10k<- vector('list', length = length(regions))
for(i in 1:length(sims.store)){
  
  latest<- sims.store[[i]] %>%
    rename(cumNumCase_10k = observed) %>%
    tail(., 1)
  
  cumNumCases_10k.sim.last<- as.numeric(latest[,which(substr(colnames(latest), 1, 1) == 'V')])
  ci.low.cumNumCases_10k<- quantile(cumNumCases_10k.sim.last, c(0.05), method = 6, na.rm = TRUE)[[1]]
  ci.upp.cumNumCases_10k<- quantile(cumNumCases_10k.sim.last, c(0.95), method = 6, na.rm = TRUE)[[1]]
  
  dat.cumsum10k[[i]]<-
    latest %>%
    select(date, country, cumNumCase_10k) %>%
    mutate(ci.low = ci.low.cumNumCases_10k,
           ci.upp = ci.upp.cumNumCases_10k)
}
dat.cumsum10k.df<- do.call('rbind', dat.cumsum10k) %>%
  select(country, cumNumCase_10k, ci.low, ci.upp) %>%
  arrange(-cumNumCase_10k) %>%
  rename(country = country,
         `Cum. Incidence per 10k pop.` = cumNumCase_10k)
dat.cumsum10k.df$`Cum. Incidence per 10k pop.`<- formatC(dat.cumsum10k.df$`Cum. Incidence per 10k pop.`, digits = 4, format = "f")
dat.cumsum10k.df$ci.low<- formatC(dat.cumsum10k.df$ci.low, digits = 4, format = "f")
dat.cumsum10k.df$ci.upp<- formatC(dat.cumsum10k.df$ci.upp, digits = 4, format = "f")
dat.cumsum10k.df<- dat.cumsum10k.df %>%
  left_join(who.info.tab, by = 'country') %>%
  select(full_name, `Cum. Incidence per 10k pop.`, ci.low, ci.upp) %>%
  rename(Country = full_name)
dat.cumsum10k.df$`Cum. Incidence per 10k pop.`[dat.cumsum10k.df$`Cum. Incidence per 10k pop.` == '   NA']<- 0



# FUNCTION PLOTTING BY COUNTRY FIGURES ----
# I could not find a way to loop over the figure-producing for each country while creating new header each time in the Rmarkdown.
# I had to have one r code chunck per country, under 47 headers, in order to have the countries listed in the Table of Content.
# The hack I got away with  was wrapping this code into a "function", which only takes as argument the index "c" corresponding to the columns in the dataframe for which to plot the data (so we start at c = 2, because the first column is the date in the data).
# It is quite hacky/poor-quick-fix, since many variables in it are not even passed as argument of the function, but are environement variables defined in the Rmarkdown. Hence, if you change those variables in the Rmarkdown doc or do not define them, this piece of code will not work.
plot.country<- function(c){
  
  # SET UP THE PLOT AREA
  par(mfrow = c(2,2),
      oma = c(2,2,4,1) + 0.1,
      mar = c(2,2,1,1) + 0.1)
  
  
  # RAW CASES
  # Set plot, add axes, add background gridlines
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.raw.cases), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.raw.cases, font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.raw.cases, col = 'lightgrey', lty = 'dotted')
  for(c2 in 2:ncol(d)){
    lines(d[,c2] ~ d$date, lwd = 1.5, col = 'grey')
  } # Draw lines + overplot focal country line
  lines(d[,c] ~ d$date, lwd = 3)
  
  
  # LOG CASES
  # Set plot, add axes, add background gridlines
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.log.cases), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.log.cases, labels = as.character(10^yseq.log.cases), font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.log.cases, col = 'lightgrey', lty = 'dotted')
  for(c2 in 2:ncol(d.10k.log)){
    lines(d.10k.log[,c2] ~ d.10k.log$date, lwd = 1.5, col = 'grey')
  } # Draw lines + overplot focal country line
  lines(d.10k.log[,c] ~ d.10k.log$date, lwd = 3)
  
  
  # DEATHS RAW
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.raw.deaths), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.raw.deaths, font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.raw.deaths, col = 'lightgrey', lty = 'dotted')  
  for(c2 in 2:ncol(d.deaths)){
    lines(d.deaths[,c2] ~ d.deaths$date, lwd = 1.5, col = 'grey')
  } # Draw lines + overplot focal country line
  lines(d.deaths[,c] ~ d.deaths$date, lwd = 3)
  
  # DEATHS LOG
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.log.deaths), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.log.deaths, labels = as.character(10^yseq.log.deaths), font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.log.deaths, col = 'lightgrey', lty = 'dotted')  
  for(c2 in 2:ncol(d.deaths.10k.log)){
    lines(d.deaths.10k.log[,c2] ~ d.deaths.10k.log$date, lwd = 1.5, col = 'grey')
  }
  lines(d.deaths.10k.log[,c] ~ d.deaths.10k.log$date, lwd = 3)
  
  
  # ADD LABELS
  # Y-labels
  mtext('Cases', side = 2, line = ylab.line.fbc, outer = TRUE, col = 'black', cex = ylab.cex.fbc, font = ylab.f.fbc, at = 0.75)
  mtext('Deaths', side = 2, line = ylab.line.fbc, outer = TRUE, col = 'black', cex = ylab.cex.fbc, font = ylab.f.fbc, at = 0.25)
  
  # X-labels
  mtext('Cumulative cases per 10k pop.', side = 3, line = xTOPlab.line.fbc, outer = TRUE, col = 'black', cex = xTOPlab.cex.fbc, font = xTOPlab.f.fbc, at = 0.77)
  mtext('Cumulative cases', side = 3, line = xTOPlab.line.fbc, outer = TRUE, col = 'black', cex = xTOPlab.cex.fbc, font = xTOPlab.f.fbc, at = 0.25)
  mtext('Date (dd/mm)', side = 1, line = xBOTTOMlab.line.fbc, outer = TRUE, col = 'black', cex = xBOTTOMlab.cex.fbc, font = xBOTTOMlab.f.fbc)
  
  # TITLE
  mtext(paste(who.info.tab[match(countries[c], who.info.tab$country), 'full_name']), side = 3, line = 2, outer = TRUE, col = 'black', cex = 2, font = 2)
  
}



# DATA FOR MAPS ----

# tmp1<- gather(tail(d,1), 'country', 'total_cases', 2:ncol(d))[,-1]
# tmp2<- gather(tail(d.deaths,1), 'country', 'total_deaths', 2:ncol(d.deaths))[,-1]
# 
# tmp3<- d %>%
#   gather('country', 'cumcases', 2:ncol(d)) %>%
#   left_join(pops, by = 'country') %>%
#   mutate(cumcases_10k = cumcases * (10000/popsize)) %>%
#   select(date, country, cumcases_10k) %>%
#   spread(country, cumcases_10k) %>%
#   tail(., 1) %>%
#   gather('country', 'CaseperPop', 2:ncol(d))%>%
#   select(-date)
# 
# tmp4<- d.deaths %>%
#   gather('country', 'cumDeaths', 2:ncol(d.deaths)) %>%
#   left_join(pops, by = 'country') %>%
#   mutate(cumDeaths_10k = cumDeaths * (10000/popsize)) %>%
#   select(date, country, cumDeaths_10k) %>%
#   spread(country, cumDeaths_10k) %>%
#   tail(., 1) %>%
#   gather('country', 'DeathsperPop', 2:ncol(d.deaths))%>%
#   select(-date)
# 
# who_data<- left_join(iso3, pops, by = 'country') %>%
#   left_join(tmp1, 'country') %>%
#   left_join(tmp2, 'country') %>%
#   left_join(tmp3, 'country') %>%
#   left_join(tmp4, 'country') %>%
#   replace(is.na(.), 0) %>%
#   as.data.frame() %>%
#   rename(pop = popsize)


#iso3<- read.table('input_files/Africa_countryISO3.txt', header=TRUE, sep = '\t')
#iso3.temp<- rename(iso3, Country = country)


who_dt_data<- # Assemble the doubling times formatted for maps plotting
  left_join(who.info.tab[,c('ISO3', 'country')], Td.report[,1:2], by = 'country') %>%
  left_join(Td.report.deaths[,1:2], by = 'country') %>%
  rename(Dt_cases = Td.obs.x,
         Dt_deaths = Td.obs.y,
         countryterritoryCode = ISO3)%>%
  replace(is.na(.), 0)


who_dt_data$Dt_cases[which(who_dt_data$Dt_cases == 'Inf')]<- 0
who_dt_data$Dt_deaths[which(who_dt_data$Dt_deaths == 'Inf')]<- 0




# PRODUCE THE MAPS FOR THE REPORT ----

africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
africa@data %<>% left_join(who_data, by=c("ISO_A3"="countryterritoryCode"))

# Map CASES ----
breaks <- classIntervals(africa@data$total_cases, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-1
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cum_cases_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_cases", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palblue,legend.title.txt = "Cumulative Cases", legend.title.cex = 1,
           legend.values.cex = 1, legend.pos = c(-30,-40))
dev.off()

# Map CASES 10k ----
breaks <- classIntervals(africa@data$CaseperPop, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.000001
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cases_10k_pop_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "CaseperPop", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palblue,legend.title.txt = "Cum. cases per 10k pop.", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


# Map DEATHS ----
breaks <- classIntervals(africa@data$total_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-1
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cum_deaths_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_deaths", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palred,legend.title.txt = "Cumulative Deaths", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()

# Map DEATHS 10k ----
breaks <- classIntervals(africa@data$DeathsperPop, n = 6, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.0000001
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_deaths_10k_pop_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "DeathsperPop", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palred,legend.title.txt = "Cum. deaths per 10k pop.", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


# Map Dt CASES ----
africa@data %<>% left_join(who_dt_data, by=c("ISO_A3"="countryterritoryCode"))

breaks <- classIntervals(africa@data$Dt_cases, n = 9, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(9, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_dt_cases_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_cases", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palgreen,legend.title.txt = "Doubling time cases (days)", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()

# 2.6: Dt DEATHS ----
breaks <- classIntervals(africa@data$Dt_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(7, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_dt_deaths_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_deaths", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palgreen,legend.title.txt = "Doubling time deaths (days)", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


# ASSEMBLE MAPS ----

#crop images and create 2x6 plot
#This assumes images are 1920x1240, will centre-crop to 1080x960 
#Read images
image1 <- image_read(paste0("./output/Map_cum_Cases_", today, "_.png"))
image2 <- image_read(paste0("./output/Map_cases_10k_pop_", today, "_.png"))
image3 <- image_read(paste0("./output/Map_cum_deaths_", today, "_.png"))
image4 <- image_read(paste0("./output/Map_deaths_10k_pop_", today, "_.png"))
image5 <- image_read(paste0("./output/Map_dt_cases_", today, "_.png"))
image6 <- image_read(paste0("./output/Map_dt_deaths_", today, "_.png"))

#Crop images
image1_crop <- image_crop(image1, "1080x960+420+140")
image2_crop <- image_crop(image2, "1080x960+420+140")
image3_crop <- image_crop(image3, "1080x960+420+140")
image4_crop <- image_crop(image4, "1080x960+420+140")
image5_crop <- image_crop(image5, "1080x960+420+140")
image6_crop <- image_crop(image6, "1080x960+420+140")

#save to 3x2 plot-
png(file = paste0("./output/6Maps_WHO_Africa_", today, "_.png"), width=1080*2, height=960*3, pointsize=22)
par(mai=rep(0,4)) # no margins
layout(matrix(1:6, ncol=2, byrow=TRUE))
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image1_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image2_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image3_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image4_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image5_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
rasterImage(image6_crop, 0, 0, 1,1)
dev.off()


# SAVE OUTPUT RData ----
save.image(paste0('output/WHO_report_analysis_', today, '.RData'))




d.10k<- # Cumulative cases per 10k
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases per 10k popula') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame()


pairs<- expand.grid(colnames(d.10k[,-1]), colnames(d.10k[,-1]))
pairs$Var1<- as.character(pairs$Var1)
pairs$Var2<- as.character(pairs$Var2)
pairs$time.diff<- NA
for(i in 1:nrow(pairs)){
  
  pairs$time.diff[i]<- epidemic.diff(d.10k, focal.country = as.character(pairs[i,1]), vs.country =  as.character(pairs[i,2]))
  
}


head(pairs[order(pairs$time.diff, na.last = TRUE, decreasing = TRUE), ])


time.diff.df<-
  pairs[order(pairs$time.diff, na.last = TRUE, decreasing = TRUE), ] %>%
  rename(`focal country` = Var1,
         `compared to` = Var2,
         `Time difference (days)` = time.diff)

time.diff.df$`focal country`<- who.info.tab[match(time.diff.df$`focal country`, who.info.tab$country), 'name_plot']
time.diff.df$`compared to`<- who.info.tab[match(time.diff.df$`compared to`, who.info.tab$country), 'name_plot']



time.diff.df$epidemic.diff.text<- formatC(time.diff.df$`Time difference (days)`, digits = 1, format = "f")


time.diff.df$`focal country`<- factor(time.diff.df$`focal country`, levels = rev(unique(as.character(time.diff.df$`focal country`))))

time.diff.df$`compared to`<- factor(time.diff.df$`compared to`, levels = unique(as.character(time.diff.df$`compared to`)))


ggplot(time.diff.df, aes(x = `compared to`, y = `focal country`, fill= `Time difference (days)`)) + 
  geom_tile()+
  xlab('')+ylab('')+
  scale_fill_gradient2(low="red", high="forestgreen", midpoint = 0) +
  geom_text(aes(label = epidemic.diff.text, x = `compared to`, y = `focal country`), size = 2, fontface = 2)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  theme(#legend.position="none",
    panel.border= element_rect(color = 'black', size = 0.5),
    axis.text.y = element_text(face="bold", colour="black", size=10),
    axis.text.x = element_text(colour="black", face="bold", size=10, angle = 45, hjust = 0, vjust = -0.2),
    axis.title.y = element_text(face="bold", colour="black", size=15),
    axis.title.x = element_text(face="bold", colour="black", size=15),
    axis.line.y = element_line(color="black", size = 0.5),
    axis.line.x = element_line(color="black", size = 0.5),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
    panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))


time.diff.df$pair<- NA

for(i in 1:nrow(time.diff.df)){
  
  print(i)
  
  focal.pair<- c(as.character(time.diff.df[i,1]), as.character(time.diff.df[i,2]))
  
  j = i+1
  
  if(j > nrow(time.diff.df)){
    break
  }
  
  while(
    length(intersect(
      focal.pair,
      c(as.character(time.diff.df[j,1]), as.character(time.diff.df[j,2])))) < 2){
    
    j = j+1
    
    if(j > nrow(time.diff.df)){
      break
    }
    
    match = j
    #print(match)
  }
  
  time.diff.df[match, 'pair']<- paste0('matches ', i)
  
}


upper<- time.diff.df[-which(na.omit(time.diff.df$`Time difference (days)`) < 0),]


ggplot(upper, aes(x = `compared to`, y = `focal country`, fill= `Time difference (days)`)) + 
  geom_tile()+
  xlab('')+ylab('')+
  scale_fill_gradient(low="white", high="firebrick") +
  geom_text(aes(label = epidemic.diff.text, x = `compared to`, y = `focal country`), size = 2, fontface = 2)+
  scale_x_discrete(position = "top")+
  theme_bw()+
  theme(#legend.position="none",
    panel.border= element_rect(color = 'black', size = 0.5),
    axis.text.y = element_text(face="bold", colour="black", size=10),
    axis.text.x = element_text(colour="black", face="bold", size=10, angle = 45, hjust = 0, vjust = -0.2),
    axis.title.y = element_text(face="bold", colour="black", size=15),
    axis.title.x = element_text(face="bold", colour="black", size=15),
    axis.line.y = element_line(color="black", size = 0.5),
    axis.line.x = element_line(color="black", size = 0.5),
    plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5),
    panel.grid.major = element_line(color = 'grey', linetype = 'dotted'))

























