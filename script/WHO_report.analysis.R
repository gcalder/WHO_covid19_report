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
today<- Sys.Date() - 1 # Set date as to that of the data to fetch.
its = 1000 # Number of iterations for the poisson error simulation (bootstrap), Set to 1000. Or 10 for a quick test.
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
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
         `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() #%>%
  #select(c(date, which(colnames(.) %in% who.info.tab$country)))

d.long<- d %>% gather('country', 'cumNumCases', 2:ncol(d)) # long version, more practical for certain analyses/figures

d.deaths<-  # Get cumulative deaths  data
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative deaths') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
         `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame()

d.deaths.long<-  d.deaths %>% gather('country', 'cumNumDeaths', 2:ncol(d.deaths))

d.10k.log.long<- # Cumulative cases per 10k, log10()-ed
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases per 10k popula') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
         `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
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
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
         `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  gather('country', 'cumdeaths_10k', 2:ncol(.)) %>%
  na_if(0) %>%
  mutate(cumdeaths_10k_log = log10(cumdeaths_10k)) %>%
  select(-cumdeaths_10k)

d.deaths.10k.log<- # Wide version of the above
  d.deaths.10k.log.long %>%
  spread(country, cumdeaths_10k_log)


d.deaths.10k<- # Cumulative deaths per 10k population
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative deaths per 10k popul') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
         `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame()


who_data<- read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'data for map') 


#Map with countries in WHO-Africa for front page.
africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
africa@data %<>% left_join(who_data, by=c("ISO_A3"="countryterritoryCode"))
africa@data$WHOCountry <- ifelse(is.na(africa@data$location),0,1)
png(file = "input_files/WHO_Africa3.png", width=3246, height=2880, pointsize=22)
typoLayer(spdf = africa, var = "WHOCountry", col = c("darkorange2", "white"), legend.pos = "n")
dev.off()


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
  
  # Get data for focal country, rename cumulative cases columns "cumNumCases", to pass it through sim.epi() function + # Additional step: cleaning
  
  d.select<- 
    d[,c(1, which(colnames(d) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%  
    mutate(numNewCases = c(cumNumCases[1], diff(cumNumCases))) %>%
    select(date, numNewCases)
  

  d.clean.0<- d.select
  
  while(sum(d.clean.0[,2] < 0) != 0){ # Re-iterate smoothing until no negative point is left!
    
    d.clean.0<- data.cleaner(d.clean.0)
    
  }
  
  d.clean <- d.clean.0 %>%
    mutate(cumNumCases = cumsum(numNewCases)) %>%  
    select(date, cumNumCases) %>%
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

Td.report %<>% rename(country = variable)

# At present (early April) there are still very few cases in most countries.
# This causes issue either to compute Td (divisions by zero) or with the poisson error simulation (poisson process with 0 mean most of the time = generate only zero data)
# Identify all countries for which there were an issue arising from this, causing numerical issue either in Td computation or in the 95%CI computation, and set both Td and CIs to NA, as these are not reliable.
# Do this for the reported table only. For the figure, these will get trimed out anyway ...? # TOREVIEW

# DOUBLING TIMES: Deaths----
# This sections works exactly as the previous one but for deaths.
# The output is stored in the Td.report.deaths tables and sims.store.deaths list

t2<-  d.deaths$date[which.min(abs(d.deaths$date-t2.define))]
t1<- d.deaths$date[which.min(abs(d.deaths$date-(t1.define)))]
Td.report.deaths<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())
regions<- colnames(d.deaths)[-1]
sims.store.deaths<- vector('list', length = length(regions))


for(r in 1:length(regions)){
  
  
  d.select<- # Additional step: cleaning
    d.deaths[,c(1, which(colnames(d.deaths) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%  # naming 'cumNumCases' because that's how sim.epi() takes variable as argument. But it is indeed DEATHS (cases of deaths ...). A bit confusing, sim.epi() function may be improved to make it general and avoid forcing the variable be named 'cumNumCases' ...
    mutate(numNewCases = c(cumNumCases[1], diff(cumNumCases))) %>%
    select(date, numNewCases)
  
  
  d.clean.0<- d.select
  
  while(sum(d.clean.0[,2] < 0) != 0){ # Re-iterate smoothing until no negative point is left!
    
    d.clean.0<- data.cleaner(d.clean.0)
    
  }
  
  
  d.clean <- d.clean.0 %>%
    mutate(cumNumCases = cumsum(numNewCases)) %>%  
    select(date, cumNumCases) %>%
    as.data.frame()
  

  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE) # Simulate poisson error on raw DEATHS
  
  d.clean.sim.cum<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(who.info.tab[who.info.tab$country == regions[r], 'popsize']))) %>%
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

Td.report.deaths %<>% rename(country = variable)


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
         `Cum. reported cases` = cumNumCase_raw)
dat.cumsumraw.df$`Cum. reported cases`<- formatC(dat.cumsumraw.df$`Cum. reported cases`, digits = 0, format = "f") # Format numbers to be with 1 digit, even those with 0 digits (e.g. 4 --> 4.0)
dat.cumsumraw.df$ci.low<- formatC(dat.cumsumraw.df$ci.low, digits = 0, format = "f")
dat.cumsumraw.df$ci.upp<- formatC(dat.cumsumraw.df$ci.upp, digits = 0, format = "f")
dat.cumsumraw.df<- dat.cumsumraw.df %>%
  left_join(who.info.tab, by = 'country') %>%
  select(full_name, `Cum. reported cases`, ci.low, ci.upp) %>%
  rename(Country = full_name)
dat.cumsumraw.df$`Cum. reported cases`[dat.cumsumraw.df$`Cum. reported cases` == 'NA']<- 0


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
         `Cum. reported cases per 10k pop.` = cumNumCase_10k)
dat.cumsum10k.df$`Cum. reported cases per 10k pop.`<- formatC(dat.cumsum10k.df$`Cum. reported cases per 10k pop.`, digits = 4, format = "f")
dat.cumsum10k.df$ci.low<- formatC(dat.cumsum10k.df$ci.low, digits = 4, format = "f")
dat.cumsum10k.df$ci.upp<- formatC(dat.cumsum10k.df$ci.upp, digits = 4, format = "f")
dat.cumsum10k.df<- dat.cumsum10k.df %>%
  left_join(who.info.tab, by = 'country') %>%
  select(full_name, `Cum. reported cases per 10k pop.`, ci.low, ci.upp) %>%
  rename(Country = full_name)
dat.cumsum10k.df$`Cum. reported cases per 10k pop.`[dat.cumsum10k.df$`Cum. reported cases per 10k pop.` == '   NA']<- 0



# By-COUNTRY PLOTS FUNC (1)  ----
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
  mtext(paste0('Date (dd',str_sub(date.format, 3, 3),'mm)'), side = 1, line = xBOTTOMlab.line.fbc, outer = TRUE, col = 'black', cex = xBOTTOMlab.cex.fbc, font = xBOTTOMlab.f.fbc)
  
  # TITLE
  mtext(paste(who.info.tab[match(countries[c], who.info.tab$country), 'full_name']), side = 3, line = 2, outer = TRUE, col = 'black', cex = 2, font = 2)
  
}


# By-COUNTRY PLOTS FUNC (2) ----
# New version, with added fan plots
plot.country.2<- function(c){
  
  # SET UP THE PLOT AREA
  par(mfrow = c(3,2),
      oma = c(2,2,4,1) + 0.1,
      mar = c(2,2,1,1) + 0.1)
  
  
  # RAW CASES ----
  # Set plot, add axes, add background gridlines
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.raw.cases), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.raw.cases, font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.raw.cases, col = 'lightgrey', lty = 'dotted')
  for(c2 in 2:ncol(d)){
    lines(d[,c2] ~ d$date, lwd = 1.5, col = 'grey')
  } # Draw lines + overplot focal country line
  lines(d[,c] ~ d$date, lwd = 3)
  
  
  # DEATHS RAW ----
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.raw.deaths), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.raw.deaths, font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.raw.deaths, col = 'lightgrey', lty = 'dotted')  
  for(c2 in 2:ncol(d.deaths)){
    lines(d.deaths[,c2] ~ d.deaths$date, lwd = 1.5, col = 'grey')
  } # Draw lines + overplot focal country line
  lines(d.deaths[,c] ~ d.deaths$date, lwd = 3)
  
  # LOG CASES ----
  # Set plot, add axes, add background gridlines
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.log.cases), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.log.cases, labels = as.character(10^yseq.log.cases), font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.log.cases, col = 'lightgrey', lty = 'dotted')
  for(c2 in 2:ncol(d.10k.log)){
    lines(d.10k.log[,c2] ~ d.10k.log$date, lwd = 1.5, col = 'grey')
  } # Draw lines + overplot focal country line
  lines(d.10k.log[,c] ~ d.10k.log$date, lwd = 3)
  
  
  # DEATHS LOG ----
  plot('', xlim = range(xseq.fbc), ylim = range(yseq.log.deaths), xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  axis(1, at = xseq.fbc, labels = format(xseq.fbc, date.format), font = x.font.fbc, cex.axis = x.cex.fbc)
  axis(2, at = yseq.log.deaths, labels = as.character(10^yseq.log.deaths), font = y.font.fbc, cex.axis = y.cex.fbc)
  abline(v = xseq.fbc, h = yseq.log.deaths, col = 'lightgrey', lty = 'dotted')  
  for(c2 in 2:ncol(d.deaths.10k.log)){
    lines(d.deaths.10k.log[,c2] ~ d.deaths.10k.log$date, lwd = 1.5, col = 'grey')
  }
  lines(d.deaths.10k.log[,c] ~ d.deaths.10k.log$date, lwd = 3)
  
  
  
  
  
  
  
  # FAN PLOT CASES ----
  
  # Get the cumulative incidence for today and 7 days ago
  d.today.and.todayMinus7<- d[which(d$date %in% c(today, today-7)), ]
  
  # Take incidence today, log10 it. This is y2.OBS
  y2.obs<- d.today.and.todayMinus7[2,] %>%
    gather('country', 'cases.today', 2:ncol(.)) %>%
    mutate(`obs.log10_cases.today` = log10(cases.today)) %>%
    select(country, obs.log10_cases.today)
  
  # Take incidence 7 days ago, log10 it, add ((7*log10(2))/Td) to it, where Td is the doubling time for that country, get back to linear scale, that's the PREDICTED Y2. Then format dataframe to wide shape.
  y1.dt.y2Pred<- d.today.and.todayMinus7[1,] %>%
    gather('country', 'cases.-7', 2:ncol(.)) %>%
    mutate(`log10_cases.-7` = log10(`cases.-7`)) %>%
    left_join(who_dt_data, 'country') %>%
    mutate(pred.log10_cases.today = `log10_cases.-7` + ((7*log10(2))/Dt_cases)) %>%
    #left_join(y2.obs, "country")
    mutate(`cases.-7` = 10^(`log10_cases.-7`), # back to linear scale, per 10k
           pred.cases.today = 10^(pred.log10_cases.today)) %>%
    select(country, `cases.-7`, pred.cases.today) %>%
    gather('time','y1.or.y2', 2:3) %>%
    spread(country, y1.or.y2) %>%
    mutate(time = ifelse(time == 'cases.-7', as.character(today - 7), as.character(today))) %>%
    rename(date = time)
  
  # Get the LOG10 of the ratio of Y2 PREDICTED over Y1 OBSERVED
  test<- y1.dt.y2Pred %>%
    gather('country', 'n', 2:ncol(.)) %>%
    group_by(country) %>%
    mutate(log10increase.7days = log10(n/n[1]))
  #increase.7days = n/n[1])
  
  # Format dataframe for plottinf with basegraphic, set all NA and Inf to NA
  test.for.basegraphic<- test %>%
    select(date, country, log10increase.7days)
  test.for.basegraphic$log10increase.7days[which(!is.finite(test.for.basegraphic$log10increase.7days))]<- NA
  test.for.basegraphic<- test.for.basegraphic %>%  
    spread(country, log10increase.7days) %>%
    as.data.frame()
  
  # Set the plot scaled by the maximum increase, add all lines, overplot the focal country line (c)
  max.increase.country<- names(which.max(test.for.basegraphic[2,-1]))
  plot(test.for.basegraphic[,max.increase.country] ~ c(0,1), data = test.for.basegraphic,
       type = 'l',
       xlim = c(0,1.2),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  for(c2 in 2:ncol(test.for.basegraphic)){
    lines(test.for.basegraphic[,c2] ~ c(0,1), lwd = 1.5, col = 'grey')
  }
  abline(v = c(0,1), h = log10(seq(1:10)), col = 'lightgrey', lty = 'dotted')  
  
  lines(test.for.basegraphic[,c] ~ c(0,1), lwd = 3)
  points(test.for.basegraphic[,c] ~ c(0,1), cex = 1.2, pch = 16)
  axis(2, at = log10(seq(1:10)), labels = 10^(log10(seq(1:10))), font = y.font.fbc, cex.axis = y.cex.fbc)
  
  axis(1, at = c(0,1), labels = format(as.Date(test.for.basegraphic$date), date.format), font = x.font.fbc, cex.axis = x.cex.fbc)

  # FAN PLOTS DEATHS ----
  
  # Get the cumulative deaths for today and 7 days ago
  d.deaths.today.and.todayMinus7<- d.deaths[which(d.deaths$date %in% c(today, today-7)), ]
  
  # Take deaths today, log10 it. This is y2.OBS
  y2.obs.deaths<- d.deaths.today.and.todayMinus7[2,] %>%
    gather('country', 'deaths.today', 2:ncol(.)) %>%
    mutate(`obs.log10_deaths.today` = log10(deaths.today)) %>%
    select(country, obs.log10_deaths.today)
  
  # Take deaths 7 days ago, log10 it, add ((7*log10(2))/Td.DEATHS) to it, where Td.DEATHS is the DEATHS doubling time for that country, get back to linear scale, that's the PREDICTED Y2. Then format dataframe to wide shape.
  y1.dt.y2Pred.deaths<- d.deaths.today.and.todayMinus7[1,] %>%
    gather('country', 'deaths.-7', 2:ncol(.)) %>%
    mutate(`log10_deaths.-7` = log10(`deaths.-7`)) %>%
    left_join(who_dt_data, 'country') %>%
    mutate(pred.log10_deaths.today = `log10_deaths.-7` + ((7*log10(2))/Dt_deaths)) %>%
    #left_join(y2.obs, "country")
    mutate(`deaths.-7` = 10^(`log10_deaths.-7`), # back to linear scale, per 10k
           pred.deaths.today = 10^(pred.log10_deaths.today)) %>%
    select(country, `deaths.-7`, pred.deaths.today) %>%
    gather('time','y1.or.y2', 2:3) %>%
    spread(country, y1.or.y2) %>%
    mutate(time = ifelse(time == 'deaths.-7', as.character(today - 7), as.character(today))) %>%
    rename(date = time)
  
  # Get the LOG10 of the ratio of Y2 PREDICTED over Y1 OBSERVED
  test.deaths<- y1.dt.y2Pred.deaths %>%
    gather('country', 'n', 2:ncol(.)) %>%
    group_by(country) %>%
    mutate(log10increase.deaths.7days = log10(n/n[1]))
  #increase.7days = n/n[1])
  
  # Format dataframe for plottinf with basegraphic, set all NA and Inf to NA
  test.for.basegraphic.deaths<- test.deaths %>%
    select(date, country, log10increase.deaths.7days)
  
  test.for.basegraphic.deaths$log10increase.deaths.7days[which(!is.finite(test.for.basegraphic.deaths$log10increase.deaths.7days))]<- NA
  
  test.for.basegraphic.deaths<- test.for.basegraphic.deaths %>%  
    spread(country, log10increase.deaths.7days) %>%
    as.data.frame()
  
  # Set the plot scaled by the maximum increase, add all lines, overplot the focal country line (c)
  max.increase.country.deaths<- names(which.max(test.for.basegraphic.deaths[2,-1]))
  plot(test.for.basegraphic.deaths[,max.increase.country.deaths] ~ c(0,1), data = test.for.basegraphic.deaths,
       type = 'l',
       xlim = c(0,1.2),
       xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
  for(c2 in 2:ncol(test.for.basegraphic.deaths)){
    lines(test.for.basegraphic.deaths[,c2] ~ c(0,1), lwd = 1.5, col = 'grey')
  }
  abline(v = c(0,1), h = log10(seq(1:10)), col = 'lightgrey', lty = 'dotted')  
  
  lines(test.for.basegraphic.deaths[,c] ~ c(0,1), lwd = 3)
  points(test.for.basegraphic.deaths[,c] ~ c(0,1), cex = 1.2, pch = 16)
  axis(2, at = log10(seq(1:10)), labels = 10^(log10(seq(1:10))), font = y.font.fbc, cex.axis = y.cex.fbc)
  axis(1, at = c(0,1), labels = format(as.Date(test.for.basegraphic$date), date.format), font = x.font.fbc, cex.axis = x.cex.fbc)

  
  # ADD LABELS ----
  # Y-labels
  mtext('Cumulative', side = 2, line = ylab.line.fbc, outer = TRUE, col = 'black', cex = ylab.cex.fbc, font = ylab.f.fbc, at = 0.85)
  mtext('Per 10k population', side = 2, line = ylab.line.fbc, outer = TRUE, col = 'black', cex = ylab.cex.fbc, font = ylab.f.fbc, at = 0.5)
  mtext('Relative increase', side = 2, line = ylab.line.fbc, outer = TRUE, col = 'black', cex = ylab.cex.fbc, font = ylab.f.fbc, at = 0.15)
  
  # X-labels
  mtext('Cases', side = 3, line = xTOPlab.line.fbc, outer = TRUE, col = 'black', cex = xTOPlab.cex.fbc, font = xTOPlab.f.fbc, at = 0.25)
  mtext('Deaths', side = 3, line = xTOPlab.line.fbc, outer = TRUE, col = 'black', cex = xTOPlab.cex.fbc, font = xTOPlab.f.fbc, at = 0.77)
 
  
   mtext(paste0('Date (dd',str_sub(date.format, 3, 3),'mm)'), side = 1, line = xBOTTOMlab.line.fbc, outer = TRUE, col = 'black', cex = xBOTTOMlab.cex.fbc, font = xBOTTOMlab.f.fbc, at = 0.25)
  
   mtext(paste0('Date (dd',str_sub(date.format, 3, 3),'mm)'), side = 1, line = xBOTTOMlab.line.fbc, outer = TRUE, col = 'black', cex = xBOTTOMlab.cex.fbc, font = xBOTTOMlab.f.fbc, at = 0.77)
   
   
  # TITLE
  mtext(paste(who.info.tab[match(countries[c], who.info.tab$country), 'full_name']), side = 3, line = 2, outer = TRUE, col = 'black', cex = 2, font = 2)
  
}

# DATA FOR MAPS ----



who_dt_data<- # Assemble the doubling times formatted for maps plotting
left_join(who.info.tab[,c('ISO3', 'country')], Td.report[,1:2], by = 'country') %>%
  left_join(Td.report.deaths[,1:2], by = 'country') %>%
  rename(Dt_cases = Td.obs.x,
         Dt_deaths = Td.obs.y,
         countryterritoryCode = ISO3)#%>%
  #replace(is.na(.), 0)


who_dt_data$Dt_cases[!is.finite(who_dt_data$Dt_cases)]<- 0
who_dt_data$Dt_deaths[!is.finite(who_dt_data$Dt_deaths)]<- 0



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
choroLayer(spdf = africa, var = "total_cases", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palblue,
           legend.title.txt = "",
           legend.title.cex = 1,
           legend.values.cex = 1, 
           legend.pos = c(-30,-35)) # -30, -40

points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No case reported', adj = 0)
dev.off()





# Map CASES 10k ----
breaks <- classIntervals(africa@data$CaseperPop, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.000001
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cases_10k_pop_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "CaseperPop", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palblue,legend.title.txt = "", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No case reported', adj = 0)
dev.off()


# Map DEATHS ----
breaks <- classIntervals(africa@data$total_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-1
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_cum_deaths_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_deaths", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palred,legend.title.txt = "", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No death reported', adj = 0)
dev.off()

# Map DEATHS 10k ----
breaks <- classIntervals(africa@data$DeathsperPop, n = 6, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.0000001
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_deaths_10k_pop_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "DeathsperPop", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palred,legend.title.txt = "", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No death reported', adj = 0)
dev.off()


# Map Dt CASES ----
africa@data %<>% left_join(who_dt_data, by=c("ISO_A3"="countryterritoryCode"))



breaks <- classIntervals(africa@data$Dt_cases, n = 9, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(9, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_dt_cases_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_cases", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palgreen, legend.title.txt = "Days", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No case reported or < 7 days ago', adj = 0)
dev.off()

# Map Dt DEATHS ----
breaks <- classIntervals(africa@data$Dt_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(7, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = paste0('./output/Map_dt_deaths_', today, '_.png'), width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_deaths", colNA = "grey", legend.nodata = "Non WHO Afro country",
           breaks=breaks, col=palgreen,legend.title.txt = "Days", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-35))
points(-23.3, -31, pch = 16, col = 'white', cex = 2)
text(-24, -30, 'No death reported or < 7 days ago', adj = 0)
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
# png(file = paste0("./output/6Maps_WHO_Africa_", today, "_.png"), width=1080*2, height=960*3, pointsize=22)
# par(mai=rep(0,4)) # no margins
# layout(matrix(1:6, ncol=2, byrow=TRUE))
# plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
# rasterImage(image1_crop, 0, 0, 1,1)
# plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
# rasterImage(image3_crop, 0, 0, 1,1)
# plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
# rasterImage(image2_crop, 0, 0, 1,1)
# plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
# rasterImage(image4_crop, 0, 0, 1,1)
# plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
# rasterImage(image5_crop, 0, 0, 1,1)
# plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i')
# rasterImage(image6_crop, 0, 0, 1,1)
# dev.off()


png(file = paste0("./output/6Maps_WHO_Africa_", today, "_.png"), width=1080*2, height=960*3, pointsize=22)
par(mai=rep(0.5,4)) # no margins
layout(matrix(1:6, ncol=2, byrow=TRUE))
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED CASES', cex.main = 2)
rasterImage(image1_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED DEATHS', cex.main = 2)
rasterImage(image3_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED CASES PER 10k POPULATION', cex.main = 2)
rasterImage(image2_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'CUMULATIVE REPORTED DEATHS PER 10k POPULATION', cex.main = 2)
rasterImage(image4_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'DOUBLING TIME CASES', cex.main = 2)
rasterImage(image5_crop, 0, 0, 1,1)
plot(NA, xlim=0:1, ylim=0:1, bty="n", axes=0, xaxs = 'i', yaxs='i', main = 'DOUBLING TIME DEATHS', cex.main = 2)
rasterImage(image6_crop, 0, 0, 1,1)
dev.off()


# PAIRWISE TIME AHEAD COMPARISON ----
# d.10k<- # Cumulative cases per 10k
#   read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases per 10k popula') %>%
#     rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
# `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
#   mutate(date = as.Date(date)) %>%
#   as.data.frame()
# 
# pairs<- expand.grid(colnames(d.10k[,-1]), colnames(d.10k[,-1]))
# pairs$Var1<- as.character(pairs$Var1)
# pairs$Var2<- as.character(pairs$Var2)
# pairs$time.diff<- NA
# for(i in 1:nrow(pairs)){
#   
#   pairs$time.diff[i]<- epidemic.diff(d.10k, focal.country = as.character(pairs[i,1]), vs.country =  as.character(pairs[i,2]))
#   
# }
# 
# time.diff.df<-
#   pairs[order(pairs$time.diff, na.last = TRUE, decreasing = TRUE), ] %>%
#   rename(`focal country` = Var1,
#          `compared to` = Var2,
#          `Time difference (days)` = time.diff)
# 
# time.diff.df$`focal country`<- who.info.tab[match(time.diff.df$`focal country`, who.info.tab$country), 'name_plot']
# time.diff.df$`compared to`<- who.info.tab[match(time.diff.df$`compared to`, who.info.tab$country), 'name_plot']
# 
# time.diff.df$epidemic.diff.text<- formatC(time.diff.df$`Time difference (days)`, digits = 1, format = "f")
# time.diff.df$`focal country`<- factor(time.diff.df$`focal country`, levels = rev(unique(as.character(time.diff.df$`focal country`))))
# time.diff.df$`compared to`<- factor(time.diff.df$`compared to`, levels = unique(as.character(time.diff.df$`compared to`)))
# 
# upper<- time.diff.df[-which(na.omit(time.diff.df$`Time difference (days)`) < 0),] # This is the dataframe used for the heatmap in final report





# SAVE OUTPUT RData ----
save.image(paste0('output/WHO_report_analysis_', today, '.RData'))






















