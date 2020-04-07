# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   SCRIPT ANALYSING THE WHO DATA    #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# DIRECTORIES & DATE SET UP ----

setwd('/Users/s1687811/Documents/GitHub/covid19/WHO_report/')
today<- Sys.Date() - 4


library(plyr)

# LOAD DATA & SOURCE USEFUL FUNCTIONS ----
source('/Users/s1687811/Documents/GitHub/covid19/script/sourced_functions_doublingTime_reports.R')


who.tab<- read.csv('./input_files/WHO_countries.txt', sep = '\t', header=FALSE) # WHO list of countries
colnames(who.tab)<- c('country', 'name_plot', 'full_name')

who<- as.character(read.csv('./input_files/WHO_countries.txt', sep = '\t', header=FALSE)[,1])
pops<- read_excel('./input_files/Africa_popsizes.xlsx') %>% as.data.frame()

d<- 
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, ' (WHO).xlsx'), sheet = 2) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  select(c(date, which(colnames(.) %in% who)))

who.in<- who[is.element(who, colnames(d))]
who.out<- who[!is.element(who, colnames(d))]

d.long<-
  d %>%
  gather('country', 'cumNumCases', 2:ncol(d))

d.deaths<- 
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, ' (WHO).xlsx'), sheet = 1) %>%
  mutate(date = as.Date(date)) %>%
  as.data.frame() %>%
  select(c(date, which(colnames(.) %in% who)))

d.deaths.long<-
  d.deaths %>%
  gather('country', 'cumNumDeaths', 2:ncol(d.deaths))


d.10k.log.long<- 
  d %>%
  gather('country', 'cumcases', 2:ncol(d)) %>%
  left_join(pops, by = 'country') %>%
  mutate(cumcases_10k = cumcases * (10000/popsize)) %>%
  na_if(0) %>%
  mutate(cumcases_10k_log = log10(cumcases_10k)) %>%
  select(date, country, cumcases_10k_log)

d.10k.log<-
  d.10k.log.long %>%
  spread(country, cumcases_10k_log)


d.deaths.10k.log.long<- 
  d.deaths %>%
  gather('country', 'cumdeaths', 2:ncol(d.deaths)) %>%
  left_join(pops, by = 'country') %>%
  mutate(cumdeaths_10k = cumdeaths * (10000/popsize)) %>%
  na_if(0) %>%
  mutate(cumdeaths_10k_log = log10(cumdeaths_10k)) %>%
  select(date, country, cumdeaths_10k_log)


d.deaths.10k.log<-
  d.deaths.10k.log.long %>%
  spread(country, cumdeaths_10k_log)



# Producing Aligned curves dataset used in figure 6
aligned_curves<- data.frame(day_since_start = c(0, cumsum(as.numeric(diff(d$date)))))

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


# FUNCTION PLOTTING BY COUNTRY FIGURES ----

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
  mtext(paste(who.tab[match(countries[c], who.tab$country), 'full_name']), side = 3, line = 2, outer = TRUE, col = 'black', cex = 2, font = 2)
  
}


# DOUBLING TIMES RUN: Incidence----


Td.report<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())

its = 10

regions<- colnames(d)[-1]

sims.store<- vector('list', length = length(regions))


for(r in 1:length(regions)){
  
  d.clean<- # For focal country
    d[,c(1, which(colnames(d) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>%
    as.data.frame()
  
  
  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE) # Simulate poisson error on raw cases
  
  
  d.clean.sim.cum<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(pops[pops$country == regions[r],'popsize']))) %>%
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  
  t2<- tail(tail(d.clean.sim.cum$date, 8), 1) # t2: latest date
  t1<- head(tail(d.clean.sim.cum$date, 8), 1) # t1: t2 - 7 available time steps
  
  
  d.clean.sim.cum.list<- # Apply compute.td.m1.v2 on each dataset (via sapply)
    d.clean.sim.cum %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.cum.list, Td.lapply, dates = d.clean.sim.cum$date, t1 = t1, t2 = t2)
  
  # Observed and bootstrap distribution of Tds
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] # NAs come from cases where there were still zero cases observed at t1 in a simulated dataset. Normal to happen when still very low number of cases observed. Becomes less of a problem as number of cases rise.
  
  # Get CI from distribution of Td
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  #print(paste0(regions[r], ' : ', Td.obs, ' (', ci.low , ' - ', ci.upp , ')'))
  
  Td.report<- rbind(Td.report,
                    data.frame(variable = paste(regions[r]),
                               Td.obs = Td.obs[[1]],
                               ci.low = ci.low,
                               ci.upp = ci.upp,
                               t1 = t1,
                               t2 = t2)
  )
  
  
  d.clean.sim.cum$country = regions[r]
  sims.store[[r]]<- d.clean.sim.cum
  colnames(sims.store[[r]])[2]<- 'observed'
  
} 


Td.report<-
  Td.report %>%
  rename(country = variable)

Td.report.clean<- 
  Td.report %>%
  #rename(country = variable) %>%
  left_join(who.tab, by = 'country') %>%
  select(full_name, Td.obs, ci.low, ci.upp) %>%
  rename(Country = full_name,
         `Incidence doubling time (days)` = Td.obs)

has.pb<- which(
  Td.report.clean$ci.upp %in% c(NA, 'NA', Inf, 'Inf') |
    Td.report.clean$ci.low %in% c(NA, 'NA', Inf, 'Inf') |
    Td.report.clean$`Incidence doubling time (days)` %in% c(NA, 'NA', Inf, 'Inf')
  )


Td.report.clean[has.pb,2:4]<- 'NA'

Td.report.clean2<- 
  Td.report.clean %>%
  arrange(as.numeric(`Incidence doubling time (days)`))



# DOUBLING TIMES RUN: Deaths----


Td.report.deaths<- data.frame(variable = character(), Td.obs = numeric(), ci.low = numeric(), ci.upp = numeric(), t1 = character(), t2 = character())

its = 10

regions<- colnames(d.deaths)[-1]

sims.store.deaths<- vector('list', length = length(regions))


for(r in 1:length(regions)){
  
  d.clean<- # For focal country
    d.deaths[,c(1, which(colnames(d.deaths) == regions[r]))] %>%
    rename(cumNumCases = paste(regions[r])) %>% # naming 'cumNumCases' because that's how sim.epi takes variable as argument. but it is indeed DEATHS (cases of deaths ...)
    as.data.frame()
  
  
  d.clean.sim<- sim.epi(d.clean, its = its, plotsim = FALSE) # Simulate poisson error on raw DEATHS
  
  d.clean.sim.cum<-
    d.clean.sim %>%
    select(-date, -cumNumCases) %>%
    mutate_all(~ cumsum(.) * (10000/(pops[pops$country == regions[r],'popsize']))) %>%
    rename(cumNumCases = numNewCases) %>%
    cbind(date = d.clean.sim$date) %>%
    select(c(ncol(.), seq(1, ncol(.)-1)))
  
  
  t2<- tail(tail(d.clean.sim.cum$date, 8), 1) # t2: latest date
  t1<- head(tail(d.clean.sim.cum$date, 8), 1) # t1: t2 - 7 available time steps
  
  
  d.clean.sim.cum.list<- # Apply compute.td.m1.v2 on each dataset (via sapply)
    d.clean.sim.cum %>%
    select(-date) %>%
    as.list()
  
  Tds<- sapply(d.clean.sim.cum.list, Td.lapply, dates = d.clean.sim.cum$date, t1 = t1, t2 = t2)
  
  # Observed and bootstrap distribution of Tds
  Td.obs<- as.numeric(Tds['cumNumCases'])
  Td.bootstrap<- as.numeric(Tds[-which(names(Tds) == 'cumNumCases')])
  Td.bootstrap<- Td.bootstrap[!is.na(Td.bootstrap == TRUE)] # NAs come from cases where there were still zero cases observed at t1 in a simulated dataset. Normal to happen when still very low number of cases observed. Becomes less of a problem as number of cases rise.
  
  # Get CI from distribution of Td
  ci.low<- round(quantile(Td.bootstrap, c(0.05), method = 6), 1)[[1]]
  ci.upp<- round(quantile(Td.bootstrap, c(0.95), method = 6), 1)[[1]]
  
  #print(paste0(regions[r], ' : ', Td.obs, ' (', ci.low , ' - ', ci.upp , ')'))
  
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
  #rename(country = variable) %>%
  left_join(who.tab, by = 'country') %>%
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


# Last day Incidence & CI, raw ----

dat.cumsumraw<- vector('list', length = length(regions))

for(i in 1:length(sims.store)){
  
  latest<- sims.store[[i]] %>%
    #mutate(region = names(sims.store[i])) %>%
    rename(cumNumCase_raw = observed) %>%
    tail(., 1)
  
  latest[,2:(ncol(latest)-1)]<- latest[,2:(ncol(latest)-1)] * ((pops[pops$country == latest$country,'popsize'])/10000)
  
  
  cumNumCases_raw.sim.last<- as.numeric(latest[,which(substr(colnames(latest), 1, 1) == 'V')])
  
  ci.low.cumNumCases_raw<- quantile(cumNumCases_raw.sim.last, c(0.05), method = 6, na.rm = TRUE)[[1]]
  ci.upp.cumNumCases_raw<- quantile(cumNumCases_raw.sim.last, c(0.95), method = 6, na.rm = TRUE)[[1]]
  
  dat.cumsumraw[[i]]<-
    latest %>%
    select(date, country, cumNumCase_raw) %>%
    mutate(ci.low = ci.low.cumNumCases_raw,
           ci.upp = ci.upp.cumNumCases_raw)
}

dat.cumsumraw.df<- do.call('rbind', dat.cumsumraw) %>%
  select(country, cumNumCase_raw, ci.low, ci.upp) %>%
  arrange(-cumNumCase_raw) %>%
  rename(country = country,
         `Cum. Incidence` = cumNumCase_raw)

dat.cumsumraw.df$`Cum. Incidence`<- formatC(dat.cumsumraw.df$`Cum. Incidence`, digits = 0, format = "f")
dat.cumsumraw.df$ci.low<- formatC(dat.cumsumraw.df$ci.low, digits = 0, format = "f")
dat.cumsumraw.df$ci.upp<- formatC(dat.cumsumraw.df$ci.upp, digits = 0, format = "f")


dat.cumsumraw.df<- dat.cumsumraw.df %>%
  left_join(who.tab, by = 'country') %>%
  select(full_name, `Cum. Incidence`, ci.low, ci.upp) %>%
  rename(Country = full_name)


dat.cumsumraw.df$`Cum. Incidence`[dat.cumsumraw.df$`Cum. Incidence` == 'NA']<- 0


# Last day Incidence & CI 10k----

dat.cumsum10k<- vector('list', length = length(regions))

for(i in 1:length(sims.store)){
  
  latest<- sims.store[[i]] %>%
    #mutate(region = names(sims.store[i])) %>%
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
  left_join(who.tab, by = 'country') %>%
  select(full_name, `Cum. Incidence per 10k pop.`, ci.low, ci.upp) %>%
  rename(Country = full_name)



dat.cumsum10k.df$`Cum. Incidence per 10k pop.`[dat.cumsum10k.df$`Cum. Incidence per 10k pop.` == '   NA']<- 0



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


# STUFF FOR MAPS ----

tmp1<- gather(tail(d,1), 'country', 'total_cases', 2:ncol(d))[,-1]
tmp2<- gather(tail(d.deaths,1), 'country', 'total_deaths', 2:ncol(d.deaths))[,-1]

tmp3<- d %>%
  gather('country', 'cumcases', 2:ncol(d)) %>%
  left_join(pops, by = 'country') %>%
  mutate(cumcases_10k = cumcases * (10000/popsize)) %>%
  select(date, country, cumcases_10k) %>%
  spread(country, cumcases_10k) %>%
  tail(., 1) %>%
  gather('country', 'CaseperPop', 2:ncol(d))%>%
  select(-date)

tmp4<- d.deaths %>%
  gather('country', 'cumDeaths', 2:ncol(d.deaths)) %>%
  left_join(pops, by = 'country') %>%
  mutate(cumDeaths_10k = cumDeaths * (10000/popsize)) %>%
  select(date, country, cumDeaths_10k) %>%
  spread(country, cumDeaths_10k) %>%
  tail(., 1) %>%
  gather('country', 'DeathsperPop', 2:ncol(d.deaths))%>%
  select(-date)


iso3<- read.table('input_files/Africa_countryISO3.txt', header=TRUE, sep = '\t')


who_data<- left_join(iso3, pops, by = 'country') %>%
  left_join(tmp1, 'country') %>%
  left_join(tmp2, 'country') %>%
  left_join(tmp3, 'country') %>%
  left_join(tmp4, 'country') %>%
  replace(is.na(.), 0) %>%
  as.data.frame() %>%
  rename(pop = popsize)



iso3.temp<- rename(iso3, Country = country)
who_dt_data<-
left_join(iso3, Td.report[,1:2][,1:2], by = 'country') %>%
  left_join(Td.report.deaths[,1:2], by = 'country') %>%
  rename(Dt_cases = Td.obs.x,
         Dt_deaths = Td.obs.y)%>%
  replace(is.na(.), 0)


who_dt_data$Dt_cases[which(who_dt_data$Dt_cases == 'Inf')]<- 0
who_dt_data$Dt_deaths[which(who_dt_data$Dt_deaths == 'Inf')]<- 0



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
library(geojsonio)

# 2) MAPS FOR REPORT ----

africa <- geojson_read("./input_files/Africa1.geojson", what="sp")

#import WHO data and link to map-data
#who_data <- read_csv("Africa_2020-04-02.csv")

#who_data NOW A TABLE PRODUCED BY THE ANALYSIS SCRIPT
africa@data %<>% left_join(who_data, by=c("ISO_A3"="countryterritoryCode"))


# 2.1: CASES ----
breaks <- classIntervals(africa@data$total_cases, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-1
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = "./output/Map_cum_cases.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_cases", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palblue,legend.title.txt = "Cumulative Cases", legend.title.cex = 1,
           legend.values.cex = 1, legend.pos = c(-30,-40))
dev.off()

# 2.2: CASES 10k ----
breaks <- classIntervals(africa@data$CaseperPop, n = 8, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.000001
palblue <- brewer.pal(9, name = "Blues")
palblue[1]<-"#FFFFFF"
png(filename = "./output/Map_cases_10k_pop.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "CaseperPop", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palblue,legend.title.txt = "Cum. cases per 10k pop.", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


# 2.3: DEATHS ----
breaks <- classIntervals(africa@data$total_deaths, n = 6, style = "jenks", na.rm=T)$brks
breaks[2]<-1
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = "./output/Map_cum_deaths.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "total_deaths", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palred,legend.title.txt = "Cumulative Deaths", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()

# 2.4: DEATHS 10k ----
breaks <- classIntervals(africa@data$DeathsperPop, n = 6, style = "jenks", na.rm=T)$brks
breaks <- c(0,breaks)
breaks[2]<-0.0000001
palred <- brewer.pal(7, name = "Reds")
palred[1]<-"#FFFFFF"
png(filename = "./output/Map_deaths_10k_pop.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "DeathsperPop", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palred,legend.title.txt = "Cum. deaths per 10k pop.", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


# 2.5: Dt CASES ----
# who_dt_data <- read_csv("Africa_Dts.csv") NOW AN OUTPUT FROM THE ANALYSIS SCRIPT
africa@data %<>% left_join(who_dt_data, by=c("ISO_A3"="countryterritoryCode"))

breaks <- classIntervals(africa@data$Dt_cases, n = 9, style = "jenks", na.rm=T)$brks
breaks[2]<-0.00001
palgreen <- brewer.pal(9, name = "Greens")
palgreen <- rev(palgreen)
palgreen[1]<-"#FFFFFF"
png(filename = "./output/Map_dt_cases.png", width=1920, height=1240, pointsize = 22)
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
png(filename = "./output/Map_dt_deaths.png", width=1920, height=1240, pointsize = 22)
choroLayer(spdf = africa, var = "Dt_deaths", colNA = "grey", legend.nodata = "Non WHO-Africa country",
           breaks=breaks, col=palgreen,legend.title.txt = "Doubling time deaths (days)", legend.title.cex = 1, 
           legend.values.cex = 1, legend.values.rnd = 3, legend.pos = c(-30,-40))
dev.off()


# 3) ASSEMBLE ----

#crop images and create 2x6 plot
#This assumes images are 1920x1240, will centre-crop to 1080x960 
#Read images
image1 <- image_read("./output/Map_cum_Cases.png")
image2 <- image_read("./output/Map_cases_10k_pop.png")
image3 <- image_read("./output/Map_cum_deaths.png")
image4 <- image_read("./output/Map_deaths_10k_pop.png")
image5 <- image_read("./output/Map_dt_cases.png")
image6 <- image_read("./output/Map_dt_deaths.png")

#Crop images
image1_crop <- image_crop(image1, "1080x960+420+140")
image2_crop <- image_crop(image2, "1080x960+420+140")
image3_crop <- image_crop(image3, "1080x960+420+140")
image4_crop <- image_crop(image4, "1080x960+420+140")
image5_crop <- image_crop(image5, "1080x960+420+140")
image6_crop <- image_crop(image6, "1080x960+420+140")

#save to 3x2 plot-
png(file = "./output/6Maps_WHO_Africa.png", width=1080*2, height=960*3, pointsize=22)
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



#Map with countries in WHO-Africa for front page.
# africa@data$WHOCountry <- ifelse(is.na(africa@data$country.y),0,1)
# png(file = "./output/WHO_Africa.png", width=1080*2, height=960*3, pointsize=22)
# typoLayer(spdf = africa, var = "WHOCountry", col = c("forestgreen", "white"), legend.pos = "n")
# dev.off()

#Dt calculations
#Dt_Cases = africa@data %>% group_by(countriesAndTerritories) %>% arrange(countriesAndTerritories,dateRep) %>% mutate(Dt=7*log(2)/log(nth(cumCases,-1)/nth(cumCases,-8))) %>% summarise(Dt=max(Dt))
# 
# Dt_Deaths = africa_data %>% group_by(countriesAndTerritories) %>% arrange(countriesAndTerritories,dateRep) %>% mutate(Dt=7*log(2)/log(nth(cumDeaths,-1)/nth(cumDeaths,-8))) %>% summarise(Dt=max(Dt))



# SAVE OUTPUT RData ----

save.image(paste0('output/WHO_report_analysis_', today, '.RData'))


