# ARCHIVED CODE


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















