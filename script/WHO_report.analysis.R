# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#   WHO report - analysis script     #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


# 1) SET UP ----

today<- Sys.Date() - 11 # Set date as to that of the data to fetch.
iter = 1000 # Number of iterations for the poisson error simulation (bootstrap), Set to 1000. Or 10 for a quick test.
set.seed(as.numeric(today)) # setting seed allows repeatability of poisson error simulations. Use the date as a reference point for the seed.

source('./script/sourced_functions_doublingTime_reports.R') # Source several functions used in the below script. See README file for details. 

time_window<- 7 # Time window over which doubling time is calculated
t2.define<- today
t1.define<- t2.define - time_window

# library to read excel files
library(readxl)
# large library of generic data science tools for manipulating data
library(tidyverse)
library(magrittr)
# import a relatively sensible colour palette 
library(RColorBrewer)
# class interval library to group countries together with similar cases/deaths counts 
# so that they are given the same colour
library(classInt)
# libraries to create all of the maps (require a lot of other packages to work)
library(sf)
library(geojsonio)
library(cartography)
# library with useful function for reading/manipulating dates
library(lubridate)
# library to manipulate images
library(magick)
# rolling windows for weekly ratios
library(RcppRoll)

# LOADING DATA ----

# Load data and make various versions of them (wide, long, log10, linear, etc. that are used later in the script or in the Rmd  file)

# WHO list of countries & their popsize.
# Various encoding of the countries name used for different purpose (match various data sources, for maps, shortened for figures etc.)
who_country_aliases_and_populations<-
read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'population counts per country')[,-1] %>%
  rename(ISO3 = countryterritoryCode,
         popsize = pop) %>%
  left_join(read.csv('input_files/WHO_country_list.csv', stringsAsFactors = FALSE), by = 'ISO3')


# All columns in WHO_cases_and_deaths are WHO countries.
# After modifying "Côte d'Ivoire" into "Cote d'Ivoire" (without the ^), and `São Tomé and Príncipe into Sao Tome and Principe, then the variable "country" of the who_country_aliases_and_populations is the one that matches the countries name of the data
# + formatting variable names to fit in rest of the script

WHO_cases_and_deaths<- # Get cumulative case counts data
  read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative cases') %>%
  rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
         `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
  mutate(date = as.Date(date)) %>%
  gather(key=country,value=cum_cases,-date) %>%
  # Get cumulative death counts data
  inner_join(
    read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'cumulative deaths') %>%
      rename(`Cote d'Ivoire` = `Côte d’Ivoire`,
             `Sao Tome and Principe` = `São Tomé and Príncipe`) %>%
      mutate(date = as.Date(date)) %>%
      gather(key=country,value=cum_deaths,-date),
    by=c("date","country")
  ) %>% 
  # revert back to per day deaths / cases
  group_by(country) %>%
  mutate(cum_cases=as.integer(cum_cases),cum_deaths=as.integer(cum_deaths),cases = as.integer(c(0,diff(cum_cases))), deaths = as.integer(c(0,diff(cum_deaths)))) %>%
  ungroup() %>%
  # convert to per 10k pop
  inner_join(who_country_aliases_and_populations %>% 
               select(country,popsize)) %>% 
  mutate(popsize = popsize / 10000,cum_cases_per_10k=cum_cases / popsize,cum_deaths_per_10k=cum_deaths / popsize,cases_per_10k=cases / popsize,deaths_per_10k=deaths / popsize)

# check data is complete
if(!(WHO_cases_and_deaths %>% 
  group_by(country) %>% 
  summarise(n()) %>%
  distinct(n()) %>%
  length()) == 1) stop('Some countries are missing reporting dates')

if(sum(is.na(WHO_cases_and_deaths$cases)) > 0 | sum(is.na(WHO_cases_and_deaths$deaths)) > 0) stop('Some countries have NA entries')

who_countrywide_data<- read_excel(paste0('./data/', today, '/WHO_Africa_data_', today, '.xlsx'), sheet = 'data for map') 

# Creates the begining image / logo showing the WHO AFRO Region
 africa <- geojson_read("./input_files/Africa1.geojson", what="sp")
 africa@data %<>% left_join(who_countrywide_data, by=c("ISO_A3"="countryterritoryCode"))
 africa@data$WHOCountry <- ifelse(is.na(africa@data$location),0,1)
 png(file = "input_files/WHO_Africa3.png", width=3246, height=2880, pointsize=22)
typoLayer(spdf = africa, var = "WHOCountry", col = c("darkorange2", "white"), legend.pos = "n")
text(-34, -43,"Total Reported Cases", adj = 0,col="#08519C",font=2,cex=6)
text(-18, -48,paste0(WHO_cases_and_deaths %>% filter(date == today) %>% pull(cum_cases) %>% sum()), adj = 0,col="#08519C",font=2,cex=6)
text(24, -43,"Total Reported Deaths", adj = 0,col="#CB181D",font=2,cex=6)
text(44, -48,paste0(WHO_cases_and_deaths %>% filter(date == today) %>% pull(cum_deaths) %>% sum()), adj = 0,col="#CB181D",font=2,cex=6)
 dev.off()

# DOUBLING TIMES: Incidence----
# This sections computes
# For incidence
# the Doubling Time (Td) between t1 and t2 for each country
# And simulate the epicurves (poisson error bootstraping)
# To re-estimate Td on each simulation
# To obtain a CI of the Td


# Detect any negative values 
 WHO_cases_and_deaths_negative <- WHO_cases_and_deaths %>%
  mutate(cases_positive = (cases > -1), deaths_positive = (deaths > -1)) %>%
  group_by(country) %>%
  mutate(calc_cases_dt = eight_day_pos_window(cases_positive), calc_deaths_dt = eight_day_pos_window(deaths_positive)) %>%
   ungroup()

# simulated datasets from bootstrapping
WHO_cases_and_deaths_simulated <- WHO_cases_and_deaths_negative %>%
  group_by(country,date) %>%
  mutate(sim_deaths = map(iter,rpois_error,deaths,deaths_positive),sim_cases = map(iter,rpois_error,cases,cases_positive)) %>%
  group_by(country) %>%
  mutate(sim_cum_deaths = sim_cum_calc_per_pop(sim_deaths,1,deaths_positive),
         sim_cum_cases = sim_cum_calc_per_pop(sim_cases,1,cases_positive),
         sim_cum_deaths_per_10k = sim_cum_calc_per_pop(sim_deaths,popsize,deaths_positive),
         sim_cum_cases_per_10k = sim_cum_calc_per_pop(sim_cases,popsize,cases_positive)) %>%
  ungroup()

# calculated doubling times for reported cases and for deaths
WHO_cases_and_deaths_simulated_doubling_time<- WHO_cases_and_deaths_simulated %>%
  group_by(country) %>%
  summarise(sim_cases_doubling_times = Td.lapply(sim_cum_cases_per_10k,date,t1.define, t2.define, cases_positive,iter)) %>%
  inner_join( 
    WHO_cases_and_deaths_simulated %>%
      group_by(country) %>%
    summarise(sim_deaths_doubling_times = Td.lapply(sim_cum_deaths_per_10k,date,t1.define, t2.define, deaths_positive,iter))
  ) %>% 
  ungroup()

# Get observed Td and CI from distribution of Td
WHO_cases_and_deaths_doubling_time <- WHO_cases_and_deaths_simulated_doubling_time %>%
  group_by(country)%>%
  transmute(
    deaths_ci_low = round(quantile(sim_deaths_doubling_times[[1]], c(0.05), method = 6,na.rm = TRUE), 1)[[1]], 
    deaths_ci_upp = round(quantile(sim_deaths_doubling_times[[1]], c(0.95), method = 6,na.rm = TRUE), 1)[[1]],
    cases_ci_low = round(quantile(sim_cases_doubling_times[[1]], c(0.05), method = 6,na.rm = TRUE), 1)[[1]], 
    cases_ci_upp = round(quantile(sim_cases_doubling_times[[1]], c(0.95), method = 6,na.rm = TRUE), 1)[[1]]) %>%
  inner_join(
    WHO_cases_and_deaths_simulated %>%
      filter(date %in% c(t1.define,t2.define)) %>%
      group_by(country) %>%
      summarise(cases_doubling_time = compute.td(cum_cases_per_10k)))  %>%
  inner_join(
    WHO_cases_and_deaths_simulated %>%
      filter(date %in% c(t1.define,t2.define)) %>%
      group_by(country) %>%
      summarise(deaths_doubling_time = compute.td(cum_deaths_per_10k))
  )

WHO_cases_and_deaths_doubling_time$cases_doubling_time[!(WHO_cases_and_deaths_negative %>% filter(date == max(date)) %>% pull(calc_cases_dt))] <- -1

WHO_cases_and_deaths_doubling_time$deaths_doubling_time[!(WHO_cases_and_deaths_negative %>% filter(date == max(date)) %>% pull(calc_deaths_dt))] <- -1

# calculate seven day cumulative increase
WHO_cases_and_deaths_7_day_increase <- WHO_cases_and_deaths %>%
  select(country,date) %>%
  filter(date == today | date == (today - 7)) %>%
  inner_join(WHO_cases_and_deaths_doubling_time %>%
               ungroup() %>%
               transmute(country,`doubling time`=cases_doubling_time))%>%
  spread(key= date,value = `doubling time`) %>%
  transmute(country, `2020-01-08` = 2^(7 / .[[3]]),`2020-01-01`=1) %>%
  gather(key=date,value=cum_cases_relative_increase,-country)%>%
  inner_join( 
    WHO_cases_and_deaths %>%
      select(country,date) %>%
      filter(date == today | date == (today - 7)) %>%
      inner_join(WHO_cases_and_deaths_doubling_time %>%
                   ungroup() %>%
                   transmute(country,`doubling time`=deaths_doubling_time))%>%
      spread(key= date,value = `doubling time`) %>%
      transmute(country, `2020-01-08` = 2^(7 / .[[3]]),`2020-01-01`=1) %>%
      gather(key=date,value=cum_deaths_relative_increase,-country) 
  ) %>%
  mutate(date = ymd(date))

WHO_cases_and_deaths_7_day_increase$cum_cases_relative_increase[!is.finite(WHO_cases_and_deaths_7_day_increase$cum_cases_relative_increase)] <- 1
WHO_cases_and_deaths_7_day_increase$cum_deaths_relative_increase[!is.finite(WHO_cases_and_deaths_7_day_increase$cum_deaths_relative_increase)] <- 1
WHO_cases_and_deaths_7_day_increase$date[WHO_cases_and_deaths_7_day_increase$date == "2020-01-08"] <- today
WHO_cases_and_deaths_7_day_increase$date[WHO_cases_and_deaths_7_day_increase$date == "2020-01-01"] <- (today-7)


# The TWO NEXT SECTIONS are to collect the Last day incidence of the observed data but ALSO OF THE SIMULATED DATA
# that, to be able to report a 95%CI on the observed data
# Done on the raw number of cases and the per 10k pop 

# Last day Incidence & CI, raw ----
  WHO_latest_day_cases_and_deaths_simulated <- WHO_cases_and_deaths_simulated %>%
  filter(date == today) %>%
  group_by(country)%>%
  transmute(
            last_day_case_ci_high =round(quantile(sim_cum_cases[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_high =round(quantile(sim_cum_deaths[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_case_ci_low =round(quantile(sim_cum_cases[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_low =round(quantile(sim_cum_deaths[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]],
            last_day_case_ci_high_per_10k =round(quantile(sim_cum_cases_per_10k[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_high_per_10k =round(quantile(sim_cum_deaths_per_10k[[1]], c(0.95), method = 6,na.rm = TRUE))[[1]],
            last_day_case_ci_low_per_10k =round(quantile(sim_cum_cases_per_10k[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]],
            last_day_deaths_ci_low_per_10k =round(quantile(sim_cum_deaths_per_10k[[1]], c(0.05), method = 6,na.rm = TRUE))[[1]]) %>%
  inner_join(
    WHO_cases_and_deaths %>%
      filter(date == today) %>%
      transmute(
        country,
        last_day_case_obs = cum_cases,
        last_day_deaths_obs = cum_deaths,
        last_day_case_obs_per_10k = cum_cases_per_10k,
        last_day_deaths_obs_per_10k = cum_deaths_per_10k
      )
  ) %>%
  ungroup()



# By-COUNTRY PLOTS FUNC (2) ----

# MAPS  ----

who_dt_data<- # Assemble the doubling times formatted for maps plotting
  who_country_aliases_and_populations[,c('ISO3', 'country')] %>%
  left_join(WHO_cases_and_deaths_doubling_time %>%
              select(country,cases_doubling_time,deaths_doubling_time), by = 'country') %>%
  rename(Dt_cases = cases_doubling_time,
         Dt_deaths = deaths_doubling_time,
         countryterritoryCode = ISO3)

# Set the NA and Inf Dt to zero, so that they appear in white on the maps
who_dt_data$Dt_cases[!is.finite(who_dt_data$Dt_cases)]<- 0 
who_dt_data$Dt_deaths[!is.finite(who_dt_data$Dt_deaths)]<- 0



# PRODUCE THE MAPS FOR THE REPORT ----

# Maps quick achk solution for legend: overplot a white point and then a text, to replace the "0" on the scale by a more informative legend text (0s are not real zeros here, they are NA and Inf, but the NA variable is reserved for non-WHO countries here)

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
africa@data %<>% left_join(who_dt_data %>% select(-country), by=c("ISO_A3"="countryterritoryCode"))



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
text(-24, -30, 'Non reported or adjusted < 7 days ago', adj = 0)
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
text(-24, -30, 'Non reported or adjusted < 7 days ago', adj = 0)
dev.off()


# ASSEMBLE MAPS ----

# crop images and create 3x6 plot
# This assumes images are 1920x1240, will centre-crop to 1080x960 
#Read images
image1 <- image_read(paste0("./output/Map_cum_cases_", today, "_.png"))
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


# Save a 3*2 plots image, with added titles for each map
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



# SAVE OUTPUT RData ----
save.image(paste0('output/WHO_report_analysis_', today, '.RData'))

