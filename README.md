# WHO COVID19 SITUATION REPORT

This repository is for producing a report for WHO Africa Region giving an overview and a by-country summary of the COVID-19 epidemic progression across the 47 WHO Afro Region countries.
In a nutshell, an "analysis script" loads the data, performs doubling time and 95%CIs calculations & Co. The resulting R session is saved as an RData object, which is then loaded by the Rmarkdown script, which produces the pdf/word report with the figures.

The report is released every Friday around 10am.


1) OUTSTANDING WORK IN PROGRESS

- IMPORTANT ONE: How to deal with bootstrap sample size when simulated epidemics generate NA of Inf doubling time? filtering out leads to 95%CI computed from small samples. Re-iterating simulations until reaching desired sample size would generate biased distribution... be agnostic? Leave the NAs and Inf and say there is not enough cases to compute reliable CIs. [READ compute.td.m1.v2(), sim.epi() and Td.apply() FUNCTIONS DESCRIPTION FOR MORE DETAILS]


2) FILES

./data: one directory per day. Each contains a excel file named WHO_Africa_data_2020-MM-DD.xlsx, with:
	- sheet 1: cumulative deaths
	- sheet 2: cumulative cases
	- sheet 3: cumulative cases per 10k population
	- sheet 4: cumulative deaths per 10k population
	- sheet 4: population counts per country
	- sheet 5: data for map

	As well as a csv file named gisaid_hcov-19_acknowledgement_table_2020-MM-DD.csv containing the latest meta data of SARS-CoV-2 sequences received from Africa.

Each sheet has one row per date, and one column per country (+ first column being the dates)

In current state of things, the excel file is fetched from Feifei's OneDrive (Ask for access). Feifei typically creates the latest excel spreadsheet on Thursday afternoon/evening. The GISAID csv file is received from Lu on Thursday mornings. 


./input_files: files used in analysis or Rmarkdown files (do not change! This is data (typically mapping data) required for every report)
	- Africa1.geojson: africa map data file. Used to produce maps in analysis script
	- WHO_Africa3.png: image with WHO countries colored. Used in Rmarkdown at "Section 2" page to give an overview of WHO Africa Region
	- WHO_country_list: table in txt tab delimited format, used in analysis script for mapping WHO countries name and filter out non-WHO countries from data and maps. Has correspondence between various names of the WHO countries (ISO3: for maps, country: names as they are in the data, name_plot: shortened version of names may be useful for plots, etc.)

./output: output of the analysis script, each are dated by the R script hence output from different days will be distinguished an will not overwrite each other.
	- Map_....png: individual maps (there are 6 in total: cumulative cases, cumulative deaths, cumulative cases per 10k pop, cumulative deaths per 10k pop, Dt cases, Dt deaths)
	- 6Maps_WHO_Africa_2020-MM-DD_.png: assembled maps
	- WHO_report_analysis_2020-MM-DD.RData: the R session environment of the analysis script. Since it takes a while to run because of the 1000 iterations of bootstrap (~10-20min), we run the analysis itself once, and then the Rmarkdown script directly load this .RData object and simply produces figure. This allows much quicker editing work on the final document.

./script:
	- WHO_report_analysis.R: runs the actual analysis, that is, load the data, compute doubling times, perform bootstrap to simulate Poisson error and compute 95%CI etc. All data formatting (tables, etc.) required in the final report are also produced there, such that the Rmarkdown document is really only producing the figures.
	- sourced_functions_doublingTime_reports.R: loads all required libraries + defines various pieces of code wrapped in functions, used in the analysis script (see "4) Description of sourced_functions_doublingTime_reports.R" section for more details)
	- WHO_COVID19_SituationReport.Rmd: the Rmarkdown document. Loads the analysis output and produces the pdf/word.
	- ./archived code: bunch of scratch scripts for work in progress


3) PRODUCING THE REPORT

	A) Get the WHO data, save in ./data/2020-MM-DD directory

	B) Open WHO_report_analysis.R:
			make sure the paths to data are correct
			set the variable 'today' so that it matches the data you want to analyse (e.g., if running report on the 10/04 but the data are from the 08/04 you will have to set today<- Syst.date() - 2)
			set the t1 and t2 variables to the dates you want to compute the doubling time over. By default, t2 = today, t1 = t1 - 7, i.e. we compute doubling time over a 7 day period
			set 'its' to 1000 ... or less to run a quick test! (1000 will take about 10-20 min for analysis torun over the 47 countries)
			run the script --> the session environment  will be saved in ./output/WHO_report_analysis_2020-MM-DD.RData

	C) Open WHO_COVID19_SituationReport.Rmd
			adjust title:, subtitle:, author:, {\contentsname}, variables in the header as necessary
			set output: to pdf_document or word_document (or directly select desired outputformat from knit button in RStudio)
			ensure paths are correct (for the loading of the .RData object and the sourcing of the sourced_functions_doublingTime_reports.R script)
			set the 'today' variable to match the date of the data to be analysed --> That shouldbe the same as what you have set in the analysis script
			Knit the document!
			You may want to adjust figures parameters. These are all set inthe first {r} chunk of the Rmd script. See "5) DETAILS ON FIGURES PARAMETERS CONVENTIONS USED" section for more deatils.



4) DESCRIPTION OF "sourced_functions_doublingTime_reports.R"


data.cleaner()
	- DO: applies neighbour points averaging to smooth over data when those have negative numbers of new cases/deaths reported (i.e. decreasing number of cumulative numbers)
	- INPUT: a two columns dataframe where first col is date and second col is number of new reported cases/deaths. The variables MUST BE NAMED date and numNew Cases respectively.
	- OUTPUT: a two columns dataframe (dates and number of new cases/deaths) where there will be less rows than originally, since some dates will have been removed, and replaced by a central date which averages over the two neighbourg dates. The variables remain named "date" and "numNewCases"
	- NOTE: in script, the dataframe is passed through this function until all negative points have been smoothed over. In fact, passing the dataframe through data.cleaner() once is enough, but I had one case, with Camerron data, where we had in a row 0 and -2, therefore the point remained -1 even after applying the smoothing. The fix was to re-pass the output of data.cleaner() through data.cleaner() again. Do this procedure by default now in analysis script. Note, if data do not have any negative point in the first place, data.cleaner() will simply spit out a dataframe exactly as the original one.

compute.td.m1.v2()
	- DO: compute doubling time between two specified dates
	- INPUT: a two columns dataframe where first col is date and second col is cumulative number of cases/deaths. The variables MUST BE NAMED "date" and "cumNumCases" respectively + two dates which can be specified as a string or date format.
	- OUTPUT: A single value, the doubling time, in days, computed over the t2-t1 period. Three behaviour possible: a numeric, a NA or a Inf (see NOTE)
	- NOTE: if cases/deaths have not started yet or started after t1 specified, the Td cannot be computed. Td will be numeric(0), the function spits out an NA. If the number of cases/deaths have started before t1 but have not increased since, this lead to a quotient with zero denominator, hence Td will be "Inf".

sim.epi()
	- DO: simulate new epidemics, using Poisson error structure. For a simulated dataset, a number of new cases each day is drawn from a Poisson distribution, where the mean of that. poisson distribution is the observed number of new cases for that day.
	- INPUT: a two columns dataframe where first col is date and second col is cumulative number of cases/deaths. The variables MUST BE NAMED "date" and "cumNumCases" respectively + its, the number of simulations to do, usually 1000 for a bootstrap 95%CI + plotsim to set to TRUE if want to visualise the simulated data on a plot.
	- OUTPUT: returns the original dataframe with its + 1 columns appended: the first appended columns (called "numNewCases" is be the observed data but in their non-cumulative form. The "V." columns are the simulated data.
	- NOTE: this is for this step that the data.cleaner() is necessary. If there is a negative number of new cases for one day, this causes Error on rpoiss() because the mean of a poisson process cannot be negative. Hence, if there is a negative number in the data series, all the simulations will spit out NAs from that point onward.

Td.lapply()
	- DO: simply applies compute.td.m1.v2() over a list, where each element of the list is a cumulative serie of cases/deaths. Usually, I pass the output of sim.epi(), converted into a list, in that function, to generate a bootstrap distribution of doubling times, from which the 95%CIs are derived.
	- INPUT: a list of cumulative cases/deaths, a vector of corresponding dates, t1 and t2
	- OUTPUT: a list of the same length as in input series list, but here each list item contains a single value, the doubling time for the corresponding serie.
	- NOTE: when the number of cases is low, it may occur that it is possible to compute a doubling time of the observed data, but that certain simulations led, by chance, to epidemic series where Td cannot be computed (e.g. simulated epidemic started at t > t1, or remained falt between t1 and t2), hence there can be NAs or Inf values. At present, I filter out NAs and compute the 95%CI from the smaller bootstrap distribution. However when the series generates "Inf", I do not do that, and leave the "Inf" as CI interval. Hence this is not super consistent. A decision must be made on how to deal with this. It effectively becomes a non-problem as number of cases increase, but in Africa numbers are still low enough that this problem occurs quite a few time still.


epidemic.diff()
	- DO: computes the number of days difference between two epidemic time series based on cumulative cases/deaths
	- INPUT: a dataframe with col 1 being dates and other columns, being cumulative numbers of cases/deaths in different regions/countries + the names of the two series to compare, those names must match names of the input dataframe.
	- OUTPUT: a single number, corresponding to the number of days difference of the 'focal country' relative to 'versus country'. A positive number indicates that the focal country is ahead. A negative number indicates that the focal country is behind.
	- NOTE: therefore, re-running the function but inverting focal and versus country will give the same number but with inverted sign. If the epidemics have not reached comparable numbers at any point in time yet, the function spits out an NA.



























