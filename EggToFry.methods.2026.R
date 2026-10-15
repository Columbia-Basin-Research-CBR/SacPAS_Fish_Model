# Header ====
# Example R code for Egg-to-Fry Fish Model runs
# Survivals are generated for user's parameters and data using the methods proscribed.
# Updated Aug 2026 for version 4.0 of FishModel: Egg-To-Fry

# W. Nicholas Beer CBR/SAFS/UW nickbeer@uw.edu

# parameter values for the development and survival formulations are the defaults shown on the FishModel page:
# https://cbr.wahsington.edu/sacramento/fishmodel/

# Introduction ====
# Familiarity with the GUI fishmodel page is very helpful.
# Familiarity with R is versy useful.

# THREE "modes" are possible with Egg-to-Fry modeling: historical, forecasts, and scenario
    # Historical :: hindcasts use token=value pairs to retrieve data from the CBR database
    # Scenarios  :: effectively unlimited with user-provided temperatures, flows, & redd distributions
    # Forecasts  :: limited to temperature projections through end of November from CVTEMP when available
    #            :: 

# Examples of historical runs. The methods are also the basis of forecasting and scenario runs. ====

#==== 1. R tidyverse library is used here ====
# install.packages("tidyverse")
library(tidyverse)

#==== 2. Name a workspace directory ====
# a string of numerals and characters
myDIR <- "MYDIR1234"  # edit this value

# use paste0() to assemble parts of a query string
# It is recommended to put the directory ID in the first position
base <- paste0("https://cbr.washington.edu/sac-bin/fishmodel/getandplottemp.pl?dirUseId=",myDIR)
# subsequent token=value pairs 
# can be in any order beginning with "&"

#==== 3. Querystrings ====
# Querystring request generation:
  print(paste0(base,"&generatequery=1")) 
  # Paste the result of this statement into browser (without quotation marks)
  # Any resulting query string will use default values of all the inputs

# Querystring via UI method:
  # Open Egg-to-Fry model in a browser window
  # Choose intermediate or advanced
  # Modify (or use default) inputs
  # Press "generate query strings" 
  # Press "Run"

# the two methods above will display all of the "raw" codes used to obtain specific model run results
# this example will continue with "raw=13" which collects several key outputs

#==== 4. Input controls in query strings ====
# For example: redd distribution year, run-survey type, historical temperature year, ...)
# this example continues with various historical years using the winter carcass-survey-based redd distribution
# token=value pair examples: "&reddyear=2022" "&tempsource=dbtemp&tempyear=2022" "&redds=dbcarcass" 
# NOTE: &generatequery=1 is removed in order to obtain results directly
# Example query
# Build up the query string 
base1 <- paste0(base,"&tempsource=dbtemp&redds=dbcarcass&raw=13")

# add controls as needed  and continue to make the string more compact:
string1 <- paste0(base1,"&reddyear=2022&tempyear=2022")

# Paste this string into a browser
print(string1)

#=== 5. capture results in R ====
# MOST results are multi-line, .csv text 
result <- read_csv(string1,show_col_types = FALSE)
print(result)
# Results with single value E.g. raw=1, grand survival are captured with scan. 
print(scan(paste0(base,"&reddyear=2022&tempsource=dbtemp&tempyear=2022&redds=dbcarcass&raw=1")))
print(as.numeric(result$Value[grepl("Incubation Survival",result$What)]))
print(as.numeric(result[11,2]))

#==== 6. Automate comparison ====
# Compare the hatching and incubation mortality model during the "critical years"
# ALSO use only during the "critical water years"
# relevant new token=value pairs: &mortality=hatchmort (default, thus explicit use is same as omiting) or &mortality=emergemort

 # initialize
start <- Sys.time()
hatchmortresults <- emergemortresults <- NULL
years <- c(2008,2014,2015,2021,2022)
# base1 has a lot of what we need already
for(mort in c("hatchmort","emergemort")){
	out <- NULL
	for(i in 1:length(years)){
		string1 <- paste0(base1,"&mortality=",mort,"&reddyear=",years[i],"&tempyear=",years[i])
		# print(string1)
		result <- read_csv(string1,show_col_types=FALSE)
		names(result) <- c("What",years[i])
		# glue together
		if(i==1) { out <- result } else { out <- cbind.data.frame(out,result[,2]) }
	}
	if(mort== "hatchmort"){hatchmortresults <- out 
	}else{emergemortresults <- out}
}

end <- Sys.time()
print(end-start) 

#==== 7. Compile and present results ====
report <- hatchmortresults %>% filter(What=="TDM mortality") %>% pivot_longer(cols=!1,names_to="year",values_to="TDM")
report2 <- emergemortresults %>% filter(What=="TDM mortality") %>% pivot_longer(cols=!1,names_to="year",values_to="TDM")
print("Stage-Dependent TDM Mortality")
print(report)
print("All-incubation TDM Mortality")
print(report2)

#==== 9. Forecast example
# At time of writing in September 1, 2026, there is NO forecast for 2026 season 
# so future is filled-in with historical mean.
# NOTE:  &temponly=on is appropriate for ALL redd combination(s) 
# Density effects are calibrated with winter-run carcass survey data, historical year, only.
# These are equivalent:
test <- read_csv(paste0(base,"&temponly=on&redds=dbAll&reddyear=2026&tempyear=2026&raw=13"),show_col_types = FALSE)
print(test)
test <- read_csv(paste0(base,"&temponly=on&redds=dbAll&reddyear=2026&tempsource=latest&raw=13"),show_col_types = FALSE)
print(test)

#==== 10. Scenario runs with user files ====
# Requires pre-loading user files and then applying similar token value pairs that reference the files as data sources
# Your formatted files of temperature and/or redd distribution scenarios will be placed in local directory
# querystring methods allow you to access the contents of any file and recover an inventory 
# Contact CBR staff at web@cbr.washington.edu for file handling
# The mortality and development equations can be configured with &token=value pairs for the coefficients.

 
#==== 11. Some comparisons in greater depth ====

# initialize
start <- Sys.time()
hatchmortresults <- emergemortresults <- NULL
years <- c("C","D","BN","AN","W")
for(mort in c("hatchmort","emergemort")){
  out <- NULL
  for(i in 1:length(years)){
    string1 <- paste0(base1,"&mortality=",mort,"&reddyear=",paste0(years[i],),"&tempyear=",paste0(years[i],".year") )
    # print(string1)
    result <- read_csv(string1,show_col_types=FALSE)
    names(result) <- c("What",years[i])
    # glue together
    if(i==1) { out <- result } else { out <- cbind.data.frame(out,result[,2]) }
  }
  if(mort== "hatchmort"){hatchmortresults <- out 
  }else{emergemortresults <- out}
}

end <- Sys.time()
print(end-start) 

