# Required libraries -- haven is for importing data from Stata

library(ggplot2)
library(haven)

# Load data saved in the same folder as this script
stata_dataset <- read_dta('stata_dataset.dta')

# change the value of x= and colour= to variables from the dataset. x is the variable that is counted in the histogram, 
# and colour allows you to do histogram overlays

