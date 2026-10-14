# Paired and Independent Samples T-tests --------------------------------

# Student Learning Objectives (SLOs)
# 1. Learn how to compute a paired and independent samples t-test
# 2. Learn how to check assumptions for paired and independent samples t-tests
# 3. Learn how to compute effect sizes for paired and independent samples t-tests
# 4. Learn how to find a priori and post hoc power for paired and independent samples t-tests 

## packages ----------------------------------------------------
library(haven) # read .sav files
library(gplots) # graphing 
library(ggplot2) # graphing
library(psych) # descriptive statistics 
library(rstatix) # t-tests and effect sizes 
library(pwr) # power analyses 
library(tidyverse) # data cleaning and manipulation

## Load in data ------------------------------------------

### Nursing home data ----------------------------------------

# data: Happiness scores of older adults living in nursing homes 
# scores are provided before and after dogs were introduced into the nursing home 
# happiness scored on a scale of 1-10 with 10 being the highest happiness 

# happiness scores before dogs introduced 
beforedog <- c(2,3,4,1,2,5,4,1)

# happiness scores after dogs introduced 
afterdog <- c(7,8,10,9,8,6,5,10)

# combine objects into dataframe 
happiness_wide <- as.data.frame(cbind(beforedog, afterdog))

# convert dataframe to long format 
happiness <- happiness_wide |> # build new dataframe from happiness_wide df 
  pivot_longer(cols = c('beforedog', 'afterdog'), # use information from beforedog and afterdog columns 
               names_to = 'group', # move column names to new variable called 'group' 
               values_to = 'happiness') # move scores to new variable called 'happiness' 

### Workplace data ------------------------------------------------------

# data: participants read about one of four companies that varied in racial representation and employee culture
# Participants reported positivity toward that workplace 

# racial representation (rep) coded as 0 = low diversity, 1 = high diversity

# read in data using Haven packege 'read_sav' 
workplace <- read_sav('Quant_I/Data/PSY555_WorkplaceData.sav')

## Paired Samples t-test -----------------------------------------
