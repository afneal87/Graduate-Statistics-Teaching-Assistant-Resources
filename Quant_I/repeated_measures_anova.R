# Repeated Measures ANOVA --------------------------------------------------

# Student Learning Objectives (SLOs)
# 1. Learn how to compute one-way repeated measures ANOVA
# 2. Learn how to compute follow-up procedures/pairwise comparisons for a repeated measures ANOVA

## Packages ----------------------------------------------------------------
library(ggplot2) # graphing
library(psych) # descriptive statistics 
library(haven) # read .sav files 
library(ez) # run ANOVA models 
library(rstatix) # follow-up tests of ANOVA 
library(sjstats) # follow-up tests of ANOVA 

## Load in Data ----------------------------------------------------------

# Data: Foa et al. (1991)
# 45 rape victims randomly assigned a therapy type and reported PTSD symptoms 
# Therapy types: Stress inoculation therapy (SIT), prolonged exposure (PE), supportive counseling (SC), or waiting list (WL)

longfoadata <- read_sav('Quant_I/Data/longfoadata.sav')

## Testing ANOVA assumptions -----------------------------------------

# Check distribution with boxplots 

