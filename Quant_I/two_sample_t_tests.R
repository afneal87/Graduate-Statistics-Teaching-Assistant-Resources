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

# convert group variable to a factor 
happiness$group <- as.factor(happiness$group)

### Checking assumptions --------------------------------------------

# QQ plot to check normality
happiness |> # dataframe to use for graph 
  ggplot(aes(sample = happiness)) + # variable to plot in the QQ plot 
  stat_qq() + # add QQ plot layer
  stat_qq_line() # add QQ plot reference line 
# data points fall along the line, suggesting the sample is normal 

# Shapiro Wilks test of normality
shapiro_test(happiness, happiness)
# statistic = 0.935, p = .279
# non significant p-value indicates data are normal 

### Running paired samples t-test --------------------------------

paired_t <- t_test(data = happiness, # data to use 
                   happiness ~ group, # t-test formula, compare happiness by group
                   paired = TRUE, # paired samples test 
                   detailed = TRUE) # include CI in output 
paired_t
# t(7) = 5.00, p = .002
# t-test suggests significant increase in happiness after dogs 

### Calculating effect size Cohen's d 

cohens_d(data = happiness,
         happiness ~ group,
         paired = TRUE)
# d = 1.77 

## Independent Samples t-test ---------------------------------------

# convert rep variable to factor 
workplace$rep <- factor(workplace$rep, labels = c('low', 'high'))

# remove observations with missing values 
workplace <- workplace |> 
  na.omit()

### Check assumptions --------------------------------------

# check normality with QQ plot 
workplace |> 
  ggplot(aes(sample = WPAVG)) +
  stat_qq() +
  stat_qq_line()
# QQ plot suggests data are normal 

# Run Levene's test for homogeneity of variance (HOV) 
levene_test(workplace, WPAVG ~ rep)
# statistic = 0.77, p = .782
# non significant test statistic indicates data show HOV 

### Run the t-test -----------------------------------------------

independent_t <- t_test(data = workplace, # data to use 
                        WPAVG ~ rep, # t-test formula, compare WPAVG by representation 
                        paired = FALSE, # not a paired sample 
                        var.equal = TRUE, # HOV confirmed 
                        detailed = TRUE) # include CI in output 
independent_t
# t(641) = -2.50, p = .013
# significant t-test indicates significant differences in warmth toward company based on racial diversity

### Plot the means of the samples with 95% confidence intervals 

plotmeans(WPAVG ~ rep, # t-test formula 
          data = workplace, # data to use for plot 
          bars = TRUE, # include CI bars in the plot 
          p = 0.95, # confidence interval of 95%
          ci.label = FALSE, # optional CI value annotations
          connect = FALSE, # optional line connecting sample means 
          n.label = FALSE) # optional sample size annotation

### Calculating effect size -----------------------------------------------

cohens_d(data = workplace, 
         WPAVG ~ rep,
         var.equal = TRUE)
# d = -0.197

## Power calculations --------------------------------------------

# paired samples post hoc test 
pwr.t.test(n = 8, # number of observations in each group
           d = 1.77, # effect size 
           sig.level = 0.05, # alpha level 
           type = 'paired') # type of test 
# post hoc power = 0.99 

# independent samples post hoc 
pwr.t.test(n = 323, # number of observations in each group
           d = 0.19, # effect size 
           sig.level = 0.05, # significance level 
           type = 'two.sample') # type of test 
# post hoc power = 0.67