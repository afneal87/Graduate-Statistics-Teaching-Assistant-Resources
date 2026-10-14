# Confidence Intervals, Effect Size, & Power of t-tests ------------------------

# Student Learning Objectives (SLOs)
# 1. Calculate and interpret confidence intervals for a one-sample t-test
# 2. Calculate and interpret Cohen's d effect size for a one-sample t-test
# 3. Walk through a template of creating a fun figure for a one-sample t-test
# 4. Conduct post-hoc and a priori power analyses for a one-sample t-test

# Data: memory span task scores for sample of participants who report being heavy marijunana smokers
# population mean of memory span task: 7.5 

## packages --------------------------------------
library(rstatix) # one sample t-test
library(ggpubr) # graphing 
library(Rmisc) # confidence intervals 
library(pwr) # power anlayses 

## Create dataframe -----------------------------------------------

scores <- c(4,5,6,6,7,7,6,6,3,7,8,7,5,10,9,6,5,7,6,8) # save data to object 
scores_data <- as.data.frame(scores) # convert vector to dataframe 

## Calculate confidence intervals -----------------------------------------

# calculate a 90% confidence interval of the mean 
CI(scores_data$scores, ci = .90) # 90% confidence interval
# mean = 6.40
# 90%CI[5.77, 7.03]
# confidence interval does not inlcude population mean 

# run a one-sample t-test and get confidence intervals 
scores_test <- t_test(data = scores_data, # data 
                      scores ~ 1, # t-test formula, one-sample uses ~1 
                      mu = 7.5, # population mean 
                      conf.level = .90, # confidence interval 
                      detail = TRUE) # include CI in output 
scores_test
# t(19) = -3.01, p = .007, 90%CI[5.77, 7.03] 
# smokers sample significanly lower scores on memory span test than nonsmokers 

## Calculating Effect Size Cohen's D ----------------------------------------

cohens_d(scores_data, # data 
         scores ~ 1, # t-test formula 
         mu = 7.5) # population mean
# d = -0.673 
# negative effect size indicates sample mean in less than population mean 

## Density plot to visualize one-sample t-test ----------------------------

# create dentisty plot object 
ggdensity(scores_data, x = 'scores', fill = 'lightgray') +
  # change the limits of the x-axis 
  scale_x_continuous(limits = c(0,15)) +
  # add a line for the sample mean of the data 
  stat_central_tendency(type = 'mean', color = 'red', linetype = 'dashed') +
  # add a line for the population mean of the memory span test 
  geom_vline(xintercept = 7.5, color = 'blue', linetype = 'dashed') +
  # add a subtitle with the test statistics of the t-test 
  labs(subtitle = get_test_label(scores_test, detailed = TRUE))

## Calculating Power -------------------------------------------------------

### Post-hoc power analysis -------------------------------------

# calculate achieved power using information from data 
pwr.t.test(n = 20, # sample size 
           d = -0.673, # cohen's d effect size 
           sig.level = 0.1, # alpha cutoff based on CI 
           type = 'one.sample', # type of t-test 
           alternative = 'two.sided') # alternative hypothesis
# achieved power is 0.90

### A priori power analysis -----------------------------------

# calculate required sample size to achieve 80% power 
pwr.t.test(power = 0.8, # desired power level 
           d = 0.25, # estimated effect size (small)
           sig.level = 0.1, # alpha level 
           type = 'one.sample', # type of t-test
           alternative = 'two.sided') # alternative hypothesis 
# required sample size is 101 people 