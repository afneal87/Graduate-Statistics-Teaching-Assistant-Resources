# Z-tests and t-tests in R ----------------------------------

# Student Learning Objectives (SLOs)
# 1. Learn how to standardize data (i.e. calculate z-scores)
# 2. Learn how to conduct a one-sample z-test 
# 3. Learn how to conduct a one-sample t-test 

# Data: memory span test scores of heavy marijuana smokers 
# population average: 7.5
# population standard deviation: 3.2 

## packages --------------------------------------------
library(BSDA) # one sample Z-test
library(rstatix) # one sample t-test 
library(tidyverse) # data cleaning and manipulation

## Import Data -------------------------------------------

# create new object called 'scores'
scores <- c(4,5,6,6,7,7,6,6,3,7,8,7,5,10,9,6,5,7,6,8)

# convert object to a dataframe 
scores <- as.data.frame(scores)

## One Sample Z-test ----------------------------------------------

# create new column of standardized scores 
scores <- scores |> 
  mutate(z_score = as.vector(scale(scores, 
                                   center = TRUE, # subtrack column mean from column values
                                   scale = TRUE))) # divide centered values by standard deviation

# calculate percentage of values below a given value 
pnorm(scores$z_score)
# outputs percentage below each observation 

# calculate percentage above a given value 
1 - pnorm(scores$z_score)
# outputs percentage above each observation 

# save percentage calculations as new variables in the dataframe 
scores <- scores |> 
  mutate(lower_percent = as.vector(pnorm(z_score)),
         upper_percent = as.vector(1 - pnorm(z_score)))

# run one sample z-test 
z.test(scores$scores, # data 
       mu = 7.5, # population mean 
       sigma.x = 3.2, # population standard deviation
       alternative = 'two.sided', # two tail test 
       conf.level = .95) # confidence interval of 95% 
# Z-score = -1.54, p = .124
# no significant differences in marijuana users compared to not heavy smokers 

## One Sample t-test -----------------------------------------------

# conduct t-test 
t_test(data = scores, # dataframe to use 
       scores ~ 1, # forumal for t-test, one sample uses "~1" 
       mu = 7.5) # population mean 
# t(19) = -3.01, p = .007
# significant decrease in memory span scores compared to not heavy smokers 