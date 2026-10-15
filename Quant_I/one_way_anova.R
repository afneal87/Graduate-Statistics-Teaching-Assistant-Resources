# One-Way ANOVA and Follow-up Tests -------------------------------------

# Student Learning Objectives (SLOs)
# 1. Learn how to compute a one-way ANOVA
# 2. Learn how to run planned and unplanned comparisons 
# 3. Learn how to find a priori and post hoc power for one-way ANOVA
# 4. Review assumption checks of normality and HOV 

## Packages -----------------------------------------------------
library(ez) # ANOVA test
library(emmeans) # follow-up tests for ANOVA
library(sjstats) # calculate ANOVA effect sizes
library(car) # ANOVA and HOV assumptions 
library(haven) # read .sav files 
library(tidyverse) # data manipulation and cleaning
library(psych) # descriptive statistics 
library(pwr) # power analyses 

## Read in data file ----------------------------------------------

# Data: Data from a public repository of game-play as an intervention for trauma treatment 
# Condition: one of four experimental conditions - Control, reactivation and tetris, tetris only, and reactivation only 
# Outcome: number of intrusive memory over seven days post intervention 

tetris <- read_sav('Quant_I/Data/James_Holmes_Expt 2_DATA.sav')

# convert condition to a factor 
tetris$Condition <- factor(tetris$Condition, 
                           levels = c(1:4),
                           labels = c('Control', 'Reactivation+tetris', 'Tetris_only', 'Reactivation_only'))

# rename outcome variable and add participant id to the dataframe 
tetris <- tetris |> 
  mutate(avg_intr = Days_One_to_Seven_Image_Based_Intrusions_in_Intrusion_Diary, # create new variable of outcome variable with shorter name 
         p_id = row_number()) # add variable to label each observation as its row number 

## Exploratory Data Analysis --------------------------------------------------

describeBy(tetris$avg_intr, tetris$Condition)
# highest mean in control group, lowest mean in reactivation+tetris group 

## Conduct one-way ANOVA test --------------------------------------------

tetris_ANOVA <- ezANOVA(data = tetris, # data for the model
                        wid = p_id, # unique identifier for observations
                        dv = avg_intr, # dependent variable 
                        between = Condition, # between-groups identifier 
                        type = 3, # sums of squares type 
                        return_aov = TRUE) # store ANOVA results for later use 
tetris_ANOVA
# F(3, 68) = 3.79, p = .014 
# significant F-statistic supports group differences across conditions 
# output includes Levene's test, shows no HOV violation 
# significant ANOVA requires follow-up testing for group differences 

## Calculating Omega-squared effect size for ANOVA -----------------------------

anova_stats(tetris_ANOVA$aov)
# ouptut gives different effect sizes along with ANOVA statistics 

## Follow-up Testing --------------------------------------------------

### Linear contrasts --------------------------------------------------

# Step 1: Calculate estimated marginal means based on model 
emm1 <- emmeans(tetris_ANOVA$aov, ~ Condition)
emm1
# output shows estimated marginal means for each condition with confidence intervals 

# Step 2 & 3 specify appropriate contrast coefficients and conduct analysis 

# specify coefficient values for contrasts 
coeff_set1 <- list(cvst = c(-1, 0, 1, 0), # control vs tetris only 
                   cvscombo = c(-1, 1, 0, 0), # control vs combo reactivation+tetris 
                   tvscombo = c(0, -1, 1, 0)) # tetris only vs combo reactivation+tetris 

c1 <- contrast(emm1, # estimated marginal means from the ANOVA model
               coeff_set1, # coefficients for contrasts 
               adjust = 'none') # adjustments to make 

summary(c1, infer = TRUE) # output of linear contrasts 
# control vs tetris not significant 
# control vs combo significant 
# tetris vs combo not significant 

# calculate Cohen's D for each contrast 
eff_size(c1, # contrasts 
         sigma = 3.175812, # residual standard error from ANOVA model
         edf = 68) # degrees of freedom within from ANOVA 

### Complex contrasts ---------------------------------------------------

# H1: tetris only and reactivation only will be higher than control condition
# H2: all experimental conditions will be higher than control condition 

# specify coefficients 
complexset <- list(h1 = c(-1, 0, .5, .5), # tetris only and reactivation only compared to control 
                   h2 = c(-1, .33, .33, .33)) # control compared to all experimental conditions 

c2 <- contrast(emm1, # estimated marginal means from ANOVA model
               complexset, # contrast coefficients 
               adjust = 'none') # statistical adjustments 

summary(c2, infer = TRUE) # output of complex contrasts 
# no significant differences in contrasts 

# Effect sizes for contrasts 
eff_size(c2, # contrasts object 
         sigma = 3.175812, # residual standard error from ANOVA model 
         edf = 68, # degrees of freedom within from ANOVA model 
         method = 'identity') # contrast method 

### All pairwise comparisons -------------------------------------------------

comparisons <- emmeans(tetris_ANOVA$aov, # ANOVA object to use 
                       pairwise ~ Condition, # factor to compare groups 
                       adjust = 'none') # statistical adjustments 

summary(comparisons, infer = TRUE) # output of pairwise comparisons 
# output gives estimated marginal means for each group before all pairwise comparisons 
# control vs combo significant 
# combo vs reactivation significant 
# all other pairs not significant 

# Cohen's D for pairwise comparisons 
eff_size(comparisons, # contrasts object 
         sigma = 3.175812, # residual standard error from ANOVA model
         edf = 68, # degrees of freedom within from ANOVA model
         method = 'pairwise') # method of comparisons 

## Plotting the Data ---------------------------------------------------------

# create descriptive dataframe 
descriptive_df <- tetris |> # create new dataframe using tetris dataframe 
  group_by(Condition) |> # group new dataframe by condition
  summarise(means = mean(avg_intr), # create variable of average intrusive thoughts by condition
            SE = sd(avg_intr)/sqrt(length(avg_intr))) # create variable of standard error of intrusive thoughts 

# Create a bar graph with means and standard error bars of intrusive thoughts by condition

# start with the descriptive dataframe 
descriptive_df |> 
  # create ggplot object 
  ggplot(aes(x = Condition, y = means)) +
  # add bargraph of average intrusive thoughts by condition
  geom_bar(stat = 'identity', aes(fill = Condition)) +
  # create error bars by adding and subtracting the SE for each condition from the mean
  geom_errorbar(aes(ymin = means - SE, ymax = means + SE), width = .1) +
  # add individual data points over the bars in the graph
  geom_jitter(data = tetris, aes(x = Condition, y = avg_intr), alpha = .5) +
  # change the y-axis lable of the graph
  labs(y = 'Intrusive Memories (Mean for Week)') +
  # remove the grey background and grid lines from the graph 
  theme_classic() +
  # remove the legend and change the font 
  theme(legend.position = 'none',
        text = element_text(family = 'serif'))

# create a plot using the ezPlot function from the ez package 

ezPlot(tetris, # data 
       dv = avg_intr, # dependent variable 
       wid = p_id, # unique identifier for observations 
       between = Condition, # group variable 
       x = .(Condition), # x-axis variable 
       type = 3, # sums of squares type 
       do_bars = TRUE, # include error bars in the plot 
       y_lab = 'Average Number of Intrusive Thoughts in Seven Days') + # change y-axis label
  labs(caption = "Note: error bars calculated from Fisher's LSD") + # add caption 
  theme_classic() + # change the background color and remove gridlines 
  theme(text = element_text(family = 'serif')) # change font 

## Power Analyses for One-way ANOVA ------------------------------------------------

# a priori power analysis 
pwr.anova.test(k = 4, # number of groups 
               power = .80, # desired power 
               f = .10, # estimated effect size 
               sig.level = 0.05) # alpha level 
# number of observations per group is 274
# 274 per group multiplied by 4 groups is total sample size of 1096 

## Assumption checks ---------------------------------------------------------------

# check normality with QQ plot 
tetris |> # data 
  ggplot(aes(sample = avg_intr)) + # identify the variable of interest 
  stat_qq() + # create QQ plot layer 
  stat_qq_line() + # Add QQ reference line 
  facet_wrap(~ Condition) # create individual plot for each condition 
# plots show no extreme deviations from normality 

# check normality with Shapiro Wilks test 
shapiro.test(tetris$avg_intr)
# significant p-value indicates violation of normality assumption 
# QQ plot looks okay, so proceeding with test is okay 

# check HOV with levene's test
leveneTest(avg_intr ~ Condition, data = tetris)
# output shows non significant p-value which means HOV not violated 
