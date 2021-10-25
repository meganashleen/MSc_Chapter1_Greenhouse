setwd("/Users/meganjordan/Documents")                    

# Libraries ---------------------------------------------------------------
# shortcut ctrl + shft + R will make this ^^^ for you and then you can jump to the section or minimize it


library(tidyverse) # I use tidy (ggplot, dplyr) for coding
library(vegan) # I tend to always install this and it has helpful code for generating species richness etc. but you already did it

library(performance) # does a nice check of all your model assumptions
library(see) # need this for performance to work

library(car) # set your sums of squares error for ANOVA
library(agricolae) #good Tukey's post hoc test here (HSD.test)


# Load data ---------------------------------------------------------------

data <- read.csv("GreenhouseExperiment_QAQCd_3.0.csv") #changing your '=' to '<-' because this is the preferred style

data

dim(data)
str(data) # look at the data type
# question - do you want to remove "unknown" columns for your analysis? 


GreenData <- data %>% drop_na() # took out the rows with "NA" for 'treatment' (this is tidy)
dim(GreenData)
str(GreenData)

# Visualize the data ------------------------------------------------------

library(parameters) # has a standard error function bc i am lazy & dum
#https://www.datanovia.com/en/blog/dplyr-how-to-compute-summary-statistics-across-multiple-columns/

summary <- GreenData %>% 
  select(Treatment, Abundance, Richness) %>%  # I only want summaries for these two data columns
  group_by(Treatment) %>% # group the means etc. by the treatments
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, SER = standard_error), na.rm = TRUE,
    .names = "{col}_{fn}"
  )) 


#Treatment   Abundance_Mean Abundance_SD Abundance_SER Richness_Mean Richness_SD Richness_SER
#1 Control             25.1        19.9           3.15          2.52        1.11        0.175
#2 Reference           12.8         8.19          1.29          2.42        1.15        0.182
#3 Treated             37.3        34.5           5.45          2.68        1.51        0.239


# bit skewed but none of this is unexpected for this kind of data

hist(GreenData$Abundance, xlab ="Number of seedlings", cex.lab= 1.2, main=NULL)
hist(GreenData$Richness, xlab = "Number of species", cex.lab= 1.2, main=NULL)
hist(GreenData$PHRAUST, xlab="Number of Phragmites seedlings", cex.lab= 1.2, main=NULL)

boxplot(GreenData$Abundance, ylab = "Number of seedlings")
boxplot(GreenData$Richness, ylab = "Number of species")

#Total number of seedlings and number of species need to be transformed to make them Normal

GreenData$logAbundance <- log(GreenData$Abundance) 
hist(GreenData$logAbundance)

GreenData$logRichness <- log(GreenData$Richness)
hist(GreenData$logRichness)


# GLMs --------------------------------------------------------------------

#### two-way ANOVA for seed abundance 

abund.aov <- lm(logAbundance ~ Treatment * Condition, data = GreenData) # set the data so don't have to specify, and (*) alone will also include each factor 
summary(abund.aov)

Anova(abund.aov, type = 3) # Anova is from 'car' and 'type = 3' is setting the SS to type 3 bc we are looking for an interaction. If no interaction can report type 2 SS

# I like to copy the results into my script so I don't have to rerun it every time, it is just here

#Response: logAbundance
#                      Sum Sq  Df  F value  Pr(>F)    
#(Intercept)         162.593   1 218.9114 < 2e-16 ***
#Treatment             5.154   2   3.4699 0.03445 *  
#Condition             0.620   1   0.8347 0.36285    
#Treatment:Condition   1.251   2   0.8419 0.43355    
#Residuals            84.671 114 

# check out that difference in 'treatment'

abun.trt <- HSD.test(abund.aov, "Treatment") # this will give you all the info but I like to just pull out the 'groups'

#logAbundance groups
#Treated       3.255391      a
#Control       2.975743      a
#Reference     2.414035      b

check_model(abund.aov) # honestly looks good. GLMs are robust
check_heteroscedasticity(abund.aov) # p = 0.08 so you good
check_normality(abund.aov) # residuals are non-normal (discuss with RR how much you care) 

# do a little plot with the significant factor (Treatment)


ggplot(GreenData, aes(x = Treatment, y = Abundance)) + # don;t plot your log transformed data, but do mention that in the methods
  geom_boxplot() +
  theme_classic() +
  annotate("text", x = c(1,2,3), y = 125, label = c("a","b","a")) # adding in your post-hoc results




