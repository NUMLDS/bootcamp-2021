# #########################
#
#Name: Naomi Zhang
#Bootcamp 2021
#Final bootcamp exercise
#
##########################





###1 : libraries

library(tidyverse)
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)

###import data
schools <- read.csv("data/nys_schools.csv")
counties <- read.csv("data/nys_acs.csv")





###2 : review data
head(schools)
str(schools)
#12 variables, 35663 rows

head(counties)
str(counties)
#5 variables, 496 rows

#check for blank rows
sum(is.na(schools))
sum(is.na(counties))
#0
#0
#this makes sense, NA are noted to be -99





###3 : Recoding and variable manipulation
#Recoding and variable manipulation
#Deal with missing values, which are currently coded as -99.
#Create a categorical variable that groups counties into "high", "medium", and "low" poverty groups. 
#Decide how you want to split up the groups and briefly explain your decision.
#The tests that the NYS Department of Education administers changes from time to time, 
#so scale scores are not directly comparable year-to-year. 
#Create a new variable that is the standardized z-score for math and English Language Arts (ELA) for each year 
#(hint: group by year and use the scale() function)

summary(schools)
summary(counties)
#counties is good
#only issue is schools

filter(schools, total_enroll == -99 | per_free_lunch == -99 | per_lep == -99 | mean_ela_score == -99 | mean_math_score == -99)
#2136 records

#decide to drop rows with -99's, no mercy
schools_cleaned <- schools[schools$district_name != -99 &
                              schools$county_name != -99 &
                              schools$total_enroll != -99 & 
                              schools$per_free_lunch != -99 &
                              schools$per_lep != -99 &
                              schools$mean_ela_score != -99 &
                              schools$mean_math_score != -99
                              ,]

#double check
summary(schools_cleaned)
#it clean!

#categorical column now
#doing it by household income
summary(counties)
#1st quart 46347
#3rd quart 56448

low_thresh <- 46347
high_thresh <- 56448

#add column to counties
mutate(counties, poverty_group=NA)

for (i in 1:nrow(counties)) {
    if (counties[i,]$median_household_income < low_thresh ) {
        counties[i,"poverty_group"] = "low"
    }
    else if (counties[i,]$median_household_income >= low_thresh & counties[i,]$median_household_income < high_thresh ) {
        counties[i,"poverty_group"] = "medium"
    }
    else if (counties[i,]$median_household_income >= high_thresh ) {
        counties[i,"poverty_group"] = "high"
    }
    
}

#double check
summary(counties)
head(counties)
#looks good!


#scale now
#take a backup first
schools_cleaned_backup_01 <- schools_cleaned

#add new columns
schools_cleaned <- schools_cleaned %>%
    group_by(year) %>%
    mutate(mean_ela_score_scaled = scale(mean_ela_score, scale=TRUE)) %>%
    mutate(mean_math_score_scaled = scale(mean_math_score, scale=TRUE))





###4 : Merge datasets

merged_schools_countries <- merge(schools_cleaned, counties, by = c("year","county_name"))

head(merged_schools_countries)
summary(merged_schools_countries)
#looks good





###5 : Create summary tables

head(merged_schools_countries)

#take backup first
merged_schools_countries_backup <- merged_schools_countries

#For each county: total enrollment, percent of students qualifying for free 
#or reduced price lunch, and percent of population in poverty.
summary_merged_schools_countries <- merged_schools_countries %>%
    mutate(sum_lunch = per_reduced_lunch + per_free_lunch) %>%
    group_by(county_name) %>%
    summarize(total_enroll_county = sum(total_enroll), 
              county_per_poverty = sum(total_enroll * county_per_poverty) / sum(total_enroll), #we must recalculate the percentage per total enrollment
              sum_lunch = sum(total_enroll * sum_lunch) / sum(total_enroll))
    
#For the counties with the top 5 and bottom 5 poverty rate: percent of population in poverty, 
#percent of students qualifying for free or reduced price lunch, mean reading score, and mean math score.
    
top_bottom_five <- merged_schools_countries %>%
    mutate(sum_lunch = per_reduced_lunch + per_free_lunch) %>%
    group_by(county_name) %>%
    summarize(county_per_poverty = sum(total_enroll * county_per_poverty) / sum(total_enroll), 
              sum_lunch = sum(total_enroll * sum_lunch) / sum(total_enroll), 
              avg_mean_ela = mean(mean_ela_score_scaled), 
              avg_mean_math = mean(mean_math_score_scaled)) %>%
    arrange(county_per_poverty)

summary(top_bottom_five)
#there are 62 rows

#now take the top 5 and bottom 5
top_bottom_five[c(1:5,58:62), ]





###6 : Data visualization
#The relationship between access to free/reduced price lunch and test performance, at the school level.

#add sum_lunch as a permanent column
merged_schools_countries <- merged_schools_countries %>%
    mutate(sum_lunch = per_reduced_lunch + per_free_lunch)

merged_schools_countries %>%
    ggplot() + 
    geom_point(aes(x=sum_lunch, y=mean_ela_score_scaled)) +
    labs(title = "Relationship between Free/Reduced Lunch vs ELA Test Performance",
        x = "% Free or Reduced Lunch", 
        y = "Scaled ELA Score")

merged_schools_countries %>%
    ggplot() + 
    geom_point(aes(x=sum_lunch, y=mean_math_score_scaled)) +
    labs(title = "Relationship between Free/Reduced Lunch vs Math Test Performance",
         x = "% Free or Reduced Lunch", 
         y = "Scaled Math Score")
#general downward trend for both test scores


#Average test performance across counties with high, low, and medium poverty.

merged_schools_countries %>%
    group_by(county_name, poverty_group) %>%
    summarize(avg_ela = mean(mean_ela_score_scaled), avg_math = mean(mean_math_score_scaled)) %>%
    ggplot() +
    geom_boxplot(aes(x= factor(poverty_group, levels=c("low","medium","high")), y=avg_ela)) +
    labs(title = "ELA Test Performance Across Poverty Levels",
         x = "Poverty Level", 
         y = "Scaled ELA Score")

merged_schools_countries %>%
    group_by(county_name, poverty_group) %>%
    summarize(avg_ela = mean(mean_ela_score_scaled), avg_math = mean(mean_math_score_scaled)) %>%
    ggplot() +
    geom_boxplot(aes(x= factor(poverty_group, levels=c("low","medium","high")), y=avg_math)) +
    labs(title = "Math Test Performance Across Poverty Levels",
         x = "Poverty Level", 
         y = "Scaled Math Score")

#general upward trend with higher test scores for richer counties





###7 : Answering questions

#What can the data tell us about the relationship between poverty and test performance in New York public schools? 
#Has this relationship changed over time? Is this relationship at all moderated by access to free/reduced price lunch?

The poverty level was previously determined using the household income. 
From Step 6 above, we can conclude that there is a general trend where higher test scores correlated with lower poverty (i.e. a negative correlation).
This is especially true for Math. However, for ELA tests, lower and medium poverty level does not appear to have a significant difference.

