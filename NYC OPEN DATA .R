setwd("~/Desktop/Stats 2")
getwd()

library(tidyverse)
library(janitor)
library(lubridate)
library(dplyr)

## can huge chains have multiple violations? 

set.seed(20181209)
# You can use this url to download the data directly into R (will take a few seconds)
restaurant_raw <- read_csv("https://data.cityofnewyork.us/api/views/43nn-pn8j/rows.csv")

# Cleaning names with janitor, changed the format of date, and separated inspection type 
restaurant_inspections <- restaurant_raw %>% 
  janitor::clean_names() %>%
  select(-phone, -grade_date, -record_date, -building, -street) %>% 
  mutate(inspection_date = mdy(inspection_date)) %>%
  separate(inspection_type, c("inspection_program", "inspection_type", sep = "  /  "))

### camis = unique identifier - static per restaurant permit 
### dba = name of rest. 
##How many do inspections do we get per restaurant? 
restaurant_inspections %>% 
  count(dba, camis, sort = TRUE)

#### how many inspections per year? -- what data should we focus on? 
restaurant_inspections %>% 
  count(inspection_date, sort = TRUE)

restaurant_inspections %>% 
  count(grade, sort = TRUE)

restaurant_inspections %>% 
  count(violation_description, sort = TRUE) %>%
  head() %>%
  pull(violation_description)

###can one inspection lead to mutliple violations? --> YES! 
restaurant_inspections%>% 
  count(camis, dba, inspection_date, sort = TRUE)

####Check violations for one restaurant
restaurant_inspections %>%
  filter(camis == 40400811, inspection_date == "2019-07-16" ) %>%
  count(camis, dba, inspection_date, sort = TRUE)

restaurant_inspections %>% 
  filter(action == "No violations were recorded at the time of this inspection") %>%
  count(critical_flag) 

### one row per inspection for each group, how many violations are there? 
restaurant_inspections %>% 
  group_by(camis, 
           dba, 
           boro, 
           zipcode, 
           cuisine_description, 
           inspection_date, 
           action,
           score,
           grade,) %>% 
  summarize(critical_violations = sum(!is.na(violation_code)))

#### sum of critical violations by groups 
inspections <- restaurant_inspections %>% 
  group_by(camis, 
           dba, 
           boro, 
           zipcode, 
           cuisine_description, 
           inspection_date, 
           action,
           score,
           grade,
           inspection_type,
           inspection_program) %>% 
  summarize(critical_violations = sum(critical_flag == "Critical", na.rm = TRUE),
        non_critical_violation = sum(critical_flag == "Not Critical", na.rm = TRUE)) %>%
        ungroup()
inspections
### Now we have 102991 observations now, what are the types of inspections we have? 
inspections %>% 
  count(inspection_type, sort = TRUE)

inspections %>% 
  filter(inspection_program == "Cycle",
         inspection_type == "Inspection") %>%
  count(grade, sort = TRUE)

## restaurants that have gotten a cycle inspection 
most_recent_cycle_inspection <- inspections %>% 
  filter(inspection_program == "Cycle", 
         inspection_type == "Inspection") %>% 
  arrange(desc(inspection_date)) %>%
  distinct(camis, .keep_all = TRUE)
view(most_recent_cycle_inspection)

##do some cuisines have different scores on average? 
most_recent_cycle_inspection %>% 
  group_by(cuisine_description) %>% 
  summarize(avg_score = mean(score),
            median_score = median(score),
            restaurants = n()) %>% 
  arrange(desc(restaurants))

###whats the distrubution of scores? the higher scores mean worst
##peak is low and tail has higher scores 
most_recent_cycle_inspection %>%
  ggplot(aes(score)) +
  geom_histogram()

## so whats the best test for this data? -> confidence intervals and t test 
###is it actually true that coffee cafes have less violations than latin american rest? 
### we are gonna use broom package, and take our data and nest it besides the cuisine desrip. 
library(broom)
##this is a one sample t-test 
t.test(c(1,2, 2, 2.5)) 

###create confidence intervals 
####not working - figure out why 
cuisine_conf_ints <- most_recent_cycle_inspection %>%
  add_count(cuisine_description) %>%
  filter(n > 100) %>%
  nest(cuisine_description) %>% 
  mutate(model = map(data, ~ t.test(inspections$score)))
  unnest(map(model, tidy))

#####
cuisine_conf_ints %>% 
mutate(cuisine_description = fct_reorder(cuisine_description, estimate)) %>%
ggplot(aes(estimate, cuisine_description)) + 
geom_point() 
-------------------------------------------
by_dba <- most_recent_cycle_inspection %>% 
           group_by(dba, cuisine = cuisine_description) %>%
            summarize(dba, n(),
              avg_score = mean(score),
              median_score = median(score)) %>%
        arrange(desc(restaurants))


by_dba %>% 
       ggplot(aes(avg_score)) + 
         geom_point()+
         scale_x_log10()






