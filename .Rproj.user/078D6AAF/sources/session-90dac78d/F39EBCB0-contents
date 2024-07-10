library(tidyverse)
library(viridis)
library(readxl)
library(sf)
library(scales)
library(patchwork)
library(janitor)
library(flextable)
library(tidytext)
library(grid)
library(ggrepel)


counties <- read_csv("./data/counties.csv")


counties_sub_indicator_temp <- counties %>% 
  mutate(sex_modifier = case_when(
    indicator == "Gender Parity Index" & county == "National" ~ "total", 
    indicator == "Learner-Classroom Ratio in Public Schools" & county == "National" ~ "total", 
    indicator == "Learner to teacher Ratio in Public Schools" & county == "National" ~ "total",
    indicator == "Learner-Classroom Ratio in Public Schools" & county == "National" ~ "total",
    indicator == "Number of education Institutions (Public & Private)" ~ NA_character_, 
    TRUE ~ sex_modifier)) %>% 
  mutate(sex_modifier = str_to_upper(sex_modifier), 
         age_modifier = str_to_upper(age_modifier)) %>% 
  mutate(sub_indicator = paste0(str_to_title(indicator), " ", age_modifier, " ", sex_modifier), 
         sub_indicator = str_replace_all(sub_indicator, "NA", "")) %>% 
  mutate(value = ifelse(str_detect(sub_indicator, "Teenage Pregnancy|Gbv"), 
                        value * 100, 
                        value)) %>%
  filter(indicator != "Learner to Toilet Ratio in Public Schools") %>% 
  mutate(sub_indicator = str_replace_all(sub_indicator, "School_age", "School-aged"), 
         sub_indicator = str_replace_all(sub_indicator, "Out_of_school", "Out-of-School"),
         sub_indicator = str_replace_all(sub_indicator, "PREPRIMARY_ECE", "PREPRIMARY/ECE"),
         sub_indicator = str_replace_all(sub_indicator, "\\(Ner\\)", "(NER)"), 
         sub_indicator = str_replace_all(sub_indicator, "_", " "), 
         sub_indicator = str_replace_all(sub_indicator, "recalculated", ""), 
         sub_indicator = str_replace_all(sub_indicator, "Gbv", "GBV"), 
         sub_indicator = str_replace_all(sub_indicator, "Learner toilet", "Learner-Toilet"), 
         sub_indicator = str_replace_all(sub_indicator, "Learner To Teacher", "Learner-Teacher"), 
         sub_indicator = str_replace_all(sub_indicator, "TOTAL MALE", "MALE"), 
         sub_indicator = str_replace_all(sub_indicator, "TOTAL FEMALE", "FEMALE"),
         sub_indicator = str_replace_all(sub_indicator, "TOTAL TOTAL", "TOTAL"))  


counties_sub_indicator_temp %>% 
  
  rbind(counties_sub_indicator_temp %>% 
          filter(indicator %in% c("county_population", "school_age_children") & 
                   sex_modifier == "TOTAL") %>% 
          group_by(county) %>%
          mutate(county_population = max(value)) %>% 
          ungroup() %>% 
          mutate(pc = round(value / county_population * 100, digits = 2)) %>% 
          filter(pc != 1) %>% 
          mutate(sub_indicator = str_replace_all(sub_indicator, 
                                                 "TOTAL", 
                                                 "% of COUNTY POP.")) %>% 
          select(-county_population, -value) %>% 
          rename(value = pc),
        
        counties_sub_indicator_temp %>% 
          filter(indicator == "county_population" & sex_modifier != "TOTAL") %>%
          group_by(county) %>% 
          mutate(total = sum(value), 
                 pc = round(value / total * 100, digits = 2)) %>%
          ungroup() %>% 
          select(-value, -total) %>% 
          rename(value = pc) %>% 
          mutate(sub_indicator = str_replace_all(sub_indicator, 
                                                 "FEMALE", "FEMALE %"), 
                 sub_indicator = str_replace_all(sub_indicator, 
                                                 " MALE", " MALE % ")), 
        
        counties_sub_indicator_temp %>%
          filter(indicator %in% c("out_of_school_children", "school_age_children") & 
                   sex_modifier == "TOTAL") %>%
          group_by(indicator, county, sex_modifier) %>% 
          summarise(value = sum(value), .groups = "drop") %>% 
          group_by(county) %>% 
          mutate(school_age_children = max(value)) %>% 
          ungroup() %>% 
          mutate(pc = round(value / school_age_children * 100, digits = 2)) %>% 
          filter(pc != 1) %>% 
          mutate(age_modifier = "TOTAL",
                 sub_indicator = paste0(str_to_title(indicator), " ", age_modifier, " ", sex_modifier), 
                 sub_indicator = str_replace_all(sub_indicator, "NA", ""), 
                 sub_indicator = str_replace_all(sub_indicator, 
                                                 "TOTAL TOTAL", 
                                                 "% of SCHOOL-AGED CHILDREN")) %>%  
          select(-school_age_children, -value) %>% 
          rename(value = pc)
        
  ) %>%
  mutate(sub_indicator = str_replace_all(sub_indicator, "School_age_children", "School-aged Children"), 
         sub_indicator = str_replace_all(sub_indicator, "Out_of_school_children", "Out-of-School children")) %>%
  mutate(sex_modifier = str_to_upper(sex_modifier), 
         age_modifier = str_to_upper(age_modifier)) %>%
  
  write_csv("./data/counties_sub_indicators.csv")

     
