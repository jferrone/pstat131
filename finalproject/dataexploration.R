# Data Exploration for Final Project
library(tidyverse)
library(tidymodels)
library(corrplot)
library(janitor)

rawdata <- read_csv('finalproject/data/nycschooldata.csv')

school_df <- rawdata %>%
  clean_names %>%
  select(4:41) %>%
  na_if('N/A') %>%
  mutate(
    community_school = ifelse(community_school == 'Yes', 1, 0),
    school_income_estimate = as.numeric(
      gsub('[$,]', '', school_income_estimate)
      ),
    percent_ell = as.numeric(gsub('[%]', '', percent_ell)) / 100,
    percent_asian = as.numeric(gsub('[%]', '', percent_asian)) / 100,
    percent_black = as.numeric(gsub('[%]', '', percent_black)) / 100,
    percent_hispanic = as.numeric(gsub('[%]', '', percent_hispanic)) / 100,
    percent_blhi = as.numeric(gsub('[%]', '', percent_black_hispanic)) / 100,
    percent_white = as.numeric(gsub('[%]', '', percent_white)) / 100,
    attendance_rate = as.numeric(
      gsub('[%]', '', student_attendance_rate)
      ) / 100,
    chronically_absent = as.numeric(
      gsub('[%]', '', percent_of_students_chronically_absent)
      ) / 100,
    rigorous_instruction = as.numeric(
      gsub('[%]', '', rigorous_instruction_percent)
      ) / 100,
    collab_teachers = as.numeric(
      gsub('[%]', '', collaborative_teachers_percent)
    ) / 100,
    supportive_env = as.numeric(
      gsub('[%]', '', supportive_environment_percent)
    ) / 100,
    leadership = as.numeric(
      gsub('[%]', '', effective_school_leadership_percent)
    ) / 100,
    fam_com_ties = as.numeric(
      gsub('[%]', '', strong_family_community_ties_percent)
    ) / 100,
    trust = as.numeric(gsub('[%]', '', trust_percent)) / 100,
    ela_proficiency = as.numeric(average_ela_proficiency) / 100,
    math_proficiency = as.numeric(average_math_proficiency) / 100,
    avg_proficiency = (ela_proficiency + math_proficiency) / 2
  ) %>%
  select(
    -sed_code, -percent_black_hispanic, -student_attendance_rate,
    -percent_of_students_chronically_absent, -collaborative_teachers_percent,
    -rigorous_instruction_percent, -supportive_environment_percent,
    -effective_school_leadership_percent, -strong_family_community_ties_percent,
    -trust_percent, -average_ela_proficiency, -average_math_proficiency
  )

head(school_df)

cor(select_if(school_df, is.numeric), use="pairwise.complete.obs") %>%
  corrplot(method = 'shade', type = 'lower', diag = FALSE)

eda_p1 <- ggplot(school_df, 
                 aes(x = ela_proficiency, y = math_proficiency,
                     color = school_income_estimate)) +
  geom_point()

ggplot(school_df, aes(x = school_income_estimate)) + geom_histogram()

count(school_df, grades)
