install.packages("devtools")
devtools::install_github("impact-initiatives/cleaningtools")
library(cleaningtools)
library(dplyr)



#################################################################################################
# read raw dataset and tool

my_raw_dataset <- cleaningtools::cleaningtools_raw_data
my_kobo_survey <- cleaningtools::cleaningtools_survey
my_kobo_choice <- cleaningtools::cleaningtools_choices
View(my_raw_dataset)



#################################################################################################
# check_outliers   (Outliers are defined as +/- 3 standard deviation from the mean.)

my_log1 <- my_raw_dataset %>% 
  check_outliers(uuid_column = "X_uuid") %>% 
  check_duplicate(uuid_column = "X_uuid") %>%  
  check_soft_duplicates(uuid_column = "X_uuid", kobo_survey = my_kobo_survey, sm_separator = ".") %>%
  check_value(uuid_column = "X_uuid",values_to_look = c(99, 999, 999, 88, 888, 888)) 
# typeof(my_log1)
# my_log1 %>% 
#   names()
# View(my_log1)



#################################################################################################
# 02 - Add, durations and others

my_log1$checked_dataset <- my_log1$checked_dataset %>% 
  add_duration(uuid_column = "X_uuid", start_column = "X.U.FEFF.start", end_column = "end")
#View(my_log1)

# view 6 rows and only spp columns
my_log1$checked_dataset[1:6, c("start_date", "start_time", "end_date", "end_time", "days_diff", "duration")]

my_log1 <-my_log1$checked_dataset %>%
  check_duration(column_to_check = "duration", uuid_column = "X_uuid")
#View(more_logs)



#################################################################################################
# other/text columns

# pull others column 
other_columns_to_check <- my_kobo_survey %>% 
  dplyr::filter(type == "text") %>% 
  dplyr::filter(name %in% names(my_raw_dataset)) %>%
  dplyr::pull(name) 

my_log1 <- my_log1 %>% 
  check_others(uuid_column = "X_uuid", columns_to_check = other_columns_to_check) 


#################################################################################################
# check_logical_with_list

logical_check_list <- readxl::read_excel("./inputs/01 - example - check_list.xlsx")
logical_check_list

my_log1 <- my_log1 %>% 
  check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = logical_check_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description_column = "description")

example_logic$logical_all %>% 
  head()

# the above logical check work just like this script 
# my_raw_dataset %>% 
#   dplyr::mutate(xxx =  primary_livelihood.employment == 1 & tot_expenses < 200000) %>% 
#   dplyr::select(X_uuid, xxx, primary_livelihood.employment, tot_expenses) %>% 
#   head()



#################################################################################################
# check_pii
my_log1 <- my_log1 %>% 
  check_pii(uuid_column = "X_uuid")


#################################################################################################
# percentage_missing
my_log1$checked_dataset <- my_log1$checked_dataset %>% 
  add_percentage_missing()
# View(per_missing)

my_log1 <- my_log1 %>% check_percentage_missing(uuid_column="X_uuid")
View(check_per_missing)

