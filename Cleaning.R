install.packages("devtools")
devtools::install_github("impact-initiatives/cleaningtools")
library(cleaningtools)
library(dplyr)


#rm(list = ls())

# The easiest way to get dplyr is to install the whole tidyverse:
#install.packages("tidyverse")

my_raw_dataset <- cleaningtools::cleaningtools_raw_data

#View(my_raw_dataset)
#names(my_raw_dataset)
#my_raw_dataset |> head()

my_kobo_survey <- cleaningtools::cleaningtools_survey
my_kobo_choice <- cleaningtools::cleaningtools_choices


###########################  Checking  #######################################################################

#check_pii
output_from_data <- cleaningtools::check_pii(dataset = cleaningtools::cleaningtools_raw_data, words_to_look = "date", uuid_column = "X_uuid")
output_from_data$potential_PII |> head()

#check_outliers
my_log1 <- my_raw_dataset %>% 
  check_outliers(uuid_column = "X_uuid")


#check_duplicate
my_log2 <- my_raw_dataset %>% 
  check_duplicate(uuid_column = "X_uuid")

my_log2$duplicate_log %>% 
  head()


#More checks
more_logs <- my_raw_dataset %>% 
  check_duplicate(uuid_column = "X_uuid") %>% 
  check_soft_duplicates(uuid_column = "X_uuid", kobo_survey = my_kobo_survey, sm_separator = ".") %>%
  check_outliers(uuid_column = "X_uuid") %>%
  check_value(uuid_column = "X_uuid") 







#######################add_duration method 01
more_logs$checked_dataset <- more_logs$checked_dataset %>% 
  add_duration(uuid_column = "X_uuid", start_column = "X.U.FEFF.start", end_column = "end")
more_logs$checked_dataset[1:6, c("start_date", "start_time", "end_date", "end_time", "days_diff", "duration")]

#Check duration
more_logs <- more_logs %>% 
  check_duration(column_to_check = "duration", uuid_column = "X_uuid")



########################add_duration method 02
my_audit_list <- cleaningtools::create_audit_list(audit_zip_path = "audit_for_tests_100.zip")
cleaningtools::add_duration_from_audit(some_dataset, uuid_column = "X_uuid", audit_list = list_audit)

cleaningtools::check_duration(
  testdata,
  column_to_check = "duration_audit_start_end_ms",
  lower_bound = 375490,
  higher_bound = 8642000
) |> head()







#Check others

# pull the text type columns from the survey
other_columns_to_check <- my_kobo_survey %>% 
  dplyr::filter(type == "text") %>% 
  dplyr::filter(name %in% names(my_raw_dataset)) %>%
  dplyr::pull(name) 

# check_*
more_logs <- more_logs %>% 
  check_others(uuid_column = "X_uuid", columns_to_check = other_columns_to_check)




#Checks with logical list
# check_id column is unique to this check
logical_check_list <- readxl::read_excel("./inputs/01 - example - check_list.xlsx")
View(logical_check_list)

example_logic <- my_raw_dataset %>% 
  check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = logical_check_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description_column = "description")

example_logic$logical_all %>% 
  head()



###############    Pipable   ###############################################################################

#Pipe-able
my_log3 <- my_raw_dataset %>% 
  check_outliers(uuid_column = "X_uuid") %>% 
  check_duplicate(uuid_column = "X_uuid")

my_log3$potential_outliers %>% 
  head()



###################    Cleaning   ###############################################################################

