# install.packages("devtools")
# devtools::install_github("impact-initiatives/cleaningtools")
library(cleaningtools)
library(dplyr)

rm(list = ls())
# The easiest way to get dplyr is to install the whole tidyverse:
#install.packages("tidyverse")


###########################  Data  #######################################################################
my_raw_dataset <- cleaningtools::cleaningtools_raw_data
#View(my_raw_dataset)
#names(my_raw_dataset)
#my_raw_dataset |> head()

my_kobo_survey <- cleaningtools::cleaningtools_survey
my_kobo_choice <- cleaningtools::cleaningtools_choices





##################################################################################################
######################################## Checking ################################################
##################################################################################################



###################    Check PII   ###############################################################################
PII=readxl::read_excel("./inputs/pii.xlsx")
View(PII)


#check_pii
my_pii_check <- cleaningtools::check_pii(
  dataset = cleaningtools::cleaningtools_raw_data,
  uuid_column = "X_uuid",
  # words_to_look= c("email", "ki_number")
  #words_to_look = PII$pii
  )

View(my_pii_check)





###################    Check outlier   ###############################################################################
#check_outliers (+/- 3 standard deviation...log(x + 1) transformation)
my_outlier_check <- my_raw_dataset %>%
  check_outliers(uuid_column = "X_uuid")
View(my_outlier_check)





###################    Check value   ###############################################################################

my_value_check <- my_raw_dataset %>%
  cleaningtools::check_value( uuid_column = "X_uuid", 
                                     element_name = "checked_dataset", 
                                     values_to_look = c(99, 98, 88, 888))

my_value_check$flaged_value |> head()







###################    Check duplicate   ###############################################################################
#check_duplicate
my_duplicate_check <- my_raw_dataset %>%
  check_duplicate(uuid_column = "X_uuid")

my_duplicate_check$duplicate_log %>%
  head()






###################    Check soft duplicate   ###############################################################################
#More checks
my_soft_duplicate_check <- my_raw_dataset %>%
  check_soft_duplicates(
    kobo_survey = cleaningtools_survey,
    uuid_column = "X_uuid",
    idnk_value = "dont_know",
    sm_separator = ".",
    log_name = "soft_duplicate_log",
    threshold = 7
  )
my_soft_duplicate_check$soft_duplicate_log %>%
  head()





###################    Check others  ###############################################################################

# pull the text type columns from the survey
other_columns_to_check <- my_kobo_survey %>%
  dplyr::filter(type == "text") %>%
  dplyr::filter(name %in% names(my_raw_dataset)) %>%
  dplyr::pull(name)

# check_*
my_other_check <- my_raw_dataset %>%
  check_others(uuid_column = "X_uuid", columns_to_check = other_columns_to_check)

View(my_other_check$other_log)






###################    Check duration 01  ###############################################################################
my_duration_check <- my_raw_dataset %>%
  add_duration(uuid_column = "X_uuid",
               start_column = "X.U.FEFF.start",
               end_column = "end")
my_duration_check[1:6, c("start_date",
                                 "start_time",
                                 "end_date",
                                 "end_time",
                                 "days_diff",
                                 "duration")]

#Check duration
my_duration_check <- my_duration_check %>%
  check_duration(column_to_check = "duration", uuid_column = "X_uuid",
                 lower_bound = 25, higher_bound = 90)

View(my_duration_check)







########################   Check duration 02  ################################################################
# my_audit_list <- cleaningtools::create_audit_list(audit_zip_path = "audit_for_tests_100.zip")
# cleaningtools::add_duration_from_audit(some_dataset, uuid_column = "X_uuid", audit_list = list_audit)
# 
# cleaningtools::check_duration(
#   testdata,
#   column_to_check = "duration_audit_start_end_ms",
#   lower_bound = 375490,
#   higher_bound = 8642000
# ) |> head()



#############################logical check with list######################################################
#Checks with logical list
# check_id column is unique to this check
logical_check_list <- readxl::read_excel("./inputs/check_list.xlsx")
View(logical_check_list)

my_logic_check <- my_raw_dataset %>%
  check_logical_with_list(
    uuid_column = "X_uuid",
    list_of_check = logical_check_list,
    check_id_column = "check_id",
    check_to_perform_column = "check_to_perform",
    columns_to_clean_column = "columns_to_clean",
    description_column = "description"
  )

my_logic_check$logical_all %>%
  head()

View(my_logic_check$checked_dataset)


###################    Check logical with list  ###############################################################################

my_percentage_missing <- my_raw_dataset %>% cleaningtools::add_percentage_missing()
#data_example |> head()
View(my_percentage_missing)


my_percentagecheck <- my_percentage_missing %>%
  cleaningtools::check_percentage_missing(
    uuid_column = "X_uuid"
  ) 

View(my_percentagecheck)


###################    Check logical with list  ###############################################################################

list_log <-list_of_log <- c(my_pii_check,my_percentage_missing, my_outlier_check, my_duplicate_check,
                            my_soft_duplicate_check, my_value_check, 
                            my_other_check, my_duration_check,
                            logical_check_list)
  
my_combined_log <- create_combined_log(list_of_log = list_of_log, dataset_name = "checked_dataset")
  




  
  
###################    create_xlsx_cleaning_log ###############################################################################
create_xlsx_cleaning_log(
    my_combined_log,
    sm_dropdown_type = "logical",
    output_path = paste0(output_dir, "/", Sys.Date(), "_follow-ups.xlsx")
  )










###############    Pipable   ###############################################################################

#Pipe-able
my_log3 <- my_raw_dataset %>%
  check_outliers(uuid_column = "X_uuid") %>%
  check_duplicate(uuid_column = "X_uuid") %>%
  check_soft_duplicates(
    uuid_column = "X_uuid",
    kobo_survey = my_kobo_survey,
    sm_separator = "."
  ) %>%
  check_value(uuid_column = "X_uuid") %>%
  check_duration(column_to_check = "duration", uuid_column = "X_uuid") %>%
  check_others(uuid_column = "X_uuid", columns_to_check = other_columns_to_check)


my_log3$potential_outliers %>%
  head()



###################    Cleaning   ###############################################################################
