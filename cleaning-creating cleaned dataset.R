#################################################################################################
################################### 01. create cleaning log   ####################################



names(more_logs)
#----------------------------------create_combined_log-----------------------------------------------------------------------
my_combined_log <- create_combined_log(more_logs)

my_combined_log <- create_combined_log(more_logs)


my_combined_log %>% 
  head()



#----------------------------------add_info_to_cleaning_log-----------------------------------------------------------------------
my_combined_log <- my_combined_log %>% 
  add_info_to_cleaning_log(dataset_uuid_column = "X_uuid", 
                           information_to_add = "enumerator_num")

my_combined_log$cleaning_log %>% 
  head()

#----------------------------------create_xlsx_cleaning_log-----------------------------------------------------------------------
create_xlsx_cleaning_log(my_combined_log,
                         sm_dropdown_type = "logical",
                         output_path =  "./outputs/01 - example - cleaning-log-no-kobo.xlsx")

create_xlsx_cleaning_log(my_combined_log,
                         kobo_survey = my_kobo_survey,
                         kobo_choices = my_kobo_choice,
                         use_dropdown = T,
                         sm_dropdown_type = "logical",
                         output_path =  "./outputs/02 - example - cleaning-log-with-kobo.xlsx")



###########################################################################################################################
################################### 01. create cleaned dataset #############################################################



#----------------------------------review_cleaning_log-----------------------------------------------------------------------
my_filled_log <- readxl::read_excel("./inputs/02 - example - cleaning-log-with-kobo - filled.xlsx", sheet = 2)
#View(my_filled_log)
check_log_results <- review_cleaning_log(raw_dataset = my_raw_dataset,
                                         raw_data_uuid_column = "X_uuid",
                                         cleaning_log = my_filled_log, 
                                         cleaning_log_uuid_column = "uuid",
                                         cleaning_log_question_column = "question",
                                         cleaning_log_new_value_column = "new_value",
                                         cleaning_log_change_type_column = "change_type",
                                         change_response_value = "change_response")
check_log_results


#----------------------------------create_clean_data-----------------------------------------------------------------------
 create_clean_data
my_clean_data <- create_clean_data(raw_dataset = my_raw_dataset,
                                   raw_data_uuid_column = "X_uuid",
                                   cleaning_log = my_filled_log, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")


my_filled_log %>% 
  filter(question == "primary_livelihood.employment", 
         change_type == "change_response") %>% 
  select(uuid, question, old_value, new_value)

my_clean_data %>% 
  filter(X_uuid %in% c("eb3b9935-fa0b-4d54-8058-3b629f1421ad", "eec1f630-15d5-475e-a344-32bba74b32ea", "f1175d29-ce1f-43a7-b3d1-ee26cd1b8cdb")) %>% 
  select(X_uuid,primary_livelihood, primary_livelihood.employment)


#----------------------------------recreate_parent_column-----------------------------------------------------------------------
#recreate_parent_column will recode the parent columns based on the dummies.
my_clean_data2 <- recreate_parent_column(dataset = my_clean_data,
                                         uuid_column = "X_uuid",
                                         kobo_survey = my_kobo_survey,
                                         kobo_choices = my_kobo_choice,
                                         sm_separator = ".", 
                                         cleaning_log_to_append = my_filled_log)
#View(my_clean_data2)
my_clean_data2$data_with_fix_concat %>% 
  filter(X_uuid %in% c("eb3b9935-fa0b-4d54-8058-3b629f1421ad", "eec1f630-15d5-475e-a344-32bba74b32ea", "f1175d29-ce1f-43a7-b3d1-ee26cd1b8cdb")) %>% 
  select(X_uuid,primary_livelihood, primary_livelihood.employment)
