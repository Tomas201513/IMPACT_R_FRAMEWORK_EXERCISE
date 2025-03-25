####################################################################################################################################
################################### Chapter 6: Outputs - Part 1  ###################################################################


#---------------------------------- 01 Labels ----------------------------------------------------------

# Outputs: Any manipulation to format the outputs. Outputs are created from the results table, from the stat + analysis key

library(presentresults)
library(dplyr)

results_table <- presentresults_MSNA2024_results_table
View(results_table)

kobo_survey <- presentresults_MSNA2024_kobotool_template$kobo_survey
kobo_choices <- presentresults_MSNA2024_kobotool_template$kobo_choices

results_table[c(1,9), c("analysis_var", "group_var", "group_var_value")]
kobo_survey |>
  tail() |>
  select(type, name, `label::english`)


# Adding labels to result table will be done in three steps:

# 1- review the KOBO tools for duplicate label and name 
# If there are new variables added to the dataset, they should also be added in the KOBO tool.

review_kobo_labels_results <- review_kobo_labels(kobo_survey,kobo_choices,results_table=results_table)
review_kobo_labels_results


# updating the tool
kobo_survey_fixed <- kobo_survey
kobo_survey_fixed[
  which(kobo_survey_fixed[["label::english"]] == "How often did this happen in the past [4 weeks/30 days]?"),
  "label::english"
] <- paste(
  "How often did this happen in the past [4 weeks/30 days]? ---",
  c(
    "In the past 4 weeks (30 days), was there ever no food to eat of any kind in your house because of lack of resources to get food?",
    "In the past 4 weeks (30 days), did you or any household member go to sleep at night hungry because there was not enough food?",
    "In the past 4 weeks (30 days), did you or any household member go a whole day or night without eating anything at all because there was not enough food?"
  )
)

kobo_choices_fixed <- kobo_choices |>
  filter(!`label::english` %in% c(
    "No shelter (sleeping in the open)",
    "Surface water (river, dam, lake, pond, stream, canal, irrigation channel)"
  ))

duplicated_listname_label <- review_kobo_labels_results |> filter(comments == "Kobo choices sheet has duplicated labels in the same list_name.")


kobo_choices_fixed <- kobo_choices_fixed |>
  group_by(list_name)  |> 
  mutate(`label::english` = case_when(
    list_name %in% duplicated_listname_label$list_name ~ paste(`label::english`, row_number()),
    TRUE ~ `label::english`
  ))  |> 
  ungroup()


#review te=he tool
review_kobo_labels(kobo_survey_fixed, kobo_choices_fixed, results_table = results_table)


#2 - create label dictionary
label_dictionary <- create_label_dictionary(kobo_survey_fixed,kobo_choices_fixed,results_table=results_table)
View(label_dictionary)

label_dictionary$dictionary_survey |> head(10)
label_dictionary$dictionary_choices |> head(10)
label_dictionary$analysis_type_dictionary |> head(10)



#3 - add labels to result table

results_table_labeled <- add_label_columns_to_results_table(
  results_table,
  label_dictionary
)

View(results_table_labeled)




#---------------------------------- 02 Wide Tabels ----------------------------------------------------------

# Long tabel - one that have the variables in the rows and the disagregation in the columns,
# Wide table - one that have the disagregation in the rows and the variables in the columns.

library(presentresults)

my_results_table <- presentresults::presentresults_resultstable

View(my_results_table)


# create_*_group_x_variable  ----- Wide table with the groups in the rows and the variables in the columns.

my_results_table %>% 
  create_table_group_x_variable() %>% 
  create_xlsx_group_x_variable(file_path = "./outputs/04 - example - group_x_variable.xlsx", overwrite = T)


# create_*_variable_x_group  ----- Wide table with the variables in the rows and the groups in the columns.
my_results_table %>% 
  create_table_variable_x_group() %>%
  create_xlsx_variable_x_group(file_path = "./outputs/05 - example - variable_x_group.xlsx", overwrite = T)





#---------------------------------- 04 Practice 1----------------------------------------------------------

library(dplyr)
library(presentresults)

label_exercise_results <- readxl::read_excel("./inputs/11 - exercise - label.xlsx", sheet = "results_table")
label_exercise_kobo_survey <- readxl::read_excel("./inputs/11 - exercise - label.xlsx", sheet = "kobo_survey")
label_exercise_kobo_choices <- readxl::read_excel("./inputs/11 - exercise - label.xlsx", sheet = "kobo_choices")

head(label_exercise_results)
head(label_exercise_kobo_survey)
head(label_exercise_kobo_choices)


label_exercise_kobo_survey <- label_exercise_kobo_survey %>% rename(`label::english`=label)
label_exercise_kobo_choices <- label_exercise_kobo_choices %>% rename(`label::english`=label)



review_kobo_labels_results <- review_kobo_labels(label_exercise_kobo_survey,
                                                 label_exercise_kobo_choices,
                                                 results_table=label_exercise_results)

label_dictionary <- create_label_dictionary(label_exercise_kobo_survey,
                                            label_exercise_kobo_choices,
                                            results_table=label_exercise_results)

View(label_dictionary)


#---------------------------------- 04 Practice 2----------------------------------------------------------
library(presentresults)
library(dplyr)
exercise_outputs <- readxl::read_excel("./inputs/10 - exercise - analysis_to_review.xlsx")

exercise_outputs %>% 
  create_table_group_x_variable() %>% 
  create_xlsx_group_x_variable(file_path = "./outputs/06 - correction - group_x_variable_table.xlsx", overwrite = T) 