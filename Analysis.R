###########################################################################################################################
################################### Chapter 5: Analysis Introduction   #############################################################
library(analysistools)
library(dplyr)

my_data <- analysistools_MSNA_template_data
only_nas <- analysistools_MSNA_template_data %>%
  summarise(across(.cols = everything(), .fns = function(x) {
    sum(is.na(x)) == nrow(analysistools_MSNA_template_data)
  })) %>%
  do.call(c, .)

my_data_shorter <- analysistools_MSNA_template_data[, !only_nas] %>%
  select(!grep("other", names(analysistools_MSNA_template_data), value = T))

View(my_data_shorter)


#----------------------------------create_analysis-----------------------------------------------------------------------


my_design <- srvyr::as_survey_design(my_data_shorter, strata = "admin1")
#View(my_design)

my_analysis <- create_analysis(my_design, sm_separator = "/")
View(my_analysis$results_table)
my_analysis$results_table %>%
  head()





#----------------------------------create_analysis-----------------------------------------------------------------------
sampling_frame <- data.frame(
  strata = c("admin1a", "admin1b", "admin1c"),
  population = c(100000, 200000, 300000)
)
sampling_frame

# add_weights
exercise_data_shorter_weigthed <- my_data_shorter %>% 
  add_weights(sample_data = sampling_frame, strata_column_dataset = "admin1", strata_column_sample = "strata", population_column = "population")

# survey design
my_exercise_design_weigthed <- srvyr::as_survey_design(exercise_data_shorter_weigthed, strata = "admin1", weights = "weights") 

# create_analysis
my_answer_analysis_weighted <- create_analysis(my_exercise_design_weigthed, sm_separator = "/")















###########################################################################################################################
################################### Chapter 6: Extended Analysis   ########################################################
library(analysistools)
library(dplyr)

my_data <- analysistools::analysistools_MSNA_template_data

sampling_frame <- data.frame(
  strata = c("admin1a", "admin1b", "admin1c"),
  population = c(100000, 200000, 300000)
)


#----------------------------------01 - Analysis - list of analysis----------------------------------------------------------

# loa
my_loa <- analysistools::analysistools_MSNA_template_loa
View(my_loa)

#add_weights
my_data <- my_data %>% 
  add_weights(sampling_frame, "admin1", "strata", "population")

# create survey design
my_design <- srvyr::as_survey_design(my_data, weights = "weights", strata = "admin1")
View(my_design)

# create_analysis
my_results <- create_analysis(my_design, loa = my_loa, sm_separator = "/")
View(my_results)



#----------------------------------02 - Ratio ----------------------------------------------------------
library(analysistools)
library(dplyr)

# prepare the data
my_data <- analysistools::analysistools_MSNA_template_data
set.seed(1323)
my_data <- my_data |>
  mutate(num_aged_school_children = round(runif(100, min = 0, max = 5)),
         num_enrolled_school_children = round(runif(100, min = 0, max = 5)),
         num_enrolled_school_children = case_when(num_aged_school_children == 0 ~ NA, num_aged_school_children < num_enrolled_school_children ~ num_aged_school_children,
                                                  TRUE ~ num_enrolled_school_children ))

View(my_data)

# prepare the sample frame
sampling_frame <- data.frame(
  strata = c("admin1a", "admin1b", "admin1c"),
  population = c(100000, 200000, 300000)
)

# add_weights
my_data <- my_data %>% 
  add_weights(sampling_frame, "admin1", "strata", "population")

# as_survey_design
my_design <- srvyr::as_survey_design(my_data, weights = "weights", strata = "admin1")


# read loa
my_loa_with_ratio <- read.csv("./inputs/07 - example - loa_with_ratio.csv")
View(my_loa_with_ratio)
# filter ratio
my_loa_with_ratio %>% 
  filter(analysis_type == "ratio") |>
  select(analysis_type, analysis_var, group_var, analysis_var_numerator, analysis_var_denominator)

# create_analysis
my_results_with_ratio <- create_analysis(my_design, loa = my_loa_with_ratio, sm_separator = "/")


my_results_with_ratio$results_table %>% 
  filter(analysis_type == "ratio")

View(my_results_with_ratio)




#----------------------------------create_analysis_ratio ----------------------------------------------------------

school_ex <- data.frame(
  hh = c("hh1", "hh2", "hh3", "hh4"),
  num_children = c(3, 0, 2, NA),
  num_enrolled = c(3, NA, 0, NA),
  num_attending = c(1, NA, NA, NA)
)

me_design <- srvyr::as_survey(school_ex)

school_ex

create_analysis_ratio(me_design,
                      analysis_var_numerator = "num_attending",
                      analysis_var_denominator = "num_children",
                      numerator_NA_to_0 = TRUE,
                      filter_denominator_0 = TRUE
                      ) %>%
  select(analysis_type, analysis_var, stat, n, n_total, analysis_key)









#----------------------------------EXCERCISE ----------------------------------------------------------
library(analysistools)
library(dplyr)

exercise_data <- analysistools::analysistools_MSNA_template_data

exercise_sampling_frame  <- data.frame(
  strata = c("admin1a", "admin1b", "admin1c"),
  population = c(100000, 200000, 300000)
)

exercise_data <- exercise_data %>%
  add_weights(
    exercise_sampling_frame,
    "admin1", "strata", "population"
  )

exercise_design <- srvyr::as_survey_design(exercise_data, weights = "weights", strata = "admin1")

exercise_loa <- readxl::read_excel("./inputs/09 - correction - loa.xlsx")

exercise_results <- create_analysis(exercise_design, loa = exercise_loa, sm_separator = "/")
View(exercise_results$results_table)
