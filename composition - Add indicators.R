###########################################################################################################################
################################### 01 - Add indicators   #############################################################

library(impactR4PHU)
library(dplyr)
library(addindicators)

# To Compose MSNA Indicators use humind
# install.packages("devtools")
# devtools::install_github("impact-initiatives-hppu/humind")

my_data <- impactR4PHU::impactR4PHU_data_template |>  
  filter(respondent_consent != "no") 
names(my_data)



## You receive a dataset, you need to review the following four indicators.
    # Food Consumption Score: fsl_fcs_score, fsl_fcs_cat
    # Household Hunger Score: hhs_score, hhs_cat



#----------------------------------add_fcs-----------------------------------------------------------------------
my_data_with_fcs <- my_data %>% add_fcs(
  cutoffs = "normal"
)

my_data_with_fcs[, tail(names(my_data_with_fcs), 10)] %>%
  head()


#----------------------------------add_hhs-----------------------------------------------------------------------
my_data_with_indicators <- my_data %>%
  add_fcs(
    cutoffs = "normal"
  ) %>%
  add_hhs(
  )

my_data_with_indicators[, tail(names(my_data_with_indicators), 14)] %>%
  head()





#----------------------------------add_rcsi-----------------------------------------------------------------------


#----------------------------------add_eg_fclcm_phase-----------------------------------------------------------------------

#----------------------------------add_eg_fcm_phase-----------------------------------------------------------------------


###########################################################################################################################
################################### 02 - Review an indicator   #############################################################


#----------------------------------review_variables-----------------------------------------------------------------------

# new dataset for review
review_df <- my_data %>%
  add_fcs(
    cutoffs = "normal",
  ) %>%
  select(uuid, fsl_fcs_score, fsl_fcs_cat)

# bind dataset to be reviewed and the new dataset 
binded_df <- my_data_with_indicators %>%
  full_join(review_df, by = "uuid")

# review
review_one_var <- review_variables(binded_df,
                                   columns_to_review = "fsl_fcs_cat.x",
                                   columns_to_compare_with = "fsl_fcs_cat.y")


# review_one_var %>% 
#   names()
# 
# View(review_one_var$review_table)
# 
# review_one_var$review_table %>%
#   group_by(review_check, review_comment) %>%
#   tally()



#----------------------------------mess with the variable to see how review works-----------------------------------------------------------------------

jittered_df <- binded_df
set.seed(123)
jittered_df[sample(1:nrow(jittered_df), 5), "fsl_fcs_cat.x"] <- sample(unique(jittered_df$fsl_fcs_cat.y), 5, T)
set.seed(124)
jittered_df[sample(1:nrow(jittered_df), 5), "fsl_fcs_cat.y"] <- sample(unique(jittered_df$fsl_fcs_cat.y), 5, T)
set.seed(125)
jittered_df[sample(1:nrow(jittered_df), 5), "fsl_fcs_cat.x"] <- NA
set.seed(1236)
jittered_df[sample(1:nrow(jittered_df), 5), "fsl_fcs_cat.y"] <- NA
set.seed(1237)
jittered_df[sample(1:nrow(jittered_df), 5), "fsl_fcs_score.x"] <- sample(unique(jittered_df$fsl_fcs_score.x), 5, T)


review_one_variable_jittered <- review_variables(jittered_df,
                                                 columns_to_review = "fsl_fcs_cat.x",
                                                 columns_to_compare_with = "fsl_fcs_cat.y")

review_one_variable_jittered$review_table %>%
  group_by(review_check, review_comment) %>%
  tally()

review_one_variable_jittered$dataset[, tail(names(review_one_variable_jittered$dataset), 5)] %>%
  head()

review_one_variable_jittered$dataset %>%
  filter(!review_check_fsl_fcs_cat.x) %>%
  select(uuid, fsl_fcs_cat.x, fsl_fcs_cat.y, review_check_fsl_fcs_cat.x, review_comment_fsl_fcs_cat.x)




