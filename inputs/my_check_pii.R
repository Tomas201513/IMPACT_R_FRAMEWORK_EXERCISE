function (dataset, element_name = "checked_dataset", uuid_column = "uuid", 
          words_to_look = NULL) 
{
  inputed_element <- dataset
  if (!is.data.frame(dataset) & is.list(dataset)) {
    if (!element_name %in% names(dataset)) {
      stop("element_name not found")
    }
  }
  if (!is.data.frame(dataset) & is.list(dataset)) {
    dataset <- dataset[[element_name]]
  }
  if (uuid_column %in% names(dataset) == FALSE) {
    stop("uuid not found in the dataset")
  }
  cols_to_look_for <- c("telephone", "contact", "name", "gps", 
                        "neighbourhood", "latitude", "longitude", "phone", "contact number", 
                        "geo location", "geo", "contact", "nom", "gps", "voisinage")
  cols_to_look_for <- c(words_to_look, cols_to_look_for) %>% 
    snakecase::to_snake_case()
  cols_to_look_for <- paste0(cols_to_look_for, collapse = "|")
  select_multiple_to_ignore <- auto_sm_parent_children(dataset)
  ignore <- c(select_multiple_to_ignore$sm_child, select_multiple_to_ignore$sm_parent) %>% 
    unique()
  potential_PII <- tibble::tibble(uuid = "all", question = names(dataset)) %>% 
    dplyr::filter(!question %in% ignore) %>% dplyr::mutate(snkae_case_cols = snakecase::to_snake_case(question)) %>% 
    dplyr::filter((grepl(cols_to_look_for, snkae_case_cols)) | 
                    (grepl(cols_to_look_for, question))) %>% dplyr::mutate(issue = "Potential PII") %>% 
    dplyr::select(-snkae_case_cols)
  if (is.data.frame(inputed_element)) {
    checked_dataset <- dataset
    return(list(checked_dataset = checked_dataset, potential_PII = potential_PII))
  }
  if (!is.data.frame(inputed_element)) {
    list_Df <- list(potential_PII = potential_PII)
    return(append(inputed_element, list_Df))
  }
}
