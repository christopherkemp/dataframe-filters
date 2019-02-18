#' @title dataframe-filter
#'  filter a dataframe based on input values
#'  
#' dependencies 
library(rlang)
library(dplyr)
library(stringr)

# Generic function for filtering dataframes:
# Arguments:
#' @param data dataframe to be filtered
#' @param field field you want to filter over
#' @param value name of the UI component sending some value to the server in a shiny app
#' @param id field which uniquely identifies rows of the intialDataframe
#' @details value can be a simple character vector, a numeric value or range of values, 
#' or a collapsed set of strings separated by the "|" character

dataframeFilter <- function(data, id, field, value){
  field <- rlang::enexpr(field)
  id_q <- rlang::enexpr(id)
  
  # check if any filters are applied, if not return original dataframe
  if (is.null(value)) {
    data <- data %>% select(!!id_q)
  } 
  else if (length(value)>0){
    # Case: character vector
    if (identical(typeof(value), "character") & is_false(is.list(value))){
      data <- data %>% 
        filter(tolower(!!field) %in% tolower(value)) %>% 
        select(!!id_q)
    } 
    # Case: pattern match on list elements (useful for Shiny selectize inputs)
    else if (identical(typeof(value), "character") & is.list(value)) {
      data <- data %>% 
        filter(map_lgl(tolower(!!field), str_detect, pattern = tolower(paste(value, collapse="|")))) %>% 
        select(!!id_q)
    } 
    # Case: single numeric value or range of numbers
    else if (typeof(value) %in% c("double", "integer", "float")){
      data <- data %>% 
        filter((!!field >= value[1]) | (!!field >= value[1] & !!field <= value[2])) %>% 
        select(!!id_q)
    }
  }
  return(unique(data %>% pull(!!id_q)))
}

#' @example 
#' For use within an R script, but can be extended to other cases 
#' Adapt for Shiny by wrapping `final_rows` in a reactive expression

#' Use iris dataset, creating column `id` from rownames
df <- iris %>% 
  mutate(row_id = 
    rownames(iris)
  )

#' create three filters
filter_1 <- dataframeFilter(data = df, id = row_id, field = Species, value = c("setosa"))
filter_2 <- dataframeFilter(data = df, id = row_id, field = Sepal.Length, value = c(4))
filter_3 <- dataframeFilter(data = df, id = row_id, field = Petal.Length, value = c(1.5, 1.9))

#' compose filters
final_rows <- filter_1 %>% 
  intersect(filter_2) %>%
  intersect(filter_3)

#' show resulting filtered dataframe
df %>% filter(row_id %in% final_rows)

#' @details 
#' You can build upon this example in several ways
#' First, perhaps the function is overspecified, and you want to allow the user
#' to define their own inequality conidtions within function calls to dataframeFilters.
#' Second, perhaps you might encounter other input cases I haven't considered.
#' Third, perhaps you don't want the compose-filters to be constrained to only
#' using intersect statements, but you want to have the option of defining more complex
#' logic, either in your script or through the UI of a Shiny app. Both would provide a 
#' really nice kind of flexibility to this process. 
