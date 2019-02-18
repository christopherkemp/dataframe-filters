#' @title filter functions
#'  This module contains the function for filtering a dataframe based on input 
#'  supplied by the user throuh the UI

# Generic function for filtering dataframes:
# Arguments:
#' @param filterField field you want to filter over
#' @param filterValue name of the UI component sending some value to the server
#' @details filterValue can be a simple character vector, a numeric value, 
#' or a collapsed set of strings separated by the "|" character

# TODO: maybe add another parameter for "filter type" with values "numeric range", "category", and "string match"
# TODO: at some point, review whether this is the best way to structure this function. The logic feels kind of compliated:
# Perhasp you could break things into smaller components - if someone else were reading this, could they reasonably make sense of it?

# TODO: chnage the function so that `data` is a parameter you can feed it instead of prespecifying `StudyOutcomes`

# this handles two cases - numeric input or text input (ignoring the third case for now)
studyOutcomesFilter <- function(filterField, filterValue) {
  filterField <- rlang::enexpr(filterField)
  if (is.null(filterValue)) {
    dta <- StudyOutcomes %>% select(outcome.id)
  } 
  else if (length(filterValue)>0) {
    # Case: simple character vector
    if (typeof(filterValue) == "character") {
      dta <- StudyOutcomes %>% 
        filter(
          (tolower(!!filterField) %in% tolower(filterValue)) |
            # TODO: you've got to collapse the string field -- see example below
          (map_lgl(tolower(!!filterField), str_detect, pattern=tolower(paste(filterValue, collapse="|")))) 
        ) %>% 
        select(outcome.id)
    } 
    # Case: character vector with pattern match
    #else if ((typeof(filterValue) == "character") & 
    #         (str_detect(paste(filterValue, collapse="|"), '[|]'))) {
    #  dta <- StudyOutcomes %>% 
    #    filter(map_lgl(!!filterField, str_detect, pattern=paste(filterValue, collapse="|"))) %>% 
    #    select(outcome.id)
    #} 
    # Case: range of numbers
    else if (
      typeof(filterValue) %in% c("double", "integer", "float")) {
      dta <- StudyOutcomes %>% 
        filter(!!filterField >= filterValue[1] & !!filterField <= filterValue[2]) %>% 
        select(outcome.id)
    }
  }
  return(unique(dta[,1]))
}

# Study design filter
data_filtered_design <- reactive({
  studyOutcomesFilter(filterField = design.type, filterValue = input$StudyDesign)
})

# Publication year filter
data_filter_publicationYear <- reactive({
  studyOutcomesFilter(filterField = publication.year, filterValue = input$PublicationYear)
})

# SourceType filter
data_filtered_sourceType <- reactive({
  studyOutcomesFilter(filterField = source.type, filterValue = input$SourceType)
})

# `Continent` filter - e.g. "Africa", "Asia", "North America" (categorical)
data_filtered_continent <- reactive({
  studyOutcomesFilter(filterField = continent, filterValue = input$Continent)
})

# Country filter
data_filtered_country <- reactive({
  studyOutcomesFilter(filterField = country, filterValue = input$Country)
})

# Multicomponent filter
data_filtered_multicomponent <- reactive({
  studyOutcomesFilter(filterField = multicomponent, filterValue = input$MulticomponentInterventions)
})

# Cascade Target filter (requires filtering against comma-seperated-string-valued column)
data_filtered_cascadetarget <- reactive({
  studyOutcomesFilter(filterField = cascade.target, filterValue = input$CascadeTarget)
})

# Outcome Type filter (requires filtering against comma-seperated-string-valued column)
data_filtered_outcometype <- reactive({
  studyOutcomesFilter(filterField = outcome.type, filterValue = input$OutcomeType)
})

# `Cascade stop event` filter - e.g. "Test result given", "First post-diagnosis visit"
data_filtered_cascsadestopevent <- reactive({
  studyOutcomesFilter(filterField = cascade.stop.event, filterValue = input$CascadeStopEvent)
})

# `All study components` filter - e.g. "HIV testing: self testing kit" or "HIV-testing: self testing coupon"
data_filtered_studyinterventioncomponent <- reactive({
  studyOutcomesFilter(filterField = all.study.components, filterValue = input$StudyInterventionComponent)
})

# `Measure type` filter - e.g. "count", "percentage", etc (categorical)
data_filtered_measuretype <- reactive({
  studyOutcomesFilter(filterField = measure.type, filterValue = input$MeasureType)
})

# `Outcome level` filter - e.g. "individual", "community", etc (categorical)
data_filtered_outcomelevel <- reactive({
  studyOutcomesFilter(filterField = level.of.measurement, filterValue = input$OutcomeLevel)
})

# `Measure unit` filter - e.g. "people" (text match)
data_filtered_measureunit <- reactive({
  studyOutcomesFilter(filterField = measure.unit, filterValue = input$MeasureUnit)
})

# Compose all filters

# TODO: sort of a devotes: these currently all use `intersect` (eq to "OR" statement), but you should consider permitting the user to choose `union` (eq to "AND")
data_filtered_final <- reactive({
  # Make sure to pass the intersect functions vectors instead of dataframes, as something breaks on the more complicated string matching filters otherwise
  #final_rows_1 <- intersect(data_filtered_design(), data_filter_publicationYear())
  #final_rows_2 <- intersect(final_rows_1, data_filtered_sourceType())
  #final_rows_3 <- intersect(final_rows_2, data_filtered_continent())
  #final_rows_4 <- intersect(final_rows_3, data_filtered_country())
  #final_rows_5 <- intersect(final_rows_4, data_filtered_multicomponent())
  #final_rows_6 <- intersect(final_rows_5, data_filtered_cascadetarget())
  #final_rows_7 <- intersect(final_rows_6, data_filtered_outcometype())
  #final_rows_8 <- intersect(final_rows_7, data_filtered_outcomecategory())
  #final_rows_9 <- intersect(final_rows_8, data_filtered_studyinterventioncomponent())
  #StudyOutcomes %>% filter(outcome.id %in% final_rows_9)
  
  final_rows <- intersect(data_filtered_design(), data_filter_publicationYear()) %>%
    intersect(data_filtered_sourceType()) %>%
    intersect(data_filtered_continent()) %>%
    intersect(data_filtered_country()) %>%
    intersect(data_filtered_multicomponent()) %>%
    intersect(data_filtered_cascadetarget()) %>%
    intersect(data_filtered_outcometype()) %>%
    intersect(data_filtered_cascsadestopevent()) %>%
    intersect(data_filtered_studyinterventioncomponent()) %>%
    intersect(data_filtered_measuretype()) %>%
    intersect(data_filtered_outcomelevel()) %>%
    intersect(data_filtered_measureunit())
  StudyOutcomes %>% filter(outcome.id %in% final_rows)
})