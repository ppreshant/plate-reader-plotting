# read_multiple_sheets_plugin_timeseries.R

# run `run_single_plate.R` till line 14 (fl <- ..)

regex_sheets_to_get <- '\\.' # regex for recognizing sheets to get


# processing ----

sheets_to_get <- names(fl) %>% {.[str_detect(., regex_sheets_to_get)]}


multi_sheets <- 
map(sheets_to_get,
     ~ read_multiple_grids_in_sheet(.x) %>% .[[1]] %>% # get processed_data 
      mutate(sheet_name = .x)
    )

# TODO : need to carry over sample names from the first sheet