
# Scatter plot with a linear regression fit and equation

# Almost generalized: grouping_var enabled for NULL and any single variable 
# - lm eqns is the only bottleneck
# - Need to make and space out equations for each group..


plot_scatter <- 
  function(.data = processed_quant_data, # data to be plotted
           
           # Key variables to plot (after the data is pivoted into wide format)
           x_var = `SARS CoV-2 N1`, # make this variable as the x-axis
           y_var = `SARS CoV-2 N2`, # ..
           colour_var = NULL, # Colour points/lines by this variable
           shape_var = NULL, # Shape points by this variable
           
           # grouping_variables : If you want regressions within subsets of data
           grouping_var = NULL, # Does linear regression within each group
            
           
           # pivoting data into wide format
           # If data is already in wide format with targets in each column
           already_pivoted_data = 'no',
           
           # These variables will do the pivot_wider
           measure_var = 'Copies_Per_Liter_WW', # this is the value that will be plotted across variables
           text_cols = minimal_label_columns,
           
           # Plot labels and text
           title_text = title_name,
           
           show_y.equals.x_line = 'yes',
           text_for_equation = 'Rsquare', # choice: "Rsquare" or "full equation"
           
           
           # filtering variables
           sample_filtering_var = str_c(samples_to_remove, '|NTC|Vaccine'), # filtering Sample_name column
           exclude_sample = T,
           WWTP_filtering_var = '.*', exclude_WWTP = F # filtering the WWTP column
  )

{ # Convenient handle for repetitive plotting in the same format; 
  
  
  # Preliminaries ----
  
  
  # convert column names (target names) into string
  checkx <- strx <- paste(substitute(x_var)) 
  checky <- stry <- paste(substitute(y_var))
  
  modx <- function(x) x # unimplemented feature: modifier in case this function gets transformed variables
  
  
  # check and pivot data ----
  {if(already_pivoted_data == 'no')
  {  # filtering data for plotting according to function inputs
    .data_for_plot <- .data %>% 
      
      # select relevant columns - need to ensure samples don't repeat
      select(all_of(text_cols), any_of('biological_replicates'), all_of(measure_var)) %>% 
      
      # subsetting data with filter
      filter(
        .,
        
        # filter Sample_name column
        if('Sample_name' %in% colnames(.)) { # filtering only if the column exists
          str_detect(`Sample_name`, sample_filtering_var, negate = exclude_sample) }else TRUE,
        
        # filter WWTP column
        if('WWTP' %in% colnames(.)) {
          str_detect(WWTP, WWTP_filtering_var, negate = exclude_WWTP)} else TRUE 
        
      ) %>% # Useful to remove NTCs/STDs etc that could be non-unique and cause problems
      
      
      # pivoting data into wide format - put targets next to each other
      pivot_wider(names_from = 'Target_Name', values_from = all_of(measure_var)) %>% # Target can be generalized?
      ungroup() # Needs to be ungroup so that linear regression works for all samples together
  
  } else .data_for_plot <- .data } %>%  # direct carrying of data to next steps
    
    # {if(!is.null(grouping_var)) group_by(., {{grouping_var}}) else . } 
    # is.null will evaluate it, so it doesn't work this way
    
    group_by(., {{grouping_var}})  
    
  # DISABLED FOR CHECKING IF PLOTLY RUNS (GROUPS NOT WORKING RIGHT NOW
  
  
  
  # error handling ----
  
  # If plotting transformations of variables : Not fully implemented yet
  if(enexpr(x_var) %>% is.call()) {checkx <-  enexpr(x_var)[2] %>% paste(); }# modx <- eval(enexpr(x_var)[1])}
  if(enexpr(y_var) %>% is.call()) checky <-  enexpr(y_var)[2] %>% paste()
  
  # If X/Y vars are not present in the data, stop running and print a message
  if(.data_for_plot %>% names() %>% 
     {checkx %in% . & checky %in% . } %>% 
     !.) return('X or Y axis values for this scatterplot are not present in the data frame provided. 
Check if x_var and y_var are present in .data')
  
  # If duplicates of data exist in the data, stop running and print a message
  if((.data_for_plot %>% select(all_of(c(checkx, checky))) %>% 
      map_lgl(~class(.) == 'numeric') %>% 
      sum()) < 2) 
  { duplicated_data_points <- .data_for_plot %>% 
    filter(map({{x_var}}, length) > 1)
  print(duplicated_data_points)
  
  return('Repeated data instances for the same WWTP found in this scatterplot')
  }
  
  
  
  # Linear regression ----
  
  # Making linear regression formula (wrapper below the script)
  
  # fmla <- new_formula(enexpr(y_var), enexpr(x_var))   # does not work 
  fmla <- as.formula(paste(substitute(y_var), # make a formula y ~ x
                           '~',
                           substitute(x_var),
                           collapse = ' '))

  
  # Max and ranges for plotting
  
  # determine the optimal values for drawing y == x line
  xyeq <- .data_for_plot %>% 
    summarise(across(where(is.numeric), max, na.rm = T)) %>% # identify the maximum values among y and x axis
    
    ungroup() %>% # need to ungroup data
    select(all_of(c(checkx, checky))) %>% min() %>% {.*0.9} %>% # record 90% of the min among the y and x axes
    modx()
  
  # linear regression equation
  lin_reg_eqn <- .data_for_plot %>% 
    mutate(across(all_of(c(checkx, checky)), ~if_else(.x == 0, NaN, .x))) %>% # remove zero variables (to prevent errors logscale?)
    
    lm(fmla, data = ., na.action = na.exclude) %>% lm_eqn(., trig = text_for_equation)
  
  
  
  
  # plotting part ----
  
  plt1 <- .data_for_plot %>% 
    ggplot(aes(x = {{x_var}}, y =  {{y_var}} )) +
    geom_point(size = 2, mapping = aes(colour = {{colour_var}}, shape = {{shape_var}})) +
    
    # linear regression
    geom_smooth(method = 'lm', 
                mapping = aes(group = {{grouping_var}}, # does linear regression within group
                              colour = as.factor({{grouping_var}}))) +  # plot as discrete colours
    
    geom_text(data = . %>% summarise(across(where(is.numeric), max, na.rm = T) ),
              mapping = aes(group = {{grouping_var}}),
              
              label = lin_reg_eqn, parse = TRUE, show.legend = F, hjust = 'inward', nudge_x = -5) +
    
    # Dummy y = x line
    {if(show_y.equals.x_line == 'yes') list(geom_abline(slope = 1, intercept = 0, alpha = .4),
                                            annotate(geom = 'text', x = xyeq, y = xyeq, label = 'y = x', alpha = .3))
    } +
    
    # Labeling
    ggtitle(title_text, subtitle = measure_var)
}


# formula function ----

# wrapper for making formula (y ~ x)
# Making linear regression formula (source: https://stackoverflow.com/a/50054285/9049673)
# does not work 

new_formula <- function(y1, x1)
{
  flma <- as.formula(paste(enexpr(y1), "~", enexpr(x1), collapse = ' '))  
  
}
