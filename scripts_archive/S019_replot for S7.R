# S019_replot for S7.R

source('general_functions_plate_reading.R') # source the file that contains pre-requisite functions
library(patchwork) # library for combining plots

# sample name translation
translate_samples <- c('pPK7_32' = 'M1',
                       'pPK7_6' = 'M2',
                       'pPK7_17' = 'M3',
                       'pPK7_28' = 'M4',
                       'pPK7_21' = 'M5',
                       
                       'pRV01 \\+ rGFP' = 'Wild Type',
                       'rGFP' = 'Int(-)',
                       'MG1655' = 'E.coli'
)

# Read in data ----
processed.data <- # reading in only d2 ui and i data
  str_c('plate reader data/processed/', 'S019_6-11-19', '-processed.csv') %>% 
  read_csv() 

# ran run_single_plate.R to get this data with user_inputs : sheet_name <- 2 to read the first 2 sheets of d2


# Processing ----

sel.data <- filter(processed.data, str_detect(Samples, 'RV01 $|rG|MG|7_21|7_6|7_28|7_32|7_17')) %>% 
  
  mutate(across(Samples, ~ str_replace_all(.x, translate_samples))) %>%  # Change to more readable sample names
  mutate(time = str_extract(sheet_ID, '.*(?= d)') %>% as.numeric, .after = sheet_ID, .keep = 'unused') %>%  # extract day ("n d" format)

  mutate(across(Samples, ~ fct_relevel(.x, 'Wild Type'))) # order : bring WT to the beginning  


# plots ----

plt.timeseries <-
  
  c('Wild', '^M') %>% # make one plot for Wild type and another for mutants
  
  map(
    ~ filter(sel.data, str_detect(Samples, .x)) %>% # remove empty E. coli, C- : Int(-) and Ara : pInt8
      
      {ggplot(., aes(x = time, y = `GFP/OD`, shape = as_factor(Inducer))) + # `GFP/OD`
          
          # make lines, then points
          geom_line(aes(alpha = as_factor(Inducer),
                        group = interaction(Inducer, replicate))) +
          geom_point(size = 1, fill = 'white') + # plot points above the lines
          
          
          # control shapes, line transparency and x axis line breaks
          scale_shape_manual(name = 'C12-AHL', values = c(21, 19), labels = c('0 uM', '1 uM')) + # set shape 1 = inducer 0 and 19 = inducer 1 uM ;
          scale_alpha_discrete(guide = 'none', range = c(0.2, 0.5)) + # control line transparency
          scale_x_continuous(breaks = c(0, 2, 5, 8, 10)) + # x axis numbers
          
          # induction windows
          annotate('rect', xmin = 0, ymin = 0, xmax = 0.25, ymax = Inf, alpha = .3) +
          annotate('rect', xmin = 8, ymin = 0, xmax = 9, ymax = Inf, alpha = .2) +
          
          # facets and axis labels
          facet_wrap(facets = vars(Samples), ncol = 2) +
          xlab('Time (days)') + # add x and y axis labels
          ylab('GFP/OD (a.u.)')
        
      }
  )

plt.assembl <- 
  {(plt.timeseries[[1]] + plot_spacer()) / 
      plt.timeseries[[2]] + 
      plot_layout(heights = c(1, 3), guides = 'collect') } %>% 
  
  print

ggsave('plate reader analysis/plots and data/S019_timecourse_replicates.pdf', width = 4, height = 6)
# ggsave('plate reader analysis/plots and data/archive/S019_timecourse_replicates.png', width = 5, height = 6)
