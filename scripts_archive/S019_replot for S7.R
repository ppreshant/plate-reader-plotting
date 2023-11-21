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
  
  mutate(time = str_extract(sheet_ID, '.*(?= d)') %>% as.numeric, .after = sheet_ID, .keep = 'unused') # extract day ("n d" format)


# plots ----

# filter(sel.data, str_detect(Samples, 'coli')) %>% 
sel.data %>% 
  
  {ggplot(., aes(x = time, y = `GFP/OD`, shape = as_factor(Inducer))) + # `GFP/OD`
      geom_point(size = 1) +
      geom_line(aes(alpha = as_factor(Inducer),
                    group = interaction(Inducer, replicate))) +
      # geom_boxplot(aes(y = `GFP/OD_mean`), position = position_identity(), size = 0.1, show.legend = F) + # show means as a line
      
      scale_shape_manual(name = 'C12-AHL', values = c(1, 19), labels = c('0', '1 uM')) + # set shape 1 = inducer 0 and 19 = inducer 1 uM ;
      scale_alpha_discrete(guide = 'none', range = c(0.2, 0.5)) + # control line transparency
      
      facet_wrap(facets = vars(Samples), scales = 'free_y') #+
      
      # ylab('GFP/OD (a.u.)') #+ # add y axis label 
      
      # 
      # geom_hline(yintercept = WT_baseline, linetype = 2, alpha = .5) +  # show baseline of WT negative
      # geom_hline(yintercept = rGFP_baseline, linetype = 2, alpha = .5)  # show baseline of negative samples
  }

# ggsave('plate reader analysis/plots and data/S018_top5_replicates.pdf', width = 5, height = 3)
