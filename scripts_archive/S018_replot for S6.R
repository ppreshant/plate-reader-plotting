# S018_replot for S6.R

# run_single_plate.R on 'S018_pPK6-7 selected 14_18-7-19' with sample_name = 2; till line 55

# sample name translation
translate_samples <- c('pPK7_32' = 'M1',
                       'pPK7_6' = 'M2',
                       'pPK7_17' = 'M3',
                       'pPK7_28' = 'M4',
                       'pPK7_21' = 'M5',
                       
                       'pRV01 \\+ rGFP' = 'WT',
                       'rGFP' = 'Int(-)',
                       'MG1655' = 'E.coli'
                       )


# subset data
sel.data <- filter(processed.data, str_detect(Samples, 'RV01 $|rG|MG|7_21|7_6|7_28|7_32|7_17')) %>% 
  mutate(across(Samples, ~ str_replace_all(.x, translate_samples)))
