# S018_replot for S6.R

# Read in data ----
processed.data <- # reading in only d2 ui and i data
  str_c('plate reader data/processed/', 'S018_pPK6-7 selected 14_18-7-19', '-processed.csv') %>% 
  read_csv()
# ran run_single_plate.R to get this data with user_inputs : sheet_name <- 2 to read the first 2 sheets of d2


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

# Processing ----

# subset data : WT, controls + top 5 mutants
sel.data <- filter(processed.data, str_detect(Samples, 'RV01 $|rG|MG|7_21|7_6|7_28|7_32|7_17')) %>% 
  mutate(across(Samples, ~ str_replace_all(.x, translate_samples))) %>% # Change to more readable sample names
  
  mutate(sample_category = if_else(str_detect(Samples, '^M'), 'Mutants', 'Controls')) # make sample categories for facetting
  

# find baselines for plotting
WT_baseline <- filter(sel.data, str_detect(Samples,'WT'), str_detect(Inducer,'0')) %>% # find the baseline : uninduced WT
  pull(`GFP/OD_mean`) %>% unique  

rGFP_baseline <- filter(sel.data, str_detect(Samples,'Int')) %>% # find the base value of negative control : Int(-) 
  pull(`GFP/OD_mean`) %>% mean # take mean of induced and uninduced samples (have a slight difference only)


# fold changes ----

# plot (copy from S018 branch?)

ggplot(sel.data, aes(x = Samples, y = `GFP/OD`, shape = as_factor(Inducer))) + 
  geom_point() + 
  scale_shape_manual(name = 'C12-AHL', values = c(1, 19), labels = c('0', '1 uM')) + # set shape 1 = inducer 0 and 19 = inducer 1 uM ;
  
  facet_grid(cols = vars(sample_category), scales = 'free_x') +
  
  ylab('GFP/OD (a.u.)') + # add y axis label 
  
  
  geom_hline(yintercept = WT_baseline, linetype = 2, alpha = .5) +  # show baseline of WT negative
  geom_hline(yintercept = rGFP_baseline, linetype = 2, alpha = .5) # show baseline of negative samples
  # ylim(0, 2500)



# top5_formatted <- {format_classic(plt_top5) + 


ggsave('plots and data/S018_top5.pdf', top5_formatted, width = 4, height = 3)