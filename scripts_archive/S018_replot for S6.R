# S018_replot for S6.R

source('general_functions_plate_reading.R') # source the file that contains pre-requisite functions
library(patchwork) # library for combining plots

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


# plot signal ----

plt_sig <- 
  {ggplot(sel.data, aes(x = Samples, y = `GFP/OD`, shape = as_factor(Inducer))) + 
      geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) + # jittered points
      geom_boxplot(aes(y = `GFP/OD_mean`), position = position_identity(), size = 0.1, show.legend = F) + # show means as a line
      
      scale_shape_manual(name = 'C12-AHL', values = c(1, 19), labels = c('0', '1 uM')) + # set shape 1 = inducer 0 and 19 = inducer 1 uM ;
      
      facet_grid(cols = vars(sample_category), scales = 'free_x') +
      
      ylab('GFP/OD (a.u.)') + # add y axis label 
      
      
      geom_hline(yintercept = WT_baseline, linetype = 2, alpha = .5) +  # show baseline of WT negative
      geom_hline(yintercept = rGFP_baseline, linetype = 2, alpha = .5)} %>%  # show baseline of negative samples
  print

ggsave('plate reader analysis/plots and data/S018_top5_replicates.pdf', width = 5, height = 3)

# Zoom into uninduced of mutants
sig_zoom <- {plt_sig + 
    
    ylim(0, 100) + # set axis limit
    ylab(NULL) + xlab(NULL) + # remove axes labels
    
    theme_gray() + 
    theme(
      legend.position = 'none', # remove legend
      
      strip.background = element_blank(), # remove facets
      strip.text.x = element_blank(),
      
      axis.text.y = element_text(size = 8), # reduce text size
      axis.text.x = element_text(size = 5)
      
    ) } %>% print


# create inset plot

plt_sig + inset_element(sig_zoom, 0.5, 0.5, 1, 1)

ggsave('plate reader analysis/plots and data/S018_top5_replicates_inset.pdf', width = 5, height = 3)

# fold change calc ----


foldchange <- sel.data %>% 
  select(Samples, sample_category, Inducer, replicate, `GFP/OD`) %>% 
  
  pivot_wider(names_from = Inducer, values_from = `GFP/OD`) %>% # separate induction into 2 columns
  mutate('Fold change' = `1`/`0`, Inducer = 'none') %>% # make fold change (named : ratio)
  mutate(fc_mean = mean(`Fold change`), .by = Samples)


# plot fold change ----

plt_fold <- 
  {ggplot(foldchange, aes(x = Samples, y = `Fold change`)) + 
      geom_point(position = position_jitter(width = 0.2, height = 0), size = 1) + # jittered points
      geom_boxplot(aes(y = fc_mean), position = position_identity(), size = 0.1, show.legend = F) + # show means as a line
      
      facet_grid(cols = vars(sample_category), scales = 'free_x')} %>% print

ggsave('plate reader analysis/plots and data/S018_top5_fold_replicates.pdf', width = 5, height = 3)
