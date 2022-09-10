# Collates multiple plate reader processed csv files, comparitive analysis and makes plots 
# Author : Prashant K, Date: 29/7/22

# Plotting repression data of fusion constructs mSc-U64 for presentation
# script copied over from qPCR : ramfacs_2B_repression fusions.R file

source('general_functions_plate_reading.R') # source the file that contains all the functions

# User inputs  ----
# choose file name, title for plots (file name starts in the same directory as Rproject)

flnms <- c('S044_new fusions_8-4-22')
# 'S037-2_48,51_repression red_9-2-22', # This is not MEFL normalized :( Manually retrieving from another gain 100 data S038


title_name <- 'Fusion : U64 RAM and fluorescence'

# name translators ----

sample_label_translation <- c('76' = 'Ribozyme',    # 'current name' = 'new name' format
                           '48' = 'mScarlet',  # regex based translation to change x-axis labels
                           
                           '51' = 'Ribozyme-mScarlet',
                           '77' = 'mScarlet-Ribozyme v2',
                           '79' = 'mScarlet-Ribozyme',
                           
                           '67' = '', # remove extra information
                           ' \\+ ' = '', # the special symbol '+' needs to be double escaped
                           'NTC' = 'ntc')

# Assign categories to samples : for colouring in plots
sample_category_sorter <- c('.* \\+ .*' = 'Repressed', # special symbol needs to be espaced twice
                            '48|51|77|79' = 'Maximal',
                            'MG1655' = 'negative')


# Input the data ----

# reading in files and merge : along with the run ID
.df <- get_processed_datasets(flnms)


# Pre-process ----

# Get proxy MEFL data that is missing for S037-2 ; from S038 data that has same gain of 100
# Bug: S037-2 has gains: green: 90, red : 112 -- so 100 is technically inaccurate but ~~~ it is what it is

mfl.dat <- get_processed_datasets('S038_47+67_23-2-22')
mfl_find <- find_molecular_normalizers(mfl.dat) %>%  # use function to find the mefls
  add_column(gain = 100) %>% add_column(dataset = 'S038_47+67_23-2-22')
# write.csv(mfl_find, 'plate reader data/processed/MEFL_references.csv') # save for future use

S37_dat <- get_processed_datasets('S037-2_48,51_repression red_9-2-22') %>% 
  normalize_molecules_equivalent(.normalizer = mfl_find)

.df2 <- bind_rows(.df, S37_dat)


# Processing ----

# create a subset of data for presenting the most relevant data : 51, 77, 79 ; 48, 76 with repression and without
presentation.dat <- 

  filter(.df2,
         !str_detect(Samples, '^67$|Vmax|76')) %>%  # remove samples that are unimportant for presentation
  
  # create categories of data based on the Sample
  mutate(Sample_category = str_replace_all(Samples, sample_category_sorter)) %>%
  
  mutate('sample_label' = str_replace_all(Samples, sample_label_translation)) %>%  # assign labels for readability
  
  # arrange for plotting
  mutate(across(Sample_category, ~ fct_relevel(.x, 'Maximal', 'Repressed', 'negative'))) %>%  # set the order of the colours r, b, g
  mutate(across(sample_label, ~ fct_reorder(.x, `RFP/OD`))) %>%  # descending order of copies
  mutate(across(sample_label, ~ fct_relevel(.x, 'MG1655'))) # Take MG1655 to the end -- first factor is being plotted closes to origin..

presentation_mean.dat <- group_by(presentation.dat, 
                                  Sample_category, sample_label) %>% 
  summarize('RFP/OD' = mean(`RFP/OD`, na.rm = TRUE))

# Plotting ----

# plot for presentation -- all fusions
plt_red <- ggplot(data = presentation.dat,
                  aes(y = sample_label, x = `RFP/OD`, 
                      colour = Sample_category)) +
  
  geom_jitter(width = 0, height = .3) + # plot individual replicates
  
  # plot mean as a vertical dash (for small point, use size = 0.5)
  geom_point(data = presentation_mean.dat,
             aes(shape = if_else(`RFP/OD` > 1, 'line', 'nothing') ),
             size = 10, show.legend = FALSE) + 
  scale_shape_manual(values = c('line' = '|', 'nothing' = '')) +
  
  # Indicate a light grey bar for the mean - to aid the eye
  # geom_bar(data = presentation_mean.dat, 
  #          alpha = 0.3, stat = 'identity',position = 'dodge') +
  
    # Axis labels
  xlab('Fluorescence (MEFL, uM)') + ylab('') + 
  
  # Plot title and subtitle
  ggtitle(str_c('Fluorescence measurement', ': MEFL (uM)' ),
          subtitle = 'Plasmids') +
  
  theme(legend.position = 'top', legend.title= element_blank()) # stylistic editing of legend 

plt_red_annotated <- {plt_red + ggrepel::geom_text_repel(data = presentation_mean.dat,  
                                                      mapping = aes(label = `RFP/OD` %>% round, 
                                                                    hjust = if_else(`RFP/OD` > max(`RFP/OD`)/2, 1.3, -0.3)),
                                          show.legend = FALSE)
} %>% 
  print()


# Save plot
ggsave(plot_as('all fusions, U64-red'), width = 5, height = 3)
# ggsave('all fusions, U64-red.pdf', width = 5, height = 3) # save as pdf to fix overlapping labels manually
