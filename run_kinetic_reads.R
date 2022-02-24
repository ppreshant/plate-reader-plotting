# General(ish) script to read and plot continuous growth data
# Prashant, 24/Nov/21

# Prelims ----

source('./general_functions_plate_reading.R') # source the file that contains all the functions

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) 3. number of rows and columns in plate reader data 4. Title for plots #comment (file name starts in the previous directory of this Rproject)
flnm <- 'example kinetic-E01.4-Vmax Ribo fluor kinetic_28-11-20'

# Note: This script only works for SPARK files where OD; and optionally GFP and RFP are read 

title_name <- flnm # appears on the html file name and header of selected plots, change as required

# naming ----

# regular expression based conversion of well names to descriptive names
# well.to.names_translation <- c('^(A).*' = 'Ribozyme',
#                                '^(C).*' = 'empty vector' ,
#                                '^(E).*' = 'gfp',
#                                '^(G).*' = 'media only') # format: c('well key' = informative_name , ..)

# choose naming - colour order: Green, orange, blue, Pink
# naming_order <- c('gfp', 'empty vector', 'media only', 'Ribozyme') # order of the naming


# Input data ----

flpath <- str_c('plate reader data/',flnm,'.xlsx')

# grabbing read data + metadata from raw excel file
fl <- parse_continuous_growth(flpath)


# Processing ----
# can drop all NA well columns using fl[!map_lgl(fl, ~ all(is.na(.x)))] 

# rename data and metadata columns with standardized names
colnames(fl) <- 
  regex(measurement.labels_translation, ignore_case = T) %>% 
  {str_replace_all(colnames(fl), .)}

data_columns <- c('OD', 'GFP', 'RFP')

proc.dat = fl %>% 
  
  # remove the unassigned samples
  filter(!is.na(Samples)) %>% 
  
  # remove unnecessary columns
  select(-row, -column) %>% 
  
  # order of colours for plotting
  # mutate(across(Samples, ~ fct_relevel(.x, naming_order))) %>% 
  
  group_by(Samples) %>% 
  mutate(replicate = row_number()) %>% 
  mutate('Time (h)' = round(Time/(60 * 60), 2) ) %>% # convert to hours, round to 2 decimals
  
  group_by(Samples, `Time (h)`) %>% 
  mutate(across(any_of(data_columns),
                lst(mean = ~ mean(.x, na.rm = T),
                    stdev = ~ sd(.x, na.rm = T) )
  )
  )



# plotting ----

# plot the timeseries of each replicate well
plt.raw <- {ggplot(proc.dat, 
                  aes(x = `Time (h)`, y = OD,
                      colour = Samples,
                      group = well)) +
  geom_point(alpha = 0.4, size = .5) + # make plot with points and lines
  geom_line(alpha = 0.4) +
  
  ggtitle(title_name,
          subtitle = 'Each line shows a single biological replicate')} %>% 
  
  print()

# plot the mean of replicates and a light ribbon for standard deviation
plt.summary <- {ggplot(proc.dat, 
                       aes(x = `Time (h)`, y = OD_mean,
                           colour = Samples, fill = Samples,
                           group = well)) +
    geom_point(alpha = 0.4, size = .5) +
    geom_line(alpha = 0.1) +
    geom_ribbon(aes(ymin = OD_mean - OD_stdev, 
                    ymax = OD_mean + OD_stdev),
                alpha = 0.05,
                show.legend = FALSE,
                linetype = 0) +
    
    
    ggtitle(str_c('Summary of growth curves: ', title_name), 
            subtitle = 'Mean shown as the line, and standard deviation as ribbon') } %>% 
  print()


# switch for plotting for other fluorescence data
if('GFP' %in% colnames(proc.dat)) gfpplots <- TRUE else gfpplots <- FALSE


# add direct labels to the plot for easy visualization
plt.summary.directlabels <- directlabels::direct.label(plt.summary) %>% print()

# optional saving plots
# ggsave(str_c('plate reader analysis/plots and data/archive/', title_name, '-lines.png'), plot = plt.raw, width = 7, height = 4)
# ggsave(str_c('plate reader analysis/plots and data/archive/', title_name, '-summary.png'), plot = plt.summary.directlabels, width = 7, height = 4)

# calling r markdown file for plotting in a nice format html file
rmarkdown::render('kinetic_plotting_html.Rmd', 
                  output_file = str_c('plate reader analysis/html files/', title_name, '.html'))



# Save data ----

# save data in the 'processed' folder
write.csv(proc.dat, 
          str_c('plate reader data/processed/', flnm, '-processed.csv', 
                na = ''))