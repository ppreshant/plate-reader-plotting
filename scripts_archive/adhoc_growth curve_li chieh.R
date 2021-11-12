# plotting Li Chieh's continuous growth data

# Prelims ----

source('./general_functions_plate_reading.R') # source the file that contains all the functions

# User inputs ----

# User inputs: 1. Enter name of the excel file, 2. Name of the data sheet(S) 3. number of rows and columns in plate reader data 4. Title for plots #comment (file name starts in the previous directory of this Rproject)
flnm <- '2021_11_01_merged'

# Note: The script only works for SPARK files where OD, GFP and RFP are read, 
# if you leave out anything b_gap needs to be changed accordingly
sheet_name <- 'default'

title_name <- flnm # appears on the html file name and header of selected plots, change as required

# naming ----

# regular expression based conversion of well names to descriptive names
well.to.names_translation <- c('^(A|H).*' = 'media only',
                               '^(B|C).*' = 'empty vector' ,
                               '^(D|E).*' = 'gfp',
                               '^(F|G).*' = 'Ribozyme') # informative_name -> c('assay_variables' ..)


# Input data ----

flpath <- str_c('../plate reader data/',flnm,'.csv')

## this is sleight of hand
fl <- merged_outliers.removed %>%  # grabbing data from the other file
  select(-`Temp. [°C]`)

# fl <- read_csv(path = flpath) and transpose etc like below
# old stuff --- fl <- readxl::read_excel(path = flpath, col_names = F, skip = 49,range = 'A50:JY148') %>%  # load the results sheet
  # 
  # # transpose the data and clean a little
  # pivot_longer(-`...1`) %>% 
  # pivot_wider(names_from = `...1`, values_from = value) %>% 
  # select(-name, -`Temp. [°C]`) # remove useless columns

# Processing ----

proc.dat = fl %>% 
  pivot_longer(-c(1,2), names_to = 'Well') %>% 
  mutate(Sample_name = str_replace_all(Well, well.to.names_translation),
         across(c(value, `Time [s]`), as.numeric)) %>% 
  rename(OD600 = value) %>% 
  group_by(Sample_name) %>% 
  mutate(replicate = row_number()) %>% 
  mutate('Time (hr)' = `Time [s]`/(60 * 60)) %>% # convert to hours

  
  group_by(Sample_name, `Time (hr)`) %>% 
  mutate(across(OD600,
                lst(mean = ~ mean(.x, na.rm = T),
                    stdev = ~ sd(.x, na.rm = T) )
                )
        )

# plotting ----

plt.raw <- ggplot(proc.dat, 
       aes(x = `Time [s]`, y = OD600,
           colour = Sample_name,
           group = Well)) +
  geom_point(alpha = 0.4, size = .5) +
  geom_line(alpha = 0.4) +
    
  ggtitle('Raw data of growth curves')

plt.summary <- {ggplot(proc.dat, 
                       aes(x = `Time (hr)`, y = OD600_mean,
                           colour = Sample_name, fill = Sample_name,
                           group = Well)) +
    geom_point(alpha = 0.4, size = .5) +
    geom_line(alpha = 0.1) +
    geom_ribbon(aes(ymin = OD600_mean - OD600_stdev, 
                    ymax = OD600_mean + OD600_stdev),
                alpha = 0.01,
                show.legend = FALSE,
                linetype = 0) +
    
    ggtitle('Summary of growth curves : Run 2', 
            subtitle = 'Mean shown as the line, and standard deviation as ribbon') } %>% 
  print()


ggsave(str_c('plate reader analysis/plots and data/archive/', flnm, '-lines.png'), plot = plt.raw, width = 7, height = 4)
ggsave(str_c('plate reader analysis/plots and data/archive/', flnm, '-summary.png'), plot = plt.summary, width = 7, height = 4)
