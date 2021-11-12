
# Read files ----

# file order
fl.order <- c('1_first', '1_second', '1_third', '2_fourth')
fl.prefix <- '2021_11_0'

# Read files to list
fl.list <- map2(fl.order, 
                c(0,1,1,1),
               ~ readxl::read_xlsx(path = str_c('plate reader data/', fl.prefix, .x, '.xlsx'),
                                   skip = 49 - .y,
                                   n_max = 148 - 49 - .y)
)

# check col.names
map(fl.list, ~ colnames(.x) %>% head)

# check last col
map(fl.list, ~ .x[[nrow(.x), 1]])


# processing ----

# find a typical time interval
t_int <- function(x) fl.list[[1]][1, x + 1] - fl.list[[1]][1, x] # just a function to get time interval

t_interval <- as.numeric(t_int(3)) # save the time interval

# transpose data
transposed.list <- map(fl.list, 
                       ~ t(.x) %>% # transpose
                         as_tibble %>% # convert to tibble
                         setNames(slice(., 1)) %>% # make 1st col as the colnames
                         slice(-1) %>%  # remove 1st col from the dataset
                         mutate(across(.fns = as.numeric)) # convert into numbers
)


# merge time of consecutive runs

merged_data <- transposed.list[[1]] # start with the first dataset 

for (i in 2:length(fl.order)) {
  
  current.data <- transposed.list[[i]]
  
  # Update the time with the last run + a typical time interval
  current.data$`Time [s]` <- current.data$`Time [s]` + t_interval + merged_data$`Time [s]` %>% tail(1)
  
  # merge to older data
  merged_data %<>% bind_rows(current.data)
  
}

# re-transpose the data for Li Chieh's dynamicGrabber attempt
merged_with.cycles <- mutate(merged_data, 'Cycle Nr.' = 1:n(), .before = 1)
merged_outliers.removed <- filter(merged_with.cycles,
                                  !`Cycle Nr.` %in% c(1,46, 52, 53) )

re_transposed <- 
  bind_cols(colnames(merged_outliers.removed),  # add the column names back in as the first column
          t(merged_outliers.removed) %>%  # re-transpose, make tibble
            as_tibble()) %>% 
  setNames(slice(., 1)) %>%  # make 1st row as col names
  slice(-1) # remove 1st row

write_csv(re_transposed, 
          file = str_c('plate reader data/', fl.prefix, '1_merged', '.csv'), 
          col_names = T)

# plotting ----

# plot first row for sanity check
plta1 <- ggplot(merged_outliers.removed,
       aes(x = `Time [s]`, y = B5, label = `Cycle Nr.`)) +
  geom_point() +
  geom_line()

ggplotly(plta1)

# manually inspected well A1, and decided the outliers to remove (without shaking ones)
# 1, 46, 52, 53