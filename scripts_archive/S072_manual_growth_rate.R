# S072_manual_growth_rate.R

# manually colleted data

# prelim ----
source('general_functions_plate_reading.R') # source the file that contains all the functions
source('0.5-user_inputs.R') # get user inputs


# Load data ----
flpath <- str_c('plate reader data/',flnm,'.xlsx')

processed_data <- readxl::read_xlsx(flpath, sheet = 'collated') %>% 
  
  pivot_longer(cols = contains('...'), names_to = 'samples', values_to = 'OD') %>% # pivot
  separate(samples, into = c('sample', NA), sep = '\\...') %>% # separate samples
  
  mutate(duration = as.numeric(difftime(timestamp, min(timestamp), units = 'hours'))) %>%  # make duration properly
  
  group_by(sample, duration) %>% mutate(replicate = row_number())


# plot ---

plt_od <- # see full data, decide linear range
ggplot(processed_data,
       aes(duration, log10(OD), colour = sample)) + 
  geom_point() + 
  geom_line(aes(group = interaction(sample, replicate)), alpha = 0.2)

ggplotly(plt_od)


# choose interval 2-5.5 h to get growth rate

# subset log phase ----

log_phase <- filter(processed_data, between(duration, 2, 5.5))

ggplot(log_phase,
       aes(duration, log10(OD), colour = sample)) + 
  geom_point() + 
  geom_smooth(method = 'lm')                    

# Growth rate calcs ----

calc_log_phase <- 
  log_phase %>% 
  group_by(sample, replicate) %>% nest() %>% # nest to to linear regression
  mutate(fit = map(data, ~ lm(log(OD) ~ duration, data = .x)),
         tidy = map(fit, broom::tidy),
         `growth rate` = map_dbl(tidy, ~ .[[2,2]]))


ggplot(calc_log_phase,
       aes(sample, `growth rate`)) + geom_point() +
  ggtitle('Growth burden of flipped plasmids', subtitle = title_name)

ggsave(plot_as(title_name, '-growth rates manual'), width = 3, height = 4)
