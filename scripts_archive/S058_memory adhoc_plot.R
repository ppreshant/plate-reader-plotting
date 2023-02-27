# S058 plotting

# Run `run_single_plate.R till processed.data or load from the processed.data csv file`



# data polishing ----

# extrapolate inducer for d-1 to connect the point to initial day --- or would this be misleading?
# ideal would be to show a line but not the point


processed.data %<>% 
  separate(Samples, into = c('plasmid', 'organism'), remove = F) # split Samples a/b..
  # mutate(across(day, as.numeric)) # convert day into numeric

  # filter(day == -1) %>% view
# work in progress



# Plot ----

plt_panels <- ggplot(processed.data,
       aes(x = day, y = `RFP/OD`, colour = AHL_uM)) + 
  geom_point() + geom_line(aes(group = AHL_uM)) + 
  annotate(geom = 'rect', xmin = 1, xmax = 2, ymin = -Inf, ymax = Inf, alpha = 0.2) + # show induction, day is factor
  
  ggtitle(title_name) + 
  theme(legend.position = 'top') + 
  facet_wrap(facets = vars(Samples), ncol = 4, scales = 'free_y') # zoom within every plot 'free_y' scale

# save plot 
ggsave(plot_as(title_name, '-zoomed'), plt_panels, width = 6, height = 6) # increase widthe 4 -> 6 for 'free_y'


# combine plot
plot_condensed <- 
  ggplot(filter(processed.data, str_detect(plasmid, '143|79')), # select only fluorescent plasmids
       aes(x = day, y = `RFP/OD`, colour = organism, shape = AHL_uM)) + 
  geom_point() + geom_line(aes(group = interaction(AHL_uM, organism))) +
  scale_shape_manual(values = c(1, 16)) + 
  
  ggtitle(title_name) + 
  # theme(legend.position = 'top') + 
  facet_wrap(facets = vars(plasmid), scales = 'free')


# save plot 
ggsave(plot_as(title_name, '-condensed'), plot_condensed, width = 4, height = 4)
