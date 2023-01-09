# S058 plotting

# data polishing ----

# extrapolate inducer for d-1 to connect the point to initial day --- or would this be misleading?
# ideal would be to show a line but not the point


processed.data %<>% 
  separate(Samples, into = c('plasmid', 'organism'), remove = F) # split Samples a/b..

  # filter(day == -1) %>% view
# work in progress



# Plot ----

plt_panels <- ggplot(processed.data,
       aes(x = day, y = `RFP/OD`, colour = AHL_uM)) + 
  geom_point() + geom_line(aes(group = AHL_uM)) + 
  
  ggtitle(title_name) + 
  theme(legend.position = 'top') + 
  facet_wrap(facets = vars(Samples), ncol = 4)

# save plot 
ggsave(plot_as(title_name), plt_panels, width = 4, height = 6)


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
