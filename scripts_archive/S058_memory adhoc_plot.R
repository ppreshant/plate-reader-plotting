# S058 plotting

# data polishing ----

# extrapolate inducer for d-1 to connect the point to initial day --- or would this be misleading?
# ideal would be to show a line but not the point
processed.data %>% 
  filter(day == -1) %>% view
# work in progress



# Plot ----

ggplot(processed.data,
       aes(x = day, y = `RFP/OD`, colour = AHL_uM)) + 
  geom_point() + geom_line(aes(group = AHL_uM)) + 
  
  ggtitle(title_name) + 
  theme(legend.position = 'top') + 
  facet_wrap(facets = vars(Samples), ncol = 4)

# save plot 
ggsave(plot_as(title_name), width = 4, height = 6)
