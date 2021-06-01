# 4-Plotting_fns.R


# plotting function : to reduce redundancy, common elements are captured here
plot_mean_facetted <- function(sel_tablex)
{ # Input the filtered summary table and plot the mean vs sample points with facetting and title
  plt1 <- ggplot(sel_tablex, aes(Samples, mean, colour = Time, shape = Inducer)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25) + geom_point(size = 2, fill = 'white') + facet_grid(~ category, scales = 'free_x', space = 'free_x') + scale_shape_manual(values = c(21,19)) + ggtitle('RBS mutants selected')
}

# plotting dose response (mean vs Inducer)
plot_dose_response <- function(sel_tablex, y_axis_label = 'GFP/OD (a.u.)', plot_title = 'Flipping vs inducer dose' )
{ # Input the filtered summary table and plot the mean vs Inducer points and errorbars. facet by Samples, colour by sample and title
  plt1 <- ggplot(sel_tablex, aes(Inducer, mean, colour = Reporter)) + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.1) + 
    geom_point(size = 2) + 
    facet_grid(~ category, scales = 'free_x', space = 'free_x') +
    ylab(y_axis_label) + ggtitle(plot_title) 
  
  format_classic(plt1) %>% format_logscale_x # output a classic formatted plot with logscale x
}