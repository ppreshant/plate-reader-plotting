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
  
  plt1 %>% format_logscale_x # output a classic formatted plot with logscale x
}


# untested; copied from S022
# plotting timeseries (mean in points, stdev in errorbars; Coloured by reporter plasmid, facetted by integrase plasmid and shape as inducer)
plot_time_series <- function(data_table, induction_duration = c(0,1), x_breaks = c(0,6,24,48), stroke_width = 1, x_axis_label = 'Time (days)', y_axis_label = 'GFP/OD (a.u.)', plot_title = 'AHL flipping with time' )
{
  plt <- ggplot(data_table, aes(Time, mean, colour = Inducer, shape = Reporter)) + 
    annotate('rect', xmin = induction_duration[1], ymin = 0, xmax = induction_duration[2], ymax = Inf, alpha = .2) +  # grey rectangle for induction duration
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.05) + facet_wrap(~ Samples) + geom_line() + geom_point(size = 2, fill = 'white', stroke = stroke_width) + 
    scale_x_continuous(breaks = x_breaks) + 
    ylab(y_axis_label) + xlab(x_axis_label) + ggtitle(plot_title)
  
  format_classic(plt) # output a classic formatted plot
}

