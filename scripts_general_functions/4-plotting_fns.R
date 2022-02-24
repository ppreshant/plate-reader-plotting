# 4-Plotting_fns.R

# Timeseries plots ----


# plot the timeseries of each replicate well 

plot_kinetic_raw <- function(.dataframe = proc.dat, 
                             y_variable = OD,
                             colour_variable = Samples)
{
  plt.raw <- {ggplot(proc.dat, 
                     aes(x = `Time (h)`, y = {{y_variable}},
                         colour = {{colour_variable}},
                         group = well)) +
      geom_point(alpha = 0.4, size = .5) + # make plot with points and lines
      geom_line(alpha = 0.4) +
      
      ggtitle(title_name,
              subtitle = 'Each line shows a single biological replicate')} %>% 
    
    print()
  
}


# plot the mean of replicates and a light ribbon for standard deviation

plot_kinetic_ribbon.summary <- function(.dataframe = proc.dat, 
                                y_variable = OD,
                                colour_variable = Samples)
  {
     y_w_mean <- add_prefix.suffix_expr(prefix = NULL, 
                                     .expr = !!enexpr(y_variable),
                                     suffix = 'mean')
  y_w_stdev <- add_prefix.suffix_expr(NULL, !!enexpr(y_variable), 'stdev')
  
    plt.summary <- ggplot(proc.dat, 
                           aes(x = `Time (h)`, y = {{y_w_mean}},
                               colour = {{colour_variable}}, fill = {{colour_variable}},
                               group = well)) +
        geom_point(alpha = 0.4, size = .5) +
        geom_line(alpha = 0.1) +
        geom_ribbon(aes(ymin = {{y_w_mean}} - {{y_w_stdev}}, 
                        ymax = {{y_w_mean}} + {{y_w_stdev}}),
                    alpha = 0.05,
                    show.legend = FALSE,
                    linetype = 0) +
        
        
        ggtitle(str_c('Summary of growth curves: ', title_name), 
                subtitle = 'Mean shown as the line, and standard deviation as ribbon')
    
  }

# General plots ----


# old funtions, not used here as the functions were re-written with specific features. 
# These will come in handy when generalizing stuff in the future

# plotting function : to reduce redundancy, common elements are captured here
plot_mean_facetted <- function(.data)
{ # Input the filtered summary table and plot the mean vs sample points with facetting and title
  plt1 <- ggplot(.data, aes(Samples, mean, colour = Time, shape = Inducer)) + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.25) + geom_point(size = 2, fill = 'white') + facet_grid(~ category, scales = 'free_x', space = 'free_x') + scale_shape_manual(values = c(21,19)) + ggtitle('RBS mutants selected')
}



# plotting dose response (y_axis_variable vs Inducer)
# Plots the raw data vs Inducer points and the mean too. facet by Samples, colour by sample and title

plot_dose_response <- function(.data, 
                               
                               y_var = `GFP/OD_bs`,
                               
                               y_axis_label = 'GFP/OD (a.u.)', 
                               plot_title = 'Response vs inducer dose')
{ 
  
  y_var.w.mean <- sym(rlang::as_string(ensym(y_var)) %>% str_c('_mean')) # create an expression for 'y_var'*_mean*
  
  
  plt1 <- ggplot(.data, aes(Inducer, {{y_var}}, colour = Reporter)) + 
    
    geom_jitter(height = 0) + # points for individual replicates
    
    geom_point(aes(y = !!y_var.w.mean), # plots mean as a line
                data = . %>% select(ends_with('mean') %>% unique),
               shape = '-', size = 5,
               show.legend = FALSE) +
  
    facet_grid(~ Samples, scales = 'free_x', space = 'free_x') +
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

