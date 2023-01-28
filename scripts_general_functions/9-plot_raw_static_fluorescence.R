# Plotting convenience function for static fluorescence plots : Not very generalized

## WORK IN PROGRESS

plot_raw_static_fluorescence <- function(.fluor_variable)
{
  # Parse exprs ----
  
  # Make an expression for fluor variable with "_mean"
  fluor_w_mean_expr <- rlang::parse_expr(str_c(rlang::expr_text(enexpr(.fluor_variable)), '_mean'))
  fluor_str <- rlang::expr_text(enexpr(.fluor_variable))
  
  
  # process data ---- 
  
  # process data into order (plots in descending order)
  fluor_order_processed.data <- processed.data %>% 
    arrange({{.fluor_variable}}) %>% 
    
    ungroup() %>% 
    mutate(across(Samples, fct_inorder))
  
  # Select dataset of only mean samples
  fluor_unique_mean <- fluor_order_processed.data %>% 
    select(Samples, contains('mean')) %>% 
    unique()
  
  
  # Plot ----
  
  # plot a horizontal bar graph, along with individual replicates and mean annotated
  plt_fluor <- ggplot(data = fluor_order_processed.data,
                    mapping = aes(y = Samples, x = {{.fluor_variable}})) + 
    geom_jitter(width = 0, height = .3) + # plot individual replicates
    
    
    # Indicate a light grey bar for the mean - to aid the eye
    geom_bar(data = fluor_unique_mean, 
             aes(x = {{fluor_w_mean_expr}}, y = Samples), alpha = 0.3, fill = 'gray', stat = 'identity',position = 'dodge') +
    # geom_segment(aes(x = {{fluor_w_mean_expr}}, xend = 0, yend = Samples), alpha = 0.1, colour = 'red') + 
    
    # Axis labels
    xlab(str_c(fluor_str, ' fluorescence', if(MEFL_normalization) ' (MEFL, uM)' else ' (a.u)')) + 
    ylab('') +
    
    # Plot title and subtitle
    ggtitle('Fluorescence measurement',
            subtitle = 'Plasmids')
  
  
  # Add mean and text labels ----
  
  {plt_fluor + 
      
      # plot mean as a vertical dash (for small point, use size = 0.5)
      geom_point(data = fluor_unique_mean, 
                 aes(x = {{fluor_w_mean_expr}}), size = 5, shape = '|') +
      
      # add a text label for quick reference of the mean
      geom_text(data = fluor_unique_mean,  
                mapping = aes(x = {{fluor_w_mean_expr}}, label = {{fluor_w_mean_expr}} %>% round, 
                              hjust = if_else({{fluor_w_mean_expr}} > max({{fluor_w_mean_expr}})/2, 1.3, -0.3)))
    } %>% 
    print()
  
  # ggsave(str_c('./plots and data/', title_name, '.png'), width = 5, height = 4) # save plot if necessary
  
  
  # interactive plot ---- 
  
  plotly::ggplotly(plt_fluor) %>% plotly::style(textposition = 'top right')
  
  
  
}