# 5-formatting_plots.R

# formatting plots ----

# colour palettes

# Bang Wong palette -- https://davidmathlogic.com/colorblind/
bang_wong_palette <- c('000000', 'E69F00', '56B4E9', '009E73', 
                       # 'F0E442', # lots of colours do not work in R
                       '0072B2' #, 
                       # 'D55E00', 
                       # 'CC79A7'
                       )
   
# Set theme universally : format as classic, colours = Set1
theme_set(theme_classic()) # theme
scale_colour_discrete <- function(...) { # colour palette
  
  scale_colour_brewer(..., palette="Dark2") # choose Dark2 if there are > 4 categories to avoid light yellow
  
  # scico::scale_color_scico_d(palette = 'batlow') # ugly but scientific colour scales
  # scale_colour_manual(values = bang_wong_palette) # some colours don't work
}

scale_fill_discrete <- function(...) { # fill palette
  scale_fill_brewer(..., palette="Dark2") # choose Dark2 if there are > 4 categories to avoid light yellow
  
  }



# formatting labels in logscale cleanly : a x 10^b
# use as ggplot(df,aes(x,y)) + geom_point() + scale_y_log10(labels = fancy_scientific)
fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # remove + after exponent, if exists. E.g.: (3x10^+2 -> 3x10^2) 
  l <- gsub("e\\+","e",l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # convert 1x10^ or 1.000x10^ -> 10^ 
  l <- gsub("\\'1[\\.0]*\\'\\%\\*\\%", "", l)
  # return this as an expression
  parse(text=l)
}


# plot formatting function : format as classic, colours = Set1
format_classic <- function(plt)
{ # formats plot as classic, with colour palette Set1, centred title, angled x axis labels
  plt <- plt +
    theme_classic() + scale_color_brewer(palette="Set1") + scale_fill_brewer(palette="Set1") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .3))
}

# plot formatting function : format as logscale
format_logscale_y <- function(plt)
{ # extra comments
  plt <- plt +
    scale_y_log10(  # logscale for y axis with tick marks
      labels = fancy_scientific
    )
}

format_logscale_x <- function(plt)
{ # extra comments
  plt <- plt +
    scale_x_log10(  # logscale for x axis
      labels = fancy_scientific
      # depreceated : labels = scales::trans_format("log10", scales::math_format(10^.x) )
    )
}