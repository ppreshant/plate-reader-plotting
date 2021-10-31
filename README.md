# plate-reader-plotting/basic_plots
Automated analysis, plotting of fluorescences/OD from **single-read/static** plate reader excel files.

_Tested on Tecan Spark and Infinite M1000 plate readers, other files would need minor code modifications_

## Processing workflow

1. Takes plate reader output excel file (`.xlsx`)
   - The excel file should be annotated with **sample names** and any other parameters such as **inducer concentrations** (if any) next to the OD data table (see example excel files). See below for other kinds of data plots
   - <img src="https://user-images.githubusercontent.com/14856479/139570241-ba36b10d-04cf-4e8b-b659-6ae84f04dffd.png" width="500">
  
   - _other parameters are read into the data file but not plotted, you can easily customize the plotting functions in `plate_reader_plotting_and_html.Rmd` file to add colours/shapes/facets based on the extra metadata you provide_
   - Processing multiple sheets in .xlsx is possible, for example a multi-day experiment. Check the branch `Time_series_master` for this feature. 
2. Program reads the OD, GFP and RFP. OD should always be first and any fluorescences in any order after the OD. 
   - *Please label the measurements with OD and the name of the fluorophores in the Tecan/Infinite plate reader protocol file before acquiring the data, these will appear in the plots* 
3. Data is processed: Autofluorescence is subtracted, Fluorescence/OD ratio is calculated for each fluorophore.
   - Sample named by common E.coli strains, 'MG1655', 'DH10B' or 'PBS' is assumed to be the background. Edit the variable `X`, if your base strain is different.
4. Two pre-formatted plots are made and saved into a HTML file (for easy access and presentation) for GFP only and GFP/OD, RFP/OD. _Plots show each replicate data as points and the mean using lines and a light grey bar to aid the eye._
   - <img src="https://user-images.githubusercontent.com/14856479/139571344-f2f0c1c9-9b5b-40ba-8e9d-49997b1b35fb.png" width="500">

5. A couple of interactive plots are also saved into the HTML for exploratory data analysis. 
   - Interactivity using *plotly* is quite powerful with features of zoom, showing subsets of data etc. You are always welcome to add more features using more advanced plotly functions by building the plot from scratch in plotly instead of ggplot2  

## Git organization
1. There are different master branches for each of the major kinds of experiments. These are older scripts and will be merged into the current branch eventually -
   - Time_series_master (ex: S015c) : reads multiple sheets in the same .xlsx file for fluorescence measured on multiple days and plots time-series
   - Library_master (ex: S018) : (niche use) for processing a library of variants - with and without induction, fold change etc.
   - Dose_response_master (ex: S010) : plots a hill fit of fluorescence/OD vs Inducer concentrations
*Example data files are available for each of the master branches with the names of the corresponding specific experiment number written in the brackets (ex: S010) for quick testing and deployment*. 
Please note that documentation for the branches other than `basic_plots` is not complete, please don't hesitate to ask me for guidance
2. Any experiments with analysis or plots needing significant tweaks is saved into an independent branch named after the experiment. (ex. S015c)
   - In the future specific plotting codes will be saved in the default branch (basic_plots) in `scripts_archive`
