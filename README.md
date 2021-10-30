# plate-reader-plotting/basic_plots
Automated analysis, plotting of fluorescences/OD from **single-read/static** plate reader excel files.
_tested on Tecan Spark and Infinite M1000 plate readers, other files would need minor code modifications_

## Processing workflow

1. Takes plate reader output excel file (`.xlsx`)
  1a. The excel file should be annotated with **sample names** and any other parameters such as **inducer concentrations** (if any) next to the OD data table (see example excel files)
  1b. Processing multiple sheets in .xlsx is possible, for example a multi-day experiment. Check the branch `Time_series_master`
2. **Update?** User inputs the names of different sheets (could be reading on different dates - for a timeseries or before and after induction)
3. Program reads the OD, GFP and RFP. OD should always be first and any fluorescences in any order after the OD. 
  3a. *Please label the measurements with OD and the name of the fluorophores in the Tecan/Infinite plate reader protocol file before acquiring the data, these will appear in the plots* 
4. Data is processed: Autofluorescence is subtracted, Fluorescence/OD ratio is calculated for each fluorophore.
  4a. Sample named by common E.coli strains, 'MG1655', 'DH10B' or 'PBS' is assumed to be the background. Edit the variable `X`, if your base strain is different.
5. Various pre-formatted plots are made and saved into a HTML file (for easy access and presentation)
6. A couple of interactive plots are also saved into the HTML for exploratory data analysis. 
  6a. Interactivity using *plotly* is quite powerful with features of zoom, showing subsets of data etc. You are always welcome to add more features using more advanced plotly functions by building the plot from scratch in plotly instead of ggplot2  

## Git organization
1. There are different master branches for each of the major tasks the experiments fall under
  a. basic_plots
  b. Time_series_master (ex: S015c)
  c. Library_master (ex: S018)
  d. Dose_response_master (ex: S010)
*Example data files are available for each of the master branches with the names of the corresponding specific experiment number written in the brackets (ex: S010) for quick testing and deployment*. 
Please note that documentation for the branches other than `basic_plots` is not fully complete, please don't hesitate to ask me for guidance
2. Any experiments with analysis or plots needing significant tweaks is saved into an independent branch named after the experiment. (ex. S015c)
