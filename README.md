# plate-reader-plotting/basic_plots
Automated analysis, plotting of fluorescences/OD from **single-read/static** and **kinetic** plate reader excel **.xlsx** files.

_Static read tested on excel files output by Tecan Spark and Infinite M1000 plate readers only, other files would need minor code modifications_
_Kinetic read tested on Tecan Spark only_



## Processing workflow : Static reads

1. Takes plate reader output excel file (`.xlsx`) with added metadata from the user
   - The excel file should be annotated with **sample names** and any other parameters such as **inducer concentrations** (if applicable) next to the OD data table (see example excel files). See below for other kinds of data plots <img src="https://user-images.githubusercontent.com/14856479/139570241-ba36b10d-04cf-4e8b-b659-6ae84f04dffd.png" width="500">
   - You could also shorten a table using wildcards such as : "**all**" for all rows or columns, "**[x-y]**" for ranges in rows _(ex: [C-F])_ or columns with single digits _(ex: [1-3])_. _Note: only single digits 0-9 allowed here_  <img src="https://user-images.githubusercontent.com/14856479/211264019-91bf699f-8a2f-4e39-8cb4-663a4045573d.png" width = 200>

  
   - _other `custom` parameters are read into the data file but not used for plotting by default. But you can easily customize the plotting functions in `plate_reader_plotting_and_html.Rmd` file to add colours/shapes/facets based on the extra metadata you provide_
   - Processing and collating multiple sheets saved at different times/days into the same .xlsx is possible. This would be useful for example in a multi-day experiment. The current code fetches all sheets and merges the data along with the name of the sheet, and makes it easy for you to plot the data from across sheets on your own. For specific support for measuring, and plotting a time-series, check the branch `Time_series_master` for this feature I used for an evolution type experiment. 
   - There is specific support for plotting dose-response curves, and attaching hill curve fits to the data (using the `run_dose_response_pipeline` switch in `0.5-user_inputs.R`), but some border cases with zeros in the data and points that won't fit need to be smoothened out so that errors in fitting of the non linear regression don't hold up the rest of the script. Leave an issue if you are looking for support on this feature and I would be happy to assist!
1. Program reads the OD, GFP and RFP (_could easily be extended to other FPs, see point 3b, leave an issue if you need help!_). OD should always be first and fluorescence measurements in any order after the OD. 
   - *Please label the measurements with OD and the name of the fluorophores in the Tecan/Infinite plate reader protocol file before acquiring the data, these will appear in the datasheet and make it easy for R to recognize and make the plots* 
2. Data is processed: Autofluorescence is subtracted, Fluorescence/OD ratio is calculated for each fluorophore and MEFL (_molecules of equivalent fluorophores_) normalization is done if samples include small molecule fluorophores (FITC = 100 uM, SULFOrhodamine = 50 uM : _Ask @prashant if you are using different fluorophores/concentrationa and the code can be generalized to include those)._
   - Sample named by common E.coli strains, 'MG1655', 'DH10B' or 'PBS' is assumed to be the background. Edit the variable `X`, if your base strain is different.
   - Code works for these fluorophores: GFP, mGreenlantern, mCherry, mScarlet. If you are measuring any other fluorophores, make sure to name them to translate to a **x**FP (CFP, YFP, RFP..)  in variable `measurement.labels_translation` in `1-cleaning.data_manipulate_columns.R`
3. Pre-formatted plots are made and saved into a HTML file (for easy access and presentation) for GFP/OD, RFP/OD and raw data of GFP, RFP. _Plots show each replicate data as points and the mean using lines and a light grey bar to aid the eye._ <img src="https://user-images.githubusercontent.com/14856479/139571344-f2f0c1c9-9b5b-40ba-8e9d-49997b1b35fb.png" width="500">

4. A couple of interactive plots are also saved into the HTML for exploratory data analysis. 
   - Interactivity using *plotly* is quite powerful with features of zoom, showing subsets of data etc. You are always welcome to add more features using more advanced plotly functions by building the plot from scratch in plotly instead of ggplot2  

5. Finally the data values of the base strain (MG1655, DH10B etc.) that was subtracted for the fluorescence is shown in the _Data tables_ section for quick glance.


## Processing workflow : Kinetic reads

1. Takes plate reader output excel file (`.xlsx`)
   - The excel file should _include a sheet_ named as `metadata`. This sheet should have a grid with **sample names** and any other parameters such as **inducer concentrations** (if any) one below another 
   (see example excel file : `example kinetic-E01.4-Vmax Ribo fluor kinetic_28-11-20`). 
   
   - _other parameters are read into the data file but not plotted, you can easily customize the plotting functions in `run_kinetic_reads.R` file to add colours/shapes/facets based on the extra metadata you provide_
   - Can also plot GFP/OD and other kinds of data transformations pretty easily 

2. Program reads the OD and any other fluorescence data
   - *Please label the measurements with OD and the name of the fluorophores in the Tecan/Infinite plate reader protocol file before acquiring the data, these labels will enable the script to read the data properly and will appear in the plots* 
3. Raw data plots and ribbon plots showing mean of replicates, with a shadow for standard deviation, along with direct labels are plotted into the `.html` file. Interative plots also included. Green fluorescence data is also plotted, if present along with OD. _need to generalize it to red fluor as well_

_example kinetic OD plot_

<img src="https://user-images.githubusercontent.com/14856479/155623351-6cdbaff6-b48b-41c1-aa52-85279b6c786c.png" width="500">

Put an example plot here: 

4. If you want growth rates, lag-time and other features, you can use the `growthcurver` R package. Tips below
     
     ``` 
     # grouping each dataset
     group_by(Samples) %>% 
     
     # condensing all TIME; OD into data frames for each sample (nesting)
     nest (data = c(Time, OD)) %>%
  
     # fit to each sample's growth data 
     mutate(fits = map(data, 
                      ~ growthcurver::SummarizeGrowth(.x$Time, .x$OD))
     ) %>% 
     
     # Retrieve parameters from the fit
     mutate(growth_rate = map_dbl(fits, ~ .x$vals$r),
            error = map_dbl(fits, ~ .x$vals$sigma/2))
      ```


## Modifications for other plate reader machines
The code (in script : `2-read_multiple_grids_in_sheet.R`) will recognize that your machine isn't matching the known ones : _Tecan Spark, Tecan Infinite M1000._ It will ask you to enter the word that your output `.xlsx` file is using to designate a measurement's name/ID and entering that might enable the script to work. Caveat: More changes might be necessary, and I _will update the code and README as I find/discover more stuff on instrument compatibility_. Please post an issue on github along with your `.xlsx` file and I can look into making the script work for your file. 


4. If you want growth rates, lag-time and other features, you can use the `growthcurver` R package. Tips below
      
     ```
     # grouping each dataset
     group_by(Samples) %>% 
     
     # condensing all TIME; OD into data frames for each sample (nesting)
     nest (data = c(Time, OD)) %>%
  
     # fit to each sample's growth data 
     mutate(fits = map(data, 
                      ~ growthcurver::SummarizeGrowth(.x$Time, .x$OD))
     ) %>% 
     
     # Retrieve parameters from the fit
     mutate(growth_rate = map_dbl(fits, ~ .x$vals$r),
            error = map_dbl(fits, ~ .x$vals$sigma/2))
     ```

## Git organization
1. There are different master branches for each of the major kinds of experiments. These are older scripts and will be merged into the current branch eventually -
   - Time_series_master (ex: S015c) : reads multiple sheets in the same .xlsx file for fluorescence measured on multiple days and plots time-series
   - Library_master (ex: S018) : (niche use) for processing a library of variants - with and without induction, fold change etc.
   - Dose_response_master (ex: S010) : plots a hill fit of fluorescence/OD vs Inducer concentrations
*Example data files are available for each of the master branches with the names of the corresponding specific experiment number written in the brackets (ex: S010) for quick testing and deployment*. 
Please note that documentation for the branches other than `basic_plots` is not complete, please don't hesitate to ask me for guidance
2. Any experiments with analysis or plots needing significant tweaks is saved into an independent branch named after the experiment. (ex. S015c)
   - In the future specific plotting codes will be saved in the default branch (basic_plots) in `scripts_archive`

# Copyleft : GPL-3.0-or-later license
```
Scripts for automated processing and plotting of plate reader fluorescence and OD including timecourse data
Copyright (C) 2023  Prashant Kalvapalle

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
```
