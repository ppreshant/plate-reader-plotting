# plate-reader-plotting
Normalizing fluorescence to OD and plotting from single-read plate reader excel files (tested on Tecan Spark and Infinite M1000 plate readers)

Steps
1. Takes plate reader output excel file with multiple sheets (.xlsx)
  1a. The excel file should be annotated with sample names and inducer concentrations (if any) next to the OD data table (see example excel files)
2. User inputs the names of different sheets (could be reading on different dates - for a timeseries or before and after induction)
3. Program reads the OD, GFP and RFP (in that order)
4. Data is processed: Autofluorescence is subtracted (Sample named 'MG1655' is assumed to be the background), Fluorescence /OD ratio is calculated
5. Various pre-formatted plots are made and saved into a HTML file (for easy access)
6. A couple of interactive plots are also saved into the HTML for exploratory data analysis

Git organization
1. Each branch of this repo is the code run on a file named after the experiment (ex: S015c) with the desired plotting formats and extra items
2. There are different master branches for each of the major tasks the experiments fall under
  a. Time_series_master (ex: S015c)
  b. Library_master (ex: S018)
  C. Dose_response_master (ex: S010)
Example data files are available for each of the master branches with the names of the corresponding specific experiment number in brackets (ex: S010) for quick testing and deployment
  
