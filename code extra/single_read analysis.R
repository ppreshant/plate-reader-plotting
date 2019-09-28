# Read in the plate reader excel file - locate the plate rows and columns for each variable and do some plotting and normalization calculations
# Author: Prashant Kalvapalle;  Started : March 24 2019

# The last part of the script contains important codes from the console that were used for specific situations : These will be commented

# Sourcing the file with all the functions
source('./general_functions_plate_reading.R')

# User inputs: choose file name, title for plots and experiment mode (file name starts in the previous directory of this Rproject) ----

flnm <- '../S05d_AHL-GFP_pSH001 and flip_29-3-19.xlsx'

# File input and processing ----

fl <- read_plateReader_file(flnm) # load all non empty sheets of the excel file into fl - as a list 

data_sheet1 <- fl$Sheet2 # extract the sheet of interest (sheet2 is by default the first non-empty sheet)
# data_sheet1 %<>% filter(`..1` == Label|<>|A--H) # read up regex, stringr package and str_detect function


# extract plate values from file ----
 # as of now is not generalized code; input the parameters for specific plate layout
# just taking the normalized values (manually done in spreadsheet)
nRows <- 5; 
nCols <- 5;

# COntext: 10 rows of lines before data is seen, 27 rows between data and fluorescence values, 27 more rows till RFP values
sample_names <- data_sheet1[10: (10+nRows), 11: (11+nCols+1)] # exract the Sample names table
n_GFP <- data_sheet1[(37+nRows): (37+2*nRows-1), 11: (11+nCols-1)] # exract the GFP values
n_RFP <- data_sheet1[(64+2*nRows-1): (64+3*nRows-2), 11: (11+nCols-1)] # exract the RFP values

# freshen up data : Move DH10B control to first spot
n_GFP[1,1] <- n_GFP[5,1]; n_RFP[1,1] <- n_RFP[5,1]
sample_names[2,1] <- sample_names[6,1]

sample_names_row <- sample_names[2,1:5]
n_GFP <- n_GFP[1:4,]; n_RFP <- n_RFP[1:4,]

colnames(n_GFP) <- sample_names_row
colnames(n_RFP) <- sample_names_row

# custom codes and context ----

# S010 plotting
merged3 <- merged2 %>% filter(str_detect(Samples, "^pRV01")) # filtering custom samples
merged3 <- merged2 %>% filter(str_detect(Inducer, "0$|none")) # filtering custom samples
merged3 %>% ggplot(aes(Samples,GFP, color = Samples)) + geom_point() + geom_line() + facet_wrap(~Samples, scales = 'free_x') -> plta