Notes of Plate reader analysis.
tags : #notes 

# General

## Features
- [ ] Need to include the units of the fluorescence data (a.u or MEFL) in the csv file that is saved

### Generalization of markdown plots 
_Need to incorporate the inducer column in the basic_plots pipeline_
- [x] Make an `if` loop for when `Inducer %in% colnames(..)` make a dose response plot borrowed from dose_response_. `dose_response_plots.Rmd`
- [ ] Run the regular basic_plots script (in a separate child `.Rmd` file as the else clause) : Will do once the above is settled

### Analysis
- [ ] Implement a OD subtraction of baseline PBS/LB. Check if this keeps the linearity.. Actually **need to analyze the serial dilution for OD linearity first.**. _Noticed that the OD of culture and 5x concentrated PBS slurry is comparable OD, this is concerning :: `S037 data`_
- [x] Incorporate the MEFL normalizations if found and update legend accordingly

### Short steps
- [x] Check how the data is read in current script - Is Inducer read in properly?
- [x] port code from other branch : dose_response_.


## Bugs


17/1/22
## Script: `dose_response_plots.Rmd`
- `hill_fit.SS()` : trying out 2 self starter functions : convergence failed..(with GFP data of SS.. _hmm, GFP has no trend here, look at mCherry_)
- [ ] Need some way to figure out which data needs to be fit and which ignored due to convergence issues.. _example: negative controls, PBS etc._

# Continuous_growth_curve
## features
- [ ] Add MEFL feature to kinetic read input/processing
- [ ] Add growth rate for OD only and GFP/OD data -- Shyam is interested
- [ ] Generalize the plotting from only GFP to other fluorescences : Like 106 and more in `run_kinetic_reads.R`, with the logic of 67 and 71 in `static_plate_reader..Rmd` in branch:`incorporate_dose_response`

- [ ] Merge incorporate dose response into main, looks like too many things are happening in this branch so next time split into different branches for each small task

Progress
- Fixed bug for plot_kinetic_raw and ribbon.summary functions `Time (hr)` changed to `Time(h)`
- Updated readme to have an image. Jiwoo is testing the scripts 😊
- Pushed changes to `basic_plots` branch