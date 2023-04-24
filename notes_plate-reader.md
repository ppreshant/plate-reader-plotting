Notes of Plate reader analysis.
tags : #notes 

# General

## Features
- [x] Need to include the units of the fluorescence data (a.u or MEFL) in the csv file that is saved
- [ ] MEFL automation : get old calibrants from file in `processed/..csv?`. _Stand-in values since we're using fixed gain of 90 for most readings now a days ; ideally add fresh calibrants if data is important.._ 
	- Change switch to not check for native calibrants / add another switch for getting calibrants: in `2-read_multiple_Grids..R` : `MEFL_normalization <<- ..`
	- (_future implement_) : Look for `Gain` in the first col of `fl$sheet` and read the 6th col of this .. need for all fluor; it's laborious
- [x] (_later_) Might be a good idea to include the row and column indices in the `processed.data` for cross-checking/data provenance?
- [ ] (_ignore, can't understand_) Extrapolation of regex in metadata : mixing some regex with some letters separated by spaces doesn't work, since the spaces are just skipped :( _:( Not enough context to recall what this was about)_

## Dose_reponse plots
Work on this is going on in branch : `incorporate_dose_response` -- need to merge basic plots with this before adding any work : 18-April-24

- [ ] Build safety into the hill fit using `purrr::safely()` as in [qPCR RAM stability](https://github.com/ppreshant/qPCR-analysis/blob/main/adhoc%20scripts/S8_RAM_stability.R)
- [ ] show hill coeficients on plot in `plot_dose_response()` in 4-plotting_fns.R : `.data$fit[[1]]$coefficients` gives a named vector!
- [ ] Do a `try..catch` workflow to choose between hill fits and just connecting the mean points with a line? _note from commit:_ `fb96971`
- [ ] Use insurance for hill fitting failure. Can use functions ` purrr::safely() or possibly()`. Issues explained below -
	- guide for error catching helpers from purrr : [aosmith blog](https://aosmith.rbind.io/2020/08/31/handling-errors/)
	- `possibly()` and `safely()`  work with functions as input, but the hill fitting function : [`drc::drm()`](https://www.rdocumentation.org/packages/drc/versions/2.5-12/topics/drm) value is a `drc` object and not functions
	- Contrast with [`stats::lm`](https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/lm) : `lm` returns an object of `class : "lm"`; which is a list containing stuff.. - _probably recognized by the above functions_
- [ ] How to retain the name of the inducer and units into the plots? (ex: AHL_nM)

### Generalization of markdown plots 
_Need to incorporate the inducer column in the basic_plots pipeline_
- [x] Make an `if` loop for when `Inducer %in% colnames(..)` make a dose response plot borrowed from dose_response_. `dose_response_plots.Rmd`
- [ ] Run the regular basic_plots script (in a separate child `.Rmd` file as the else clause) : _Will do once the above is settled._
	- Sould these both be called from the `.R` script to avoid a dummy parent `.Rmd` file? _The data output from this file useful generally ; so keep it?_

### Analysis
- [x] Implement a neat shortcut to get metadata that are same for the whole row/column with an 'extrapolation' function
- [ ] Implement a OD subtraction of baseline PBS/LB. Check if this keeps the linearity.. Actually **need to analyze the serial dilution for OD linearity first.**. _Noticed that the OD of culture and 5x concentrated PBS slurry is comparable OD, this is concerning :: `S037 data`_
- [x] Incorporate the MEFL normalizations if found and update legend accordingly

### Short steps
- [x] Check how the data is read in current script - Is Inducer read in properly?
- [x] port code from other branch : dose_response_.


## Bugs
- [ ] Fix the geom_text layer in the interactive plot (RFP_mean - remove label/shorten, Round decimals? / Samples = 0.9 -- what is this?). This works fine in regular plot, only poor in ggplotly. Example : ![[Pasted image 20230318160713.png]]
- [ ] Extrapolation of regex in metadata : There is a silent bug that is missing a few links from sample name to inducer and links the wrong.


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