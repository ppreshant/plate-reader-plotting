Notes of Plate reader analysis.

# Things to do

15/1/22
Need to incorporate the inducer column in the basic_plots pipeline
- [x] Make an `if` loop for when `Inducer %in% colnames(..)` make a dose response plot borrowed from dose_response_. `dose_response_plots.Rmd`
- [ ] Run the regular basic_plots script (in a separate child `.Rmd` file as the else clause) : Will do once the above is settled

Short steps
[x] Check how the data is read in current script - Is Inducer read in properly?
[x ] port code from other branch : dose_response_.

17/1/22
## Scirpt: `dose_response_plots.Rmd`
- `hill_fit.SS()` : trying out 2 self starter functions : convergence failed..(with GFP data of SS.. _hmm, GFP has no trend here, look at mCherry_)
- [ ] Need some way to figure out which data needs to be fit and which ignored due to convergence issues..

# Continuous_growth
