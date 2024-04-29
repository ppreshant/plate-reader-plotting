Here's a documentation of a few growth curve algorithms I have encountered

1. Logistic equation with interpretable parameters. R implementation in `growthcurver` package. Vignette [here](https://cran.r-project.org/web/packages/growthcurver/vignettes/Growthcurver-vignette.html).
	> In the `Growthcurver` package, we fit growth curve data to a standard form of the logistic equation common in ecology and evolution whose parameters (the growth rate, the initial population size, and the carrying capacity) provide **meaningful population-level information with straight-forward biological interpretation**.
	> Logistic equation is : $N_t= \frac{K}{1+(\frac{K−N_0}{N_0})e−rt}$. Where 
	- $N_0$ = initial population
	- $K$ = carrying capacity (max population)
	- $r$ = growth rate, *intrinsic, what would be constant without carrying capacity limits*
2. Gompertz function. Very through R [implementation](https://github.com/ppreshant/Growth-curve-analysis) including smoothing, finding peaks and valleys (for diaxuic growth too!), plotting for 96 wells. **Source**: _Friesen, Maren L., et al. "Experimental evidence for sympatric ecological diversification due to frequency‐dependent competition in Escherichia coli." Evolution 58.2 (2004): 245-260._
3. Modified logistic / based on a median filter for smoothening data. Find the python [implementation](https://github.com/SilbergLabRice/PlottingTools#growth-curve-modeler-gcm) here and the citation paper [here](https://doi.org/10.1016/j.mimet.2016.11.015). 
	- Other options for fitting : Gompertz, modgompertz, logistic, modlogistic
4. Assumption free (*semi/non-parametric?*) analysis called AMIGA. Python implementation [here](https://github.com/dacuevas/PMAnalyzer). Source: *Midani, Firas S., James Collins, and Robert A. Britton. "AMiGA: software for automated analysis of microbial growth assays." Msystems 6.4 (2021): 10-1128.* [msystems](https://journals.asm.org/doi/10.1128/mSystems.00508-21)
	- *This seems very promising, needs to be investigated! - Prashant (4/2024)*
	>AMiGA models growth curves with GP regression and infers biologically meaningful microbial growth parameters, including maximum specific growth rate (i.e., exponential growth rate), lag time, carrying capacity, and area under the curve (AUC)

