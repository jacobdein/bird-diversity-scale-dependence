# bird-diversity-scale-dependence
Supporting analysis code for the "Scale dependence of bird diversity in London" article

The organization and how to use the files is described below, and further details are provided within each file as well. To run an analysis, adjust the parameters in 0 - Batch Run Analysis.R and run the file. The analysis depends on many of the other listed files, as well as the input data. The results will be saved to a '../results' directory. The script optionally uses the R-ArcGIS bridge to read and write data to an ESRI geodatabase; however, arcgis is not required to compute the scale variance results and you can set 'arcgis' to FALSE in 1 - Run Analysis Function.R if you do not want this functionality or do not have ArcGIS Pro installed (version 3.1 was used in this analysis). File 4b - empirical Bayesian kriging.py will only run if 'arcgis' is set to TRUE.

##### 0 - Batch Run Analysis.R

The main file need to run one or many analyses. Start here and review or adjust the parameters.

##### 1 - Run Analysis Function.R

The main analysis function that organizes the workflow into the five parts described in 'functions/'. Additional parameters may be adjusted at the top of the file, including whether or not to use arcgis.

##### Additional plot files

Files 2 - 7 generate statistics and plots from the results as described within.

##### functions/

* 1 - create hbins.R
* 2 - join hbin IDs.R
* 3 - estimate diversity.R
* 4a - ordinary kriging.R
* 4b - empirical Bayesian kriging.py (requires arcgis)
* 5 - compute scale variance.R

##### utilities/

* arcgis | an arcgis helper function to read and write data
* filter_ebd | script to process and filter raw eBird data

##### package-versions.csv

Lists the package versions used in the analysis
