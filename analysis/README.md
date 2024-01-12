To reproduce the analysis, run the following scripts in order:

* **1.calculate_longterm_means.R**: Calculates the long-term means for all data fast using raster formats.

* **2.prep_SHAP.R**: Merge WTD and SIF variables into one dataframe to train XGB models and perform SHAP analysis.

* **3.calculate_SHAP.R**: Trains PFT-specific XGB models and calculate SHAP values for each model.

### `Figures`
Folder containing the scripts to generate all figures. 

* **plot_Fig_1.R**
* **plot_map_variables.R**: plots maps of the main variables used in this study
* **plot_summary_SHAP.R**: loads SHAP results and plots nice summary plots SHAP (bee-swarm and dependence plots)
* **plot_map_SHAP.R**: plots map of SHAP values of each feature/predictor
* **plot_crosscorr_scatters.R**: plot the cross-correlation between variables used in the XGB models (supplementary figure)
* Other figures in supplementary materials were plotted using the scripts above with other settings and different data (see for instance plot_summary_SHAP.R)

