# Groundwater rivals aridity in determining global photosynthesis
For full details on the methodology and results please refer to the above manuscript folder. For a step-by-step guide on how to reproduce the analysis, refer to the 'Instructions' below. All code is licensed under AGPL-v3, and the manuscript and data are licensed as CC-BY. Please review the individual directories and their LICENSE file for more information.

## Abstract
Understanding the role of groundwater in regulating photosynthesis is key in land-climate interactions. However, the impact of groundwater on terrestrial ecosystem productivity remains poorly understood. Here, we use satellite observations of solar-induced fluorescence as a proxy for photosynthesis, together with estimates of water table depth (WTD) and aridity as quantified by the annual moisture index with reanalysis data, to investigate the relationships between groundwater and photosynthesis. Using explainable machine learning (Shapley additive explanations or SHAP), we demonstrate that groundwater plays a crucial role in determining the spatial patterns of global photosynthesis, with varying importance across different ecosystem types, and that this effect is comparable to aridity. We show that in forests across the contiguous USA (CONUS), the relative importance of groundwater represents 89% of the effect attributed to the moisture index in modulating photosynthesis, and this impact is even more pronounced globally (105% relative importance). The relative importance of groundwater compared to the moisture index remains substantial in grasslands (37%), savannas and shrublands (25%), and croplands (15%) in CONUS. Global analysis confirms these trends for grasslands (41%) and croplands (15%) but indicates a 60% importance in savannas and shrublands. Our findings highlight the key global role of groundwater in driving ecosystem productivity.


## Instructions
First, clone this repo to your local computer:

```
git clone [https://github.com/computationales/fET](https://github.com/fgiardin/groundwater_aridity_SHAP/)
```

To reproduce the analysis and figures, you can follow the steps described in the `analysis` folder and its 'README.md' file. To avoid overwriting the dataframes already loaded in this repo, the scripts contained in the `analysis` folder will save their output in the main directory of the project (aka the directory where this README also is). 

Below is an overview of the content of the others directories. For detailed instructions, please consult the README files located in each subdirectory, or refer to the opening line of each script for a description of its purpose.

* `R`: contains all the R functions used in the analysis.
* `data-raw`: contains raw data and the scripts used to download, extract and process raw data. Very big data are not uploaded to this repo; please refer to the "Data availability" section of the manuscript to download them.
* `data`: contains processed data and output of the analysis.
* `manuscript`: contains manuscript and figures.

