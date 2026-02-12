[![DOI](https://zenodo.org/badge/741571682.svg)](https://zenodo.org/doi/10.5281/zenodo.10498084)

# Strong impact of groundwater on long-term photosynthesis in the contiguous United States
This repository provides code and intermediary data to reproduce the analysis of the abovementioned project. For full details on the methodology and results please refer to the 'manuscript' folder. For a step-by-step guide on how to reproduce the analysis, refer to the 'Instructions' below. 

All code is licensed under AGPL-v3, and the manuscript and data are licensed as CC-BY. Please review the individual directories and their LICENSE file for more information. You can cite the code in this repository as follows:

> Giardina et al. (2024). Strong impact of groundwater on long-term mean photosynthesis in the contiguous United States: code and intermediary data. [https://zenodo.org/doi/10.5281/zenodo.10498084](https://zenodo.org/doi/10.5281/zenodo.10498084)


## Abstract
Plants can access water stored in aquifers to sustain their activity, releasing moisture into the atmosphere—a critical survival mechanism during drought. Understanding the role of groundwater in regulating photosynthesis is thus key for modelling land-atmosphere interactions. However, the impact of groundwater on long-term terrestrial ecosystem productivity remains poorly quantified, particularly when compared to well-known factors like aridity. Here, we use satellite observations of solar-induced fluorescence as a proxy for photosynthesis, together with model estimates of water table depth and aridity—quantified by the moisture index with reanalysis data—to investigate the relationship between groundwater and photosynthesis. Using causality-guided explainable machine learning, we demonstrate that groundwater plays a crucial role in determining spatial patterns of long-term mean photosynthesis, with varying importance across ecosystem types, and that its effect is comparable to aridity. We show that the relative importance of groundwater accounts for 48 to 101% of the effect attributed to aridity in modulating forest photosynthesis across the contiguous United States. The relative importance of groundwater compared to aridity remains substantial in savannahs and shrublands (30-58%), grasslands (22-42%), and croplands (15-32%). Our findings indicate that groundwater is a key driver of long-term mean photosynthesis, with an effect comparable in magnitude to climatic aridity.

## Instructions
First, clone this repo to your local directory:

```
git clone https://github.com/fgiardin/groundwater_aridity_SHAP/
```

To reproduce the analysis and figures, follow the steps outlined in the `analysis` folder and its `README.md` file. To avoid overwriting the dataframes already loaded in this repository, scripts will save their output to the main directory of the project (the same directory where this README.md file is located).

Below is an overview of the contents of the other directories. For detailed instructions, please refer to the README files in each subdirectory and the opening line of each script, which provides a description of its purpose.

* `data-raw`: contains raw data and the scripts used to download, extract and process raw data. Very big raw data can't be uploaded to this repo; please refer to the "Data availability" section of the manuscript to download them directly. It also containt the subdirectory `high_res` to repeat the whole analysis at 1-km resolution. 
* `data`: contains input data (preprocessed as described above) and output from the training of XGB models and the calculation of Causal Shapley values
* `manuscript`: contains manuscript and figures.
* `R`: contains all the R functions used in the analysis.

