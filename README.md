[![DOI](https://zenodo.org/badge/741571682.svg)](https://zenodo.org/doi/10.5281/zenodo.10498084)

# Strong impact of groundwater on long-term photosynthesis
This repository provides code and intermediary data to reproduce the analysis of the abovementioned project. For full details on the methodology and results please refer to the 'manuscript' folder. For a step-by-step guide on how to reproduce the analysis, refer to the 'Instructions' below. 

All code is licensed under AGPL-v3, and the manuscript and data are licensed as CC-BY. Please review the individual directories and their LICENSE file for more information. You can cite the code in this repository as follows:

> Giardina et al. (2024). Strong impact of groundwater on long-term photosynthesis: code and intermediary data. [https://zenodo.org/doi/10.5281/zenodo.10498084](https://zenodo.org/doi/10.5281/zenodo.10498084)


## Abstract
Plants can access underground water reserves to sustain their activity, releasing moisture into the atmosphere—a critical survival mechanism during drought. Understanding the role of groundwater in regulating photosynthesis is thus key for predicting land-surface processes. However, the impact of groundwater on terrestrial ecosystem productivity remains poorly quantified, particularly when compared to well-known factors like aridity. Here, we use satellite observations of solar-induced fluorescence as a proxy for photosynthesis, together with model estimates of water table depth and aridity—quantified by a moisture index from reanalysis data—to investigate the relationship between groundwater and photosynthesis. Using causality-guided explainable machine learning, we demonstrate that groundwater plays a crucial role in determining spatial patterns of photosynthesis, with varying importance across ecosystem types, and that its effect is comparable to aridity. We show that the relative importance of groundwater accounts for 48 to 101% of the effect attributed to aridity in modulating forest photosynthesis across the contiguous USA. The relative importance of groundwater compared to the aridity remains substantial in savannahs and shrublands (30-58%), grasslands (22-42%), and croplands (15-32%). Our findings highlight the key role of groundwater in driving ecosystem long-term productivity. 

## Instructions
First, clone this repo to your local computer:

```
git clone https://github.com/fgiardin/groundwater_aridity_SHAP/
```

To reproduce the analysis and figures, follow the steps outlined in the `analysis` folder and its `README.md` file. To avoid overwriting the dataframes already loaded in this repository, scripts will save their output to the main directory of the project (the same directory where this README.md file is located).

Below is an overview of the contents of the other directories. For detailed instructions, please refer to the README files in each subdirectory and the opening line of each script, which provides a description of its purpose.

* `data-raw`: contains raw data and the scripts used to download, extract and process raw data. Very big raw data can't be uploaded to this repo; please refer to the "Data availability" section of the manuscript to download them directly.
* `data`: contains input data (preprocessed as described above) and output from the training of XGB models and the calculation of Causal Shapley values
* `manuscript`: contains manuscript and figures.
* `R`: contains all the R functions used in the analysis.

