[![DOI](https://zenodo.org/badge/741571682.svg)](https://doi.org/10.5281/zenodo.10498084)

# Groundwater regulates the spatial pattern of photosynthesis in the contiguous United States

Francesco Giardina, Jiangong Liu, Sonia I. Seneviratne, Benjamin D. Stocker, and Pierre Gentine

This repository provides the code and intermediary data used in the article available in *Communications Earth & Environment*: [https://www.nature.com/articles/s43247-026-04067-w](https://www.nature.com/articles/s43247-026-04067-w) ([doi:10.1038/s43247-026-04067-w](https://doi.org/10.1038/s43247-026-04067-w)).

All code is licensed under AGPL-v3, and the manuscript and data are licensed as CC-BY. Please review the individual directories and their LICENSE file for more information. The repository can be cited as follows:

> Giardina et al. (2026). Groundwater regulates the spatial pattern of photosynthesis in the contiguous United States: code and intermediary data. [https://doi.org/10.5281/zenodo.10498084](https://doi.org/10.5281/zenodo.10498084)

## Abstract
Plants can access water stored in aquifers to sustain their activity, releasing moisture into the atmosphere—a critical survival mechanism during drought. Understanding the role of groundwater in regulating photosynthesis is thus key for modelling land-atmosphere interactions. However, the contribution of groundwater to spatial variation in mean annual photosynthesis remains poorly quantified, particularly relative to well-known factors like climatic aridity. Here, we use satellite observations of solar-induced fluorescence together with model estimates of water table depth and aridity to investigate the relative role of groundwater and aridity in determining the spatial pattern of photosynthesis in the contiguous United States. Using causality-guided explainable machine learning, we show that the relative importance of groundwater accounts for 48 to 101% of the effect attributed to aridity in modulating forest photosynthesis across the contiguous United States. The relative importance of groundwater compared to aridity remains substantial in savannahs and shrublands (30-58%), grasslands (22-42%), and croplands (15-32%). Our findings reflect the role of water tables in regulating the water available in the rooting zone, with an effect comparable in magnitude to climatic aridity.

## Instructions
Clone the repository:

```bash
git clone https://github.com/fgiardin/groundwater_aridity_SHAP.git
```

Run the scripts from the repository root. The main analysis is in `analysis/1.calculate_Causal_Shapley.R`; figure scripts are in `analysis/2.figures` and `analysis/3.supplementary`. The high-resolution analysis is in `data-raw/high_res`. Figure scripts write their output to the repository root.

Raw input files that are too large for GitHub are not included. Their sources are documented in the Data availability section of the article and in the corresponding processing scripts.

## Repository structure

* `analysis`: model training and scripts for the main and supplementary figures.
* `data-raw`: scripts used to process the raw input data, including the 0.0083° high-resolution analysis.
* `data`: processed inputs and model outputs used by the figure scripts.
* `manuscript`: publication and licensing information.
* `R`: supporting R functions.
