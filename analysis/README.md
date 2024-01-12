To reproduce the analysis, run the following scripts in order. Scripts 1 and 2 need to be run on a HPC cluster (we used Euler from ETHZ Zürich).

* **How_to_run_scripts_on_Euler.md**: Instructions to run scripts 1 and 2 on Euler.

* **1.data_screening_euler.R**: Prepares FLUXNET2015 raw data in the right format for the deep learning model.

* **2.run_ML_model_euler.R**: Runs the deep learning model for every site and saves model outputs.

* **3.summary_plots.R**: Generates results on the performance of the model (Fig. 2). It divides the results in groups and prints the grouping statistics (Fig. 4). It also prints the fET vs CWD multi-panel figure (Fig. 5). It also produces Supplementary Figs 1-2).
