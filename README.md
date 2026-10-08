# Simulation experiment with transferfunctions (SoilHarmony project)

This repository contains an R project with a simulation experiment to evaluate different models for use as transfer-functions. 
The simulation experiments in this repository are meant to support the development of the [Statistical Analysis Plan](https://github.com/soilharmony/wp5-statistical-analysis-plan/) (deliverable D5.1 in WP5). 

The simulations are designed with the R package `stantargets`.
Different simulation experiments are designed for different data generating mechanisms (distributions): Gaussian (eg. pH), Gamma (for strictly positive soil descriptors), simplex/ternary data (particle size distributions), left-censored data (soil descriptors with detection limits).
The file `_targets.yaml` describes the folder structure of the experiments. 
The pipelines and interactive scripts to run the pipelines are found in the folder `pipelines` whereas all R functions, Stan models and Quarto reports can be found in the folder `source`. 


---

The project [Towards a harmonised pan-European monitoring of soil health descriptors](https://doi.org/10.3030/101296615), also known as `SOILHARMONY`, receives funding from the European Union’s HORIZON Innovation Actions 2022 under grant agreement No. 101296615.