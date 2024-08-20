
# Global Health Estimates of Stunting and Overweight

This repository contains all the R code used to produce the Global
Health Estimates of Stunting and Overweight, as developed by the
UNICEF-WHO-World Bank Joint Child Malnutrition working group.

## Overview

The R scripts in this repository are organized into several folders,
each serving a specific purpose in the data processing and modeling
workflow. Each folder contains a README file with more details about its
contents and specific instructions.

To understand the technical details of the modeling procedure and the
methods used to estimate confidence and prediction intervals, please
refer to:

- McLain et al. (2019)
- An application of the methods published in Saraswati et al. (2022), and
a corresponding editorial by Finaret (2022).

## Folder Structure

### 1. Preparing Covariates

This folder contains R code to perform multiple imputation of the
covariate data.

### 2. Preparing Primary Data

This folder includes programs to:

- Impute missing standard error (SE) information.
- Integration of data sources that were originally collected using different 
(non-standard) age ranges.
- Merge survey data with covariate data.
- Generates approximate sex-specific prevalence rates for specific years 
and countries (solely for use in visual figures).

### 3. Model

This folder contains scripts for performing a multiply imputed analysis
of the data for stunting or overweight.

### 4. Results

In this folder, you will find code for: 

- Pooling imputed estimates. 
- Plotting results. 
- Comparing estimates with past data.

### Additional Folders

- **Data**: Contains survey prevalence estimates, raw covariate data,
  country-level classifications, and outputted analysis files.
- **Figures**: Contains outputted figures displaying the results.
- **Utils**: Contains various programs and functions required to run the
  analyses.

## Important Note

All the programs in the folders `Preparing Primary Data`, `Model`, and
`Results` need to be run separately for both Stunting and Overweight.
Ensure that you read each file carefully to verify that the files are
named and stored correctly.

## References

For further reading on the technical details and applications, please
refer to the McLain et al. (2019).

## License

This project is licensed under the MIT License. See the LICENSE file for
details.

## Contact

For questions or issues, please contact the repository maintainer.

## Acknowledgments

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-FINARET20221595" class="csl-entry">

Finaret, Amelia B. 2022. “Advancing Nutritional Epidemiology by Linking
Datasets and Addressing Data Quality.” *The Journal of Nutrition* 152
(7): 1595–96. <a href="https://doi.org/10.1093/jn/nxac092">doi.org/10.1093/jn/nxac092</a>

</div>

<div id="ref-McLetal19" class="csl-entry">

McLain, Alexander C, Edward A Frongillo, Juan Feng, and Elaine Borghi.
2019. “Prediction Intervals for Penalized Longitudinal Models with
Multisource Summary Measures: An Application to Childhood Malnutrition.”
*Statistics in Medicine* 38 (6): 1002–12. <a href="https://doi.org/10.1002/sim.8024">doi.org/10.1002/sim.8024</a>

</div>

<div id="ref-Saretal22" class="csl-entry">

Saraswati, Chitra M, Elaine Borghi, João JR da Silva Breda, Monica C
Flores-Urrutia, Julianne Williams, Chika Hayashi, Edward A Frongillo,
and Alexander C McLain. 2022. “Estimating Childhood Stunting and
Overweight Trends in the European Region from Sparse Longitudinal Data.”
*The Journal of Nutrition* 152 (7): 1773–82. <a href="https://doi.org/10.1093/jn/nxac072">doi.org/10.1093/jn/nxac072</a>

</div>

</div>
