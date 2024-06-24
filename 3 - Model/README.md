
# Model

The statistical model used in these analyses is similar to that proposed
in McLain et al. (2019) and discussed in more detail therein. The
general model is a penalized longitudinal mixed-model with a
heterogeneous error term. Non-linear longitudinal patterns in the
outcomes are captured using penalized cubic B-splines, with
country-specific intercepts and random cubic B-splines. The connection
between penalized spline smoothing and linear mixed-effects models
(Currie and Durban 2002) allows the model to penalize the likelihood to
estimate the B-spline coefficients.

## Model Details

- **Heterogeneous Penalized Longitudinal Mixed-Model**: The model
  includes penalized cubic B-splines to capture non-linear longitudinal
  patterns, with country-specific intercepts and random cubic B-splines.
  The heterogeneous error term corrects for the fact that the SSE varies
  by study.
- **Model Fitting**: The model is fitted as a linear mixed model, with
  random effects chosen to penalize the likelihood when the B-spline
  coefficients are not constant.
- **Random B-Spline Coefficients**: The number of random B-spline
  coefficients is selected based on corrected Akaike information
  criterion (AICc).
- **Covariance Matrix Selection**: The covariance matrix of the random
  splines is selected by testing variance components, compound
  symmetric, and unstructured specifications, choosing the type that
  minimizes AICc.
- **Confidence and Prediction Intervals**: The model provides accurate
  confidence intervals and prediction intervals as demonstrated in
  McLain et al. (2019).

## Usage

To run the analysis on the multiply imputed dataset, follow the
instructions below:

1.  Open the `1 - Analysis_MI.R` file.
2.  Set the working directory (`wd`) to the folder that contains the
    **“3 - Model”** folder.

### Arguments

- **marker**: Set this to `"Stunting"` or `"Overweight"`.
- **year/month**: Needed to find the data. This should correspond to the
  `year` and `month` values used in the data cleaning process.
- **marker_f**: Used to append the file name for situations where
  multiple models need to be run.

### Script Sections

1.  **Setting Model Parameters** (lines 13 - 49):

    - In this section, the fixed and random effect parameters can be set
      for either indicator (Stunting or Overweight).

2.  **Running and Outputting the Models** (lines 58 - ):

    - This section runs the analyses for each imputed dataset and
      outputs the results.

## Contact

For questions or issues regarding this folder, please contact the
repository maintainer.

## References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-currie2002flexible" class="csl-entry">

Currie, Iain D, and Maria Durban. 2002. “Flexible Smoothing with
p-Splines: A Unified Approach.” *Statistical Modelling* 2 (4): 333–49.
https://doi.org/<https://doi.org/10.1191/1471082x02st039ob>.

</div>

<div id="ref-McLetal19" class="csl-entry">

McLain, Alexander C, Edward A Frongillo, Juan Feng, and Elaine Borghi.
2019. “Prediction Intervals for Penalized Longitudinal Models with
Multisource Summary Measures: An Application to Childhood Malnutrition.”
*Statistics in Medicine* 38 (6): 1002–12.
https://doi.org/<https://doi.org/10.1002/sim.8024>.

</div>

</div>
