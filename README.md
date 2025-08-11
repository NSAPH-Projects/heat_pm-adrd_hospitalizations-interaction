# Combined and synergistic effects of heat and fine particulate matter on hospitalization among patients with Alzheimer’s disease and related dementias

Code for _Federica Spoto, Antonella Zanobetti, Scott W. Delaney, Thomas M. Gill, Michelle L. Bell, Francesca Dominici, Danielle Braun and Daniel Mork_ "Combined and synergistic effects of heat and fine particulate matter on hospitalization among patients with Alzheimer’s disease and related dementias"

## Abstract
Patients with Alzheimer’s disease and related dementias (ADRD) are vulnerable to environmental stressors such as heat and air pollution. Both exposures have been independently associated with adverse health outcomes, but their combined effects remain less understood. We aimed to quantify the joint impact of extreme heat and fine particulate matter exposure (PM<sub>2.5</sub>) on the risk of all-cause hospitalization among an ADRD cohort of Medicare enrollees aged 65 years and older.

We conducted a time-stratified case-crossover analysis using data from Medicare enrollees who previously had an ADRD-related hospitalization in the contiguous US during 2000-2016. Daily heat index and PM<sub>2.5</sub> data were linked to enrollees' residence ZIP codes. Conditional logistic regression models were applied to evaluate the same-day effects of single and joint exposures on all-cause hospitalization during the warm season (May-September), incorporating interaction terms to explore potential synergistic impacts.

We found a linear trend between heat index and all-cause hospitalization with an odds ratio (OR) of 1.017 (95% CI: 1.004, 1.031) for extreme heat days (99<sup>th</sup> percentile) versus median. The PM<sub>2.5</sub>-hospitalization relationship was nonlinear with a steeper slope at lower concentrations. The OR for all-cause hospitalization at 10&micro;g/m<sup>3</sup> versus 5&micro;g/m<sup>3</sup> was 1.010 (95% CI: 1.005, 1.015). The joint-exposure analysis confirmed these findings; we estimated an OR for hospitalization of 1.016 (95% CI: 1.001, 1.032) on extreme heat days versus median when simultaneously adjusting for the estimated change in PM<sub>2.5</sub>. These findings underscore the importance of considering both heat and air pollution exposures in assessing health risks for ADRD populations.  

## Workflow 
- cohort creation: contains the R code to build the study cohort
  - 01_cohort_definition.R: creates the cohort of patients who previously had an ADRD-related hospitalization, and identifies the case day (first all-cause hospitalization) and the control days. 
  - 02_get_exposure.R: gets daily exposure levels and defines heat index.
  - 03_merge_exposure.R: assigns the same day exposure levels to the cohort of interest.
  - 04_create_lagged_data.R: creates the lagged exposure levels and assigns them to the cohort of interest. It is necessary for the sensitivity analysis.
- analysis: contains the R code to run the analyses
  - 01_single_exp.R: runs the linear and non-linear single exposure analyses.
  - 02_multi_exp.R: runs the linear and non-linear joint-exposures analyses without interaction and the linear joint-exposures analysis with interaction.
  - 03_multi-exp_non-linear_interaction.R: runs non-linear joint-exposures analysis with interaction. Needs utils.R.
  - 04_dependency_non-linear.R: runs the dependency analysis, including the model accounting for the association between heat index and PM<sub>2.5</sub>. Needs utils.R.
  - 05_sensitivity_3days_avg.R: runs the sensitivity analysis. Needs utils.R.
  - utils.R: contains the functions to create the basis for the non-linear interaction and project new data on the defined basis.
  - tab1.R: creates Table 1 and Figure 1.
  - plot_or_nonlin.R: creates Figure 2.
  
RStudio version: 4.2.3
