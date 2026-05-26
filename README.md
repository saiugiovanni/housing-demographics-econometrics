# Housing Demographics Econometrics

This repository contains a university econometrics project developed during my MSc in Mathematical Engineering.

The project analyses the relationship between US house prices, demographic dynamics and macroeconomic variables during the period 2000–2011, with particular attention to the effects of migration flows, GDP, CPI and the 2007–2008 financial crisis.

The analysis combines econometric modelling, time series techniques, hypothesis testing and dimensionality reduction methods using R.

## Main objectives

- Analyse the relationship between demographic variables and house prices
- Study the impact of migration flows and population growth on housing dynamics
- Investigate macroeconomic drivers such as GDP and CPI
- Detect structural breaks related to the financial crisis
- Build regression and autoregressive econometric models
- Perform diagnostic tests on residuals and model assumptions
- Analyse time series behaviour and autocorrelation
- Apply Principal Component Analysis (PCA) for dimensionality reduction

## Repository structure

- `econometric_analysis.R`  
  Main script containing the econometric analysis and statistical models.

- `exploratory_analysis.R`  
  Initial exploratory data analysis and visualisation.

- `population_estimates_usa.xlsx`  
  Dataset used in the analysis.

- `US_Housing_Demographics_Report.pdf`  
  Final report containing methodology, statistical results and conclusions.

## Main analyses

### Linear regression models

Construction of regression models linking population growth and house prices to demographic and macroeconomic variables such as births, deaths, migration flows, GDP and CPI.

### Structural break analysis

Implementation of structural change tests to detect breaks associated with the 2007–2008 financial crisis.

### Time series analysis

Application of autoregressive models, lag analysis and dynamic regressions for house prices and population growth.

### Residual diagnostics

Implementation of:

- Breusch-Pagan tests
- Durbin-Watson tests
- Breusch-Godfrey tests
- Box-Ljung and Box-Pierce tests

to analyse homoscedasticity and autocorrelation properties.

### Principal Component Analysis (PCA)

Dimensionality reduction analysis showing that the first two principal components explain most of the total variability in the dataset.

## Technologies used

- R
- Econometric modelling
- Time series analysis
- Statistical hypothesis testing
- Principal Component Analysis (PCA)
- Data visualisation

## How to run the code

Open R or RStudio and set the working directory to the project folder.

Then run:

```r
source("econometric_analysis.R")
