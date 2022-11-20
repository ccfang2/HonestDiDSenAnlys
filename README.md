# HonestDiD Sensitivity Analysis
> This is a shiny app that facilitates the sensitivity analysis proposed in [Rambachan and Roth (2022)](https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf) and is built on the R package: [HonestDiD](https://github.com/asheshrambachan/HonestDiD).

## About Sensitivity Analysis

This is a useful tool when you are uncertain about if parallel trends hold in your difference-in-difference setting. It reports confidence sets for causal effect of interest under a variety of possible restrictions on the underlying trends, and is helpful in telling the degree of violation on parallel trends that is needed to make a particular causal conclusion.

## Loading the app

There are two ways to use this shiny app, locally and remotely.

1. You could run this shiny app in [RStudio](https://posit.co) locally

```r
# install.packages("shiny") # install Shiny if not installed
library(shiny)
shiny::runGitHub("HonestDiDSenAnlys", "ccfang2")
```

2. Alternatively, you can access the online version of this shiny app [here](). Unfortunately, total usage duration could not be more than 25 hours per month as I use free versions of shiny apps.

## Introduction

### Screenshot
![Screenshot](https://user-images.githubusercontent.com/100428466/202920286-44f022f2-ca89-406f-8801-763e4df67519.gif)

### What does this app do for you
1. Sensitivity analysis for example papers discussed in [Rambachan and Roth (2022)](https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf)
2. Sensitivity analysis for your own data, if you already have estimated event-study coefficients and variance-covariance matrix
3. DiD estimation using two way fixed effects and [fixest](https://github.com/lrberge/fixest) package, if you haven't got estimated event-study coefficients and variance-covariance matrix yet and your DiD design is in non-staggered timing
4. DiD estimation using group-time average effects and [did](https://github.com/bcallaway11/did#difference-in-differences-) package, if you haven't got estimated event-study coefficients and variance-covariance matrix yet and your DiD design is in staggered timing

### Features
1. No need to install or configurate anything before using this app. This is user-friendly to non-R users. But it is highly recommended to read [Rambachan and Roth (2022)](https://jonathandroth.github.io/assets/files/HonestParallelTrends_Main.pdf) beforehand, so you will know all the terms that appear on the app.
2. R code is provided each time you run an analysis on this app. So, for R users, you could copy and paste the produced R code in your own R console, and will get exactly the same outputs as shown in the app. The produced R code makes your analysis more flexible as you can revise it to your own need.
3. Analysis results and plots are downloadable by a simple click of buttons. But, please be reminded that only the latest analysis results and plots are accessible, so download them timely if needed.

### Instruction
Please watch this Youtube [video]() for detailed instruction. This video is also embeded in this shiny app.

## Contact
I hope you enjoy using this app. Any comments or questions are welcome at [ccfang[at]uni-bonn.de](mailto:ccfang@uni-bonn.de).
