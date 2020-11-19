# NVCO

Nonviolent campaigns and outcomes (NVCO) is a subset of the Nonviolent and Violent campaigns and outcomes (NAVCO) dataset from Harvard University. The project obtained data on 323 major campaigns from 1900 to 2006. This dataset was subset in order to create models pertaining to nonviolent protest. Of the original 142 variables, 11 potential variables were selected in the creation of this app.

<br>

The purpose of this app is to explore the data set and some possible prediction models for the response variable, `success`. The required packages are described below.

<br>

- `shiny`: This package is required to create shiny apps, the framework this app is built in.
- `shinydashboard`: This package builds upon the shiny framework and allows for creations of other structures such as tabs.
- `readxl`: This package contains the function `read_xls()`, which is used to import Microsoft Excel data, which was the native form of the NAVCO data set.
- `tidyverse`: This package contains all of the tidy-based packages. Although they have many uses, in this project they were mainly used for data manipulation.
- `naniar`: This package is useful for handling missing values. The NAVCO data set uses `-99` to indicate missing. These functions were used to replace these values to `NA`.
- `ggplot2`: This package contains more powerful graphing functions.
- `plotly`: This package contains wrapper functions that build upon `ggplot2` functions. It allows for, among other things, interactive graphs.
- `caret`: This package contains functions for different cross-validation techniques and model fitting.
- `mathjaxr`: This package allows for displaying mathematical formulas and symbols in R output.

<br>

To install all of these packages, run the following code:

<br>

`install.packages(c('shiny', 'shinydashboard', 'readxl', 'tidyverse', 'naniar', 'ggplot2', 'plotly', 'caret', 'mathjaxr'))`

<br>

To run the app, enter the following code into an R console:

<br>

`shiny::runGitHub('NVCO', 'danielbhaines', ref='main')`
