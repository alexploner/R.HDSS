# R.HDSS
An R package to (pre-) process HDSS demographic data from the INDEPTH network

This is a collection of R functions to work with demographic datasets curated by the [INDEPTH network](www.indepth-network.org/#) 
and available from the [INDEPTH data repository](http://www.indepth-ishare.org/index.php/home). HDSS (health and demographic 
surveillance system) data files can be read into R and pre-processed, their content can be checked for quality issues, and events, 
event patterns and transitions can be extracted and summarised before further processing.

# Getting started

To install `R.HDSS` from github, you need the `devtools` package in your R environment. If you have not done so yet, start a fresh
R session and run 
```
install.packages("devtools")
```
This will install the developer tools and their dependencies. Now you can easily install `R.HDSS` via:
```
devtools::install_github("alexploner/R.HDSS")
```
Load the package and open its vignette to get an overview and demonstration of the basic use of `R.HDSS`.

# Disclaimer

I am not affiliated with the INDEPTH network.
