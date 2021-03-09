### Overview
"graphdata" is a small R package that is used to transform line item purchase data into an iGraph object for network analysis.
-graph_data() creates an iGraph object.
___
### Motivation
This package was developed in conjunction for the final project in BANA-277, Customer and Social Analytics, at the University of California,
Irvine Paul Merage School of Business. For our final project, we performed a network analysis on a large amount of data from an online
retail store. Through previous assignments, we were familiar with using iGraph, but found ourselves at a roadblock: our data was in the original
form, and needed to be transformed to be properly used with iGraph.
___
### Installation
Install the development version from GitHub via the package "devtools":
    
    # development version from GitHub:
    #install.packages("devtools") 

    # install "roller" (without vignettes)
    devtools::install_github("matthiasronnau/graphdata")
