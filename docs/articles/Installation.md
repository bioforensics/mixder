# Installation

For any installation, EuroForMix must be installed first. Please follow
the instructions from the [EuroForMix GitHub
page](https://github.com/oyvble/euroformix/). EuroForMix version 4.2.3
and earlier have been tested and are compatible with MixDeR. If using a
newer version, please be aware it has not been tested and errors may
occur!

If installing from GitHub:  
The R package `devtools` is required to install from GitHub:

    install.packages("devtools")
    devtools::install_github("bioforensics/mixder")

If installing from source, first install the following R packages:

    install.packages(c("dplyr", "ggplot2", "glue", "prompter", "readxl", "rlang", "shiny", "shinyFiles", "shinyjs", "tibble", "tidyr"))

To install MixDeR from source (i.e. the `mixder_0.1.0.tar.gz` file):

    install.packages("/path/to/mixder_0.1.0.tar.gz", repos = NULL, type="source")

For example, if the `mixder_0.1.0.tar.gz` file is located in your
Documents folder:

    install.packages("~/Documents/mixder_0.1.0.tar.gz", repos = NULL, type="source")
