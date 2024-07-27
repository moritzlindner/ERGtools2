<!-- README.md is generated from README.Rmd. Please edit that file -->

# ERGtools2

<!-- badges: start -->
<!-- badges: end -->

The ERGtools2 package is an environment for working with
electroretinogram data. It contains an import method for Diagnosys
Espion data, but allows reading in of data coming in other formats with
limited coding effort. Standard procedures like averaging, sub-setting
an visualization of individual exams are supported.

## Installation

You can install the development version of ERGtools2 from
[GitHub](https://github.com/) with:

    if (!requireNamespace("remotes", quietly = TRUE)){
      install.packages("remotes")
    }
    remotes::install_github("moritzlindner/ERGtools2")

Note that `ERGtools2` depends on the github-deposited R Packages
`EPhysData` and `EPhysMethods`. Installation usually works
automatically. Updating, however may fail. If this is the case, update
manually using the following line of code:

    remotes::install_github("moritzlindner/EPhysData")
    remotes::install_github("moritzlindner/EPhysMethods")

## Example

This is a basic example which shows you how to solve a common problem:

    library(ERGtools2)

    ## a typical workflow
    data(ERG) # load example data, to import own data, see the examples provided for newERGExam and ImportEspion
    StimulusTable(ERG) # have a look whats inside
    Metadata(ERG)
    ERG<-ClearMeasurements(ERG) # Clear Measuremnts that migth be alredy in object
    ERG<-SetStandardFunctions(ERG)
    ggERGTrace(ERG, where = list( Step = as.integer(3), Eye = "RE", Channel ="ERG", Result = as.integer(1))) # pick one and have a look at the traces as imported

<img src="man/figures/README-example-1.png" width="100%" />


    ERG <- AutoPlaceMarkers(ERG, Channel.names = pairlist(ERG = "ERG_auto")) # automatically place markers
    ggERGTrace(ERG, where = list( Step = as.integer(3), Eye = "RE", Channel ="ERG", Result = as.integer(1))) # pick one and have a look at the traces as imported

<img src="man/figures/README-example-2.png" width="100%" />
