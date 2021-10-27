
# {RankinLabTools} Package
## Overview

RankinLabTools is a set of functions and tools dedicated to reading, cleaning, and scoring data for many of the tests and sets of data in use by the Rankin lab at the UCSF Memory and Aging Center.


### Social Function Dataset Functions

One of the lab's internal projects, compiling all of our social function tests and questionnaires into a master data-set, necessitated a number of custom functions for filtering data, joining data on inexact keys, and other data cleaning tasks specific to our data and internal database. 


### Qualtrics Functions

Our lab leverages Qualtrics for a large portion of our human subjects data collection initiative. Qualtrics offers an API for downloading the collected data, and this package includes wrapper functions of the {qualtRics} package to directly query the UCSF instance of Qualtrics. Run `vignette("UCSF_Qualtrics", package="RankinLabTools")` for details on connecting.


### NACC Functions

At current, there is a standalone function for calculating the global CDR`r sprintf("\U00AE")` plus NACC FTLD score as described by Miyagawa et al. (2020). Run `vignette("NACC", package="RankinLabTools")` for details on how this function is intended to be used.

This section is in a questioning state as it is not yet clear that other NACC-related functions will be needed.
