# ESR Temperature Plots

## Instructions

This repository is used to create the Aleutian Islands regional temperature plots and Gulf of Alaska anomaly plot. The [make.esr.temperature.data](make.esr.temperature.R) function is used to produce the data required for these plots, though it is still under development. Both this function and the script for the plots should be run in the [create_ESR_temps](create_ESR_temps.R) file, following the necessary updates to the function:

1.  Create a new branch for the new survey year using the convention "survey" - "year".
2.  For Aleutian Islands years, use "inpfc_area" instead of "subregion" on lines 99, 101, 115, 187. Add "stratum" and "stationid" on lines 99 and 101.
3.  For the Gulf of Alaska, use "subregion" instead of "inpfc_area" on lines 99, 101, 115, 187. Remove "stratum" and "stationid" on lines 99 and 101.

The data output is found in temperature_summary.csv. Final plots should be placed in the appropriate "plots" sub-folder. All additional scripts used in previous temperature ESR contributions are found in the archive folder.
