# `NOAA`: Functions for processing and visualizing earthquake dataset

This package operates on dataset obtained from the U.S. National Oceanographic and Atmospheric Administration (NOAA) on significant earthquakes around the world.
This package has several functions for processing and filtering of the earthquake dataset: `eq_location_clean()`,
`eq_clean_data()`,
`filter_date_and_country()`,
`eq_create_label()`.
Functions for earthquake dataset visualization include: `geom_timeline()`, 
`geom_timeline_label()`,
`eq_map()`.

The package was created for Coursera assignment in "Mastering Software Development in R Capstone" course. The most recent updates can be installed via the `devtools` package using:

```
devtools::install_github("androsova/NOAA")
```
