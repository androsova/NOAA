context("Test performance of the NOAA functions")

library(dplyr)
library(readr)
library(ggplot2)
library(leaflet)
library(lubridate)

NOAA_url = "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
NOAA_clean_data = eq_clean_data(NOAA_url)

test_that("eq_location_clean() performs correctly", {
  expect_is(eq_location_clean(suppressMessages({readr::read_delim(NOAA_url,
                                                                  delim = "\t",
                                                                  progress = FALSE)})), "tbl_df")
})

test_that("eq_clean_data() performs correctly", {
  expect_is(eq_clean_data(NOAA_url), "tbl_df")
})

test_that("filter_date_and_country() performs correctly", {
  expect_is(filter_date_and_country(NOAA_clean_data,
                                    xmindate = "1999-01-01",
                                    xmaxdate = "2017-12-31",
                                    country = c("CHINA", "USA")), "tbl_df")
})

test_that("eq_create_label() performs correctly", {
  expect_is(NOAA_clean_data %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
              dplyr::mutate(popup_text = eq_create_label(.)), "tbl_df")
})

test_that("geom_timeline() performs correctly", {
  expect_is(geom_timeline(NOAA_clean_data,
                          xmindate = "1999-01-01",
                          xmaxdate = "2017-12-31",
                          country = c("CHINA", "USA", "JAPAN")), c("gg", "ggplot"))
})

test_that("geom_timeline_label() performs correctly", {
  expect_is(geom_timeline_label(NOAA_clean_data,
                                xmindate = "1999-01-01",
                                xmaxdate = "2017-12-31",
                                country = c("CHINA", "USA"),
                                n_max = 7.2), c("gg", "ggplot"))
})

test_that("eq_map() performs correctly", {
  expect_is(NOAA_clean_data %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
              eq_map(annot_col = "DATE"), c("gg", "ggplot"))
  expect_is(NOAA_clean_data %>%
              dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
              dplyr::mutate(popup_text = eq_create_label(.)) %>%
              eq_map(annot_col = "popup_text"), c("gg", "ggplot"))
})
