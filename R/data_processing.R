#' Polish LOCATION_NAME column in the significant earthquake data
#'
#' This function a data.frame of the entire significant earthquake
#' data \code{data}, cleans the LOCATION_NAME column by stripping out
#' the country name (including the colon) and converts names to title
#' case (as opposed to all caps).
#'
#' @param data A tibble data.frame with LOCATION_NAME column that has to be cleaned
#'
#' @return This function returns a tibble (data.frame)
#'
#' @import dplyr
#'
#' @examples
#' NOAA_url = "https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt"
#' data <- suppressMessages({readr::read_delim(NOAA_url, delim = "\t", progress = FALSE)})
#' eq_location_clean(data)
#'
#' @export
eq_location_clean = function(data){
  data %>%
    dplyr::mutate(LOCATION_NAME = sapply(LOCATION_NAME, function(x){
      x = gsub("\\]|\\)", "", gsub("^.+:[ | ]+", "", x))
      x = gsub(" \\(| \\[", ", ", x)
      splited = strsplit(x, " ")[[1]]
      splited = paste(substring(splited, 1, 1), tolower(substring(splited, 2)),
                      sep="", collapse=" ")
      if(grepl("\\[|\\(|,|;", splited)){
        splited = strsplit(splited, "\\[|\\(|,|;( )*")[[1]]
        splited = paste(toupper(substring(splited, 1, 1)), substring(splited, 2),
                  sep="", collapse=", ")
      }
      gsub(", , ", ", ", splited)
    }))
}

#' Load NOAA significant earthquake data into a tibble
#'
#' This function reads tab-delimited file of the entire significant earthquake
#' data available at \code{NOAA_url} from U.S. National Oceanographic
#' and Atmospheric Administration (NOAA) database and returns a tibble (data.frame).
#' After reading the columns YEAR, MONTH and DAY are merged into column named
#' DATE which contains Date class entries.
#'
#' @param NOAA_url A character string giving the URL path to the significant
#' earthquake data file
#'
#' @return This function returns a tibble (data.frame) of the input file
#'
#' @note This function will give error if the file path is incorrect or
#' file does not exist
#'
#' @importFrom readr read_delim
#' @import dplyr
#'
#' @examples
#' NOAA_clean_data = eq_clean_data("https://www.ngdc.noaa.gov/nndc/struts/results?type_0=Exact&query_0=$ID&t=101650&s=13&d=189&dfn=signif.txt")
#'
#' @export
eq_clean_data <- function(NOAA_url) {
  raw_data <- suppressMessages({
    readr::read_delim(NOAA_url, delim = "\t", progress = FALSE)
  })
  processed_data = raw_data %>%
    dplyr::filter(YEAR >= 1900) %>%
    tidyr::unite(DATE, YEAR, MONTH, DAY, sep = "-") %>%
    dplyr::mutate(DATE = as.Date(DATE, format = "%Y-%m-%d", origin = '1900-1-1')) %>%
    dplyr::filter(!is.na(DATE) & LATITUDE != "       ") %>%
    dplyr::mutate(LATITUDE = as.numeric(LATITUDE),
                  LONGITUDE = as.numeric(LONGITUDE))

  eq_location_clean(processed_data)
}
