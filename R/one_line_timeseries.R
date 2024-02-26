#' Create a single line time series graph in a dluhc theme
#'
#' @param .data A dataframe in long format with 2 columns necessary: Date and value
#' @param datecol The column name which contains the date value in a widely used date format
#' @param ycol The column name which contains the values
#' @param dateformat is the format which the date is presented in using the standard R date format, see here for more detail https://www.statology.org/r-date-format/
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' df <- dluhctheme::Social_Housing_Sales
#' df <- dplyr::filter(df,type == "Right to Buy")
#'
#' one_line_timeseries(.data=df,datecol = year, ycol = count, dateformat = "%d/%m/%Y")

one_line_timeseries <- function(.data,datecol,ycol,dateformat="%Y-%m-%d"){

  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

  if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
  }

  if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }


  .data <- .data |>
    dplyr::mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) |>
    dplyr::mutate(value = {{ycol}})

  ggplot2::ggplot(data = .data,ggplot2::aes(x = Date,y = value)) +
    ggplot2::geom_line(size = 1.5, color = dluhc_blue) +
    dluhctheme::dluhc_style()
}
