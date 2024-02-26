#' Create a time series with more than one line in a DLUHC style
#'
#' @param .data A dataframe with the data in a long format with one column for the date and one column for the variable value
#' @param datecol The column name of the dataframe which contains the date variable
#' @param ycol The column name of the dataframe which contains the value
#' @param groupcol The column name which contains the grouping variable (such as country, region or type)
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' df <- dluhctheme::Social_Housing_Sales
#'
#' multi_line_timeseries(.data=df,datecol = year, ycol = count, groupcol = type, dateformat = "%d/%m/%Y")

multi_line_timeseries <- function(.data,datecol,ycol,groupcol,dateformat = "%Y-%m-%d"){

  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

  if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
    }

  if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }


  .data <- .data |>
    dplyr::mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) |>
    dplyr::mutate(value = {{ycol}}) |>
    dplyr::mutate(variable = {{groupcol}})

  variable_count <- length(unique(.data$variable))


  graph <- .data |>
    ggplot2::ggplot(ggplot2::aes(x = Date,y = value)) +
    ggplot2::geom_line(size = 1, ggplot2::aes(color = variable)) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    dluhctheme::dluhc_style()

  if(variable_count < 6){
    graph <- graph +
      ggplot2::scale_color_manual(values=dluhc_palettes$categorical[1:variable_count])
  }else{
    stop("This function only allows for plotting up to 6 categories (lines) on one graph. If your grouping variable has more than 5 categories, it is suggested you use the facet_timeseries or facet_highlight_timeseries function")
  }

  graph

}
