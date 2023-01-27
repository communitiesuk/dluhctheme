#' Highlighted faceted graphs in the dluhc style
#'
#' @param .data The dataframe used for the analysis, ideally a 3 column dataframe
#' @param datecol The column name which has the date variables in. Date needs to be in one of three formats: "2021-01-23", "23/01/2021" or "21-Jan-2021"
#' @param ycol The column name which has the numeric values, ensure these are in numeric format
#' @param groupcol The column name which will split the data into individual graphs
#' @param textsize A numeric variable for text size, this isnt the actual text size, but will scale the text size in the facets correctly
#' @param dateformat The format which the date is presented in using the standard R date format, see here for more detail https://www.statology.org/r-date-format/
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' df <- dplyr::filter(gapminder::gapminder,country %in% c("Austria","India","China","France")
#' facet_highlight_time_series(.data=df,datecol = year, ycol = lifeExp, groupcol = country, dateformat = "%Y")
facet_highlighted_time_series <-
  function(.data,datecol,ycol,groupcol,textsize = 1,dateformat="%Y-%m-%d"){

    library(gghighlight)

    is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

    if(any(is.convertible.to.date(pull(.data,{{datecol}}))==FALSE)){
      stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
    }

    if(any(replace_na(is.numeric(pull(.data,{{ycol}})),TRUE)==FALSE)){
      stop("Value column contains non-numeric values. Check your data and try again")
    }


    .data <- .data %>%
      mutate(Date = as.Date(as.character({{datecol}}),tryFormats = dateformat)) %>%
      mutate(value = {{ycol}}) %>%
      mutate(GROUP = {{groupcol}})

    ggplot(.data, mapping=aes(x=Date, y=value)) +
    geom_line(show.legend = FALSE, col = "#012169" , size = 1.5) +
    geom_line(aes(Date,value,colour=GROUP), colour = "#012169")+
    facet_wrap(~ GROUP, scales='free',strip.position="top") +
    gghighlight(use_direct_label = FALSE) +
    dluhctheme::dluhc_style(size = textsize)
  }
