#' Create a single line time series graph in a dluhc theme for a line which has a predicted/forecast value
#'
#' @param .data A dataframe in long format with 2 columns necesarry: Date and value
#' @param datecol The column name which contains the date value in a widely used date format
#' @param ycol The column name which contains the values
#' @param cutdate The date which the predicted values begin from
#' @param dottedline A TRUE/FALSE statement to decide if you want a vertical dotted line on the graph to split the prediction and the recorded values
#' @param label_names A vector containing the 2 words you want as the label for your lines, the default is c("Recorded", "Forecast")
#' @param dateformat The format which the date is presented in using the standard R date format, see here for more detail https://www.statology.org/r-date-format/
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' df <- dluhctheme::OBR_Forecast
#' forecast_timeseries(df,Date,ycol=Inflation,cutdate = "01/10/2022",dateformat = "%d/%m/%Y", label_names = c("Actual","Predicted"))
forecast_timeseries <- function(.data,datecol,ycol,cutdate,dateformat="%Y-%m-%d",dottedline=TRUE,label_names = c("Recorded","Forecast")){

  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

  if(any(is.convertible.to.date(dplyr::pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
  }

  if(any(tidyr::replace_na(is.numeric(dplyr::pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }

  if(is.convertible.to.date(cutdate)==FALSE){
    stop("Cut date not in format specified. Check the dateformat argument in the function")
  }

  cutdate = as.Date(cutdate,tryFormats = dateformat)

.data <- .data |>
    dplyr::mutate(Date = as.Date({{datecol}},tryFormats = dateformat)) |>
  dplyr::mutate(value = {{ycol}}) |>
  dplyr::mutate(prediction = ifelse(Date>=cutdate,label_names[2],label_names[1]))

duplicate <- .data |>
  dplyr::mutate(check = ifelse(prediction!=dplyr::lead(prediction),1,0)) |>
  dplyr::filter(check == 1) |>
  dplyr::mutate(prediction = label_names[2]) |>
  dplyr::select(!check)

.data <- rbind(.data,duplicate)


  a <- ggplot2::ggplot(data = .data,ggplot2::aes(x = Date,y = value)) +
    ggplot2::geom_line(ggplot2::aes(linetype = factor(prediction,levels = c(label_names[1],label_names[2]))),size = 1.5, color = "#012169") +
    dluhctheme::dluhc_style() +
    ggplot2::scale_linetype_manual(values = c("solid","dashed")) +
    ggplot2::theme(legend.key.width = ggplot2::unit(2,"cm"))

  if(dottedline){
    a <- a+
      ggplot2::geom_vline(xintercept = cutdate,linetype="dotted",size=1)
  }
  a
}
