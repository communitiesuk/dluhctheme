#' Create a single line time series graph in a dluhc theme for a line which has a predicted/forecast value
#'
#' @param .data A dataframe in long format with 2 columns necesarry: Date and value
#' @param xcol The column name which contains the date value in a widely used date format
#' @param ycol The column name which contains the values
#' @param cutdate The date which the predicted values begin from
#' @param dottedline A TRUE/FALSE statement to decide if you want a vertical dotted line on the graph to split the prediction and the recorded values
#' @param label_names A vector containing the 2 words you want as the label for your lines, the default is c("Predicted", "Actual")
#' @param dateformat The format which the date is presented in using the standard R date format, see here for more detail https://www.statology.org/r-date-format/
#' @return a ggplot2 object
#' @export
#'
#' @examples
#' df <- dluhctheme::GDP_Prediction
#' forecast_timeseries(df,year,ycol=GDP,cutdate = "31/03/2022",dateformat = "%d/%m/%Y", label_names = c("Forecast","Actual"))
forecast_timeseries <- function(.data,xcol,ycol,cutdate,dateformat="%Y-%m-%d",dottedline=TRUE,label_names = c("Predicted","Actual")){
  library(tidyverse)

  is.convertible.to.date <- function(x) !is.na(as.Date(as.character(x), tz = 'UTC', format = dateformat))

  if(any(is.convertible.to.date(pull(.data,{{datecol}}))==FALSE)){
    stop("Date column not in format specified, or contains some NA values. Check the dateformat argument in the function")
  }

  if(any(replace_na(is.numeric(pull(.data,{{ycol}})),TRUE)==FALSE)){
    stop("Value column contains non-numeric values. Check your data and try again")
  }

  if(is.convertible.to.date(cutdate)==FALSE){
    stop("Cut date not in format specified. Check the dateformat argument in the function")
  }

  cutdate = as.Date(cutdate,tryFormats = dateformat)

.data <- .data %>%
    mutate(Date = as.Date({{xcol}},tryFormats = dateformat)) %>%
    mutate(value = {{ycol}}) %>%
  mutate(prediction = ifelse(Date>=cutdate,label_names[1],label_names[2]))

duplicate <- .data %>%
  mutate(check = ifelse(prediction!=lead(prediction),1,0)) %>%
  filter(check == 1) %>%
  mutate(prediction = label_names[1]) %>%
  select(!check)

.data <- rbind(.data,duplicate)


  a <- ggplot2::ggplot(data = .data,aes(x = Date,y = value)) +
    geom_line(aes(linetype = factor(prediction,levels = c(label_names[1],label_names[2]))),size = 1.5, color = "#012169") +
    dluhctheme::dluhc_style() +
    scale_linetype_manual(values = c("dashed","solid")) +
    theme(legend.key.width = unit(2,"cm"))

  if(dottedline){
    a <- a+
      geom_vline(xintercept = cutdate,linetype="dotted",size=1)
  }
  a
}
