#' Title
#'
#' @param .data A dataframe containing your data, ideally a 3 column dataframe with one value column and two categorical columns
#' @param xcol The column which contains the variable to go on the x axis of the column chart
#' @param ycol The column which contains the value to go on the y axis
#' @param groupcol The column which contains the categorical variable which the graphs will be faceted by
#' @param textsize The text size, initially set to 1, the size is a ratio to the normal dluhc_style size of 2
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' df <- dluhctheme::Net_Additions_Regional
#' df <- dplyr::filter(df,Regions %in% c("North East","North West","South East","South West","London", "East Midlands"))
#' df$year <- substr(df$Year,7,10)
#' facet_barchart(df,xcol = year,ycol = Net_Additions,groupcol=Region)
facet_barchart <- function(.data,xcol,ycol,groupcol,textsize=1){
.data <- dplyr::mutate(.data,variable = {{groupcol}})

ggplot2::ggplot(data = .data,ggplot2::aes(x={{xcol}},y={{ycol}},fill=variable)) +
  ggplot2::geom_bar(stat="identity") +
  ggplot2::facet_wrap(~factor(variable), scales='fixed',strip.position="top") +
  dluhctheme::dluhc_style(size = textsize) +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_fill_manual(values= c("#012169","#7B2876","#C5406E","#F4745D","#FFB454","#F9F871"))
}

