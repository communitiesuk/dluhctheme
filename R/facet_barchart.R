#' Title
#'
#' @param .data The dataframe used for the analysis, ideally a 3 column dataframe
#' @param xcol The column which one of the grouping variables will be in, this could be numeric or categorical
#' @param valuecol the value column which contains a numeric variables
#' @param groupcol the column which contains the category by which the graphs will be faceted
#' @param textsize The text size as a ratio:2 to the standard dluhc style text size, set to 1 by default
#'
#' @return
#' @export
#'
#' @examples
facet_barchart <- function(.data,xcol,valuecol,groupcol,textsize=1){
.data <- mutate(.data,variable = {{groupcol}})

ggplot(data = .data,aes(x={{xcol}},y={{valuecol}},fill=variable)) +
  geom_bar(stat="identity") +
  facet_wrap(~factor(variable), scales='fixed',strip.position="top") +
    dluhctheme::dluhc_style(size = textsize) +
  theme(legend.position = "none") +
  scale_fill_manual = c("#012169","blue","orange","purple","black","red","green")
}

