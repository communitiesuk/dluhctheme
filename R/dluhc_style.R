#' ggplot function to create standard background and axis labels for a DLUHC style graph
#'
#' @param size a variable used to edit the text size. This is not the font size, but works as a multiplier for font size, default is 2
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' df <- dluhctheme::Net_Additions
#' ggplot(df, aes(x = year, y=lifeExp))
#' + dluhc_style()
dluhc_style <- function(size = 2) {

    ggplot2::theme(

    #Text format:
    #This sets the title to a blank, this can be overridden by theme code later on in the ggplot call
    plot.title = ggplot2::element_text(size = size*11,face = "bold"),

    #This sets the subtitle to a blank, this can be overridden by theme code later on in the ggplot call
    plot.subtitle = ggplot2::element_text(size = size*9),

    plot.caption = ggplot2::element_blank(),
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function

    #Legend format
    #This sets the position and alignment of the legend, removes a title and backround for it and sets the requirements for any text within the legend.
    #The legend may often need some more manual tweaking when it comes to its exact position based on the plot coordinates.
    legend.position = "top",

    legend.text.align = 0,

    legend.background = ggplot2::element_blank(),

    legend.title = ggplot2::element_blank(),

    legend.key = ggplot2::element_blank(),

    legend.text = ggplot2::element_text(
                                        size=size*9,
                                        color="#222222"),

    #Axis format
    #This sets the text font, size and colour for the axis test, as well as setting the margins and removes lines and ticks.
    #In some cases, axis lines and axis ticks are things we would want to have in the chart - the cookbook shows examples of how to do so.

    axis.title = ggplot2::element_blank(),

    axis.text = ggplot2::element_text(
                                      size=size*9,
                                      color="#222222"),

    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),

    axis.ticks = ggplot2::element_line(color = "#cbcbcb"),

    axis.line = ggplot2::element_blank(),

    #Grid lines
    #This removes all minor gridlines and adds major y gridlines. In many cases you will want to change this to remove y gridlines and add x gridlines. The cookbook shows you examples for doing so

    panel.grid.minor = ggplot2::element_blank(),

    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),

    panel.grid.major.x = ggplot2::element_blank(),

    #Blank background
    #This sets the panel background as blank, removing the standard grey ggplot background colour from the plot
    panel.background = ggplot2::element_blank(),

    #Strip background (#This sets the panel background for facet-wrapped plots to white, removing the standard grey ggplot background colour and sets the title size of the facet-wrap title to font size 22)
    strip.background = ggplot2::element_rect(fill="white"),

    strip.text = ggplot2::element_text(size  = size*11,  hjust = 0)
  )
}
