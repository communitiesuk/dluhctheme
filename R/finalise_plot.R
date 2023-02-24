#' Arrange alignment and save DLUHC ggplot chart
#'
#' It will left align your source, add the DLUHC logo at the bottom right and save it to your specified location.
#' @param plot_name The variable name of the plot you have created that you want to format and save
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param save_filepath Exact filepath that you want the plot to be saved to
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to DLUHC image that sits within the data folder of your package
#' @param footerfontsize Size of the font used in the footer
#' @param logo_nudge A numeric value to move the logo left or right with, initially set to 0. Negative decimal values will move it left
#' @return (Invisibly) an updated ggplot object.
#' @examples
#' df <- data.frame(replicate(2,sample(1:19,100,rep=TRUE)))
#' myplot <- ggplot2::ggplot(data = df,ggplot2::aes(x = X1,y=X2)) +
#' ggplot2::geom_point() +
#' dluhctheme::dluhc_style() +
#' ggplot2::scale_y_continuous(expand=c(0,0), limits = c(0,20))
#'
#' finalise_plot(plot_name = myplot,
#' source = "Source: The source for my data",
#' save_filepath = "filename_that_my_plot_should_be_saved_to-nc.png",
#' width_pixels = 640,
#' height_pixels = 450,
#' footerfontsize = 18,
#' save = TRUE
#' )
#'
finalise_plot <- function(plot_name,
                          source_name,
                          save_filepath=file.path(Sys.getenv("TMPDIR"), "tmp-nc.png"),
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path = file.path(system.file("data", package = 'dluhctheme'),"DLUHC_Logo.png"),
                          footerfontsize = 18,
                          save = TRUE,
                          logo_nudge = 0) {

  save_plot <- function (plot_grid, width, height, save_filepath) {
    grid::grid.draw(plot_grid)
    #save it
    ggplot2::ggsave(filename = save_filepath,
                    plot=plot_grid, width=(width/96), height=(height/96), dpi = 96,  bg="white")
  }

  #Left align text
  left_align <- function(plot_name, pieces){
    grob <- ggplot2::ggplotGrob(plot_name)
    n <- length(pieces)
    grob$layout$l[grob$layout$name %in% pieces] <- 2
    return(grob)
  }

  create_footer <- function (source_name, logo_image_path,footerfontsize = 18) {
    #Make the footer
    footer <- grid::grobTree(grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                             grid::textGrob(source_name,
                                            x = 0.004, hjust = 0, gp = grid::gpar(fontsize=footerfontsize)),
                             grid::rasterGrob(png::readPNG(logo_image_path), x = 0.89+logo_nudge))
    return(footer)

  }

  footer <- create_footer(source_name, logo_image_path, footerfontsize)

  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.1/(height_pixels/450)))
  plot_grid
  ## print(paste("Saving to", save_filepath))
  if(save==TRUE){save_plot(plot_grid, width_pixels, height_pixels, save_filepath)}
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
}
