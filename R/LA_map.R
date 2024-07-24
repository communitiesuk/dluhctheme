
#' Create a standard theme map from an sf object
#'
#' @param .data An sf object which has data broken down by local authority. This function only works for data in local authority breakdown only.
#' @param variable The column in the .data which has the numeric value to be mapped
#' @param LA_col The column which contains the Local Authority code
#' @param map_colours The colour which you want to represent the low and high values, must be in vector form
#' @param year The financial year which the LA codes you are using relate to, given in quotations
#' @param countries The countries which you wish to appear on the map. These are E, E+W, GB and UK for England, England+Wales, Great Britain and United Kingdom respectively
#' @param save A TRUE/FALSE statement if you want the file to be exported as a png file. If TRUE, the filepath must be defined
#' @param filepath If save is TRUE, filepath is the save location of the png output must be in the form of quotations
#' @param legendtitle The text to appear as the title for the legend on the map, set to NULL unless specified
#' @param title The map title, in black bold font. Set to NULL unless specified
#' @param subtitle The map subtitile in smaller grey font than the title. Set to NULL unless specified
#' @param titlesize A ratio value to increase or decrease the title and subtitle. Initially set to 1
#' @return a ggplot map, or a png file if save is TRUE
#' @export
#'
#' @examples
#' df <- dluhctheme::Help_to_Buy
#' a <- LA_map(df,variable = Completions,LA_col = LA_Code,year = "2020-21",countries = "E",save = TRUE,filepath = "Image.png",title="Help to Buy is popular near London",legendtitle = "Completions",titlesize=2)
#' finalise_plot(a,source_name = "Source: Help to Buy Equity Loan Statistics DLUHC",save_filepath = "outputmap.png",footerfontsize = 13,width_pixels = 560,height_pixels = 640,logo_nudge = -0.03)

LA_map <- function(.data,variable,legendtitle = NULL,LA_col,title = NULL, subtitle = NULL,titlesize=1, map_colours = c("#FFFFFF","#012169"),year = "2021-22",countries = "E",save = FALSE,filepath = NULL){


    if(countries %in% c("E","E+W","GB","UK")==FALSE){
    stop("The country variable you supplied is invalid. It must be one of:
         E, E+W, GB, UK. These represent England, England and Wales, Great Britain and United Kingdom respectively")
  }



  codes_match <-
    data.frame(
      country = c("E","E+W","GB","UK"),
      codes = c("E0","E0|W","E0|W|S","E0|W|S|N"),
      x_adjust = c(0.75,0.76,0.68,0.68),
      y_adjust = c(0.54,0.52,0.3,0.3),
      width = c(0.2,0.2,0.15,0.15),
      height = c(0.2,0.2,0.15,0.15),
      legendx = c(0.29,0.23,0.1,0.1),
      legendy = c(0.44,0.75,0.27,0.27)
      )

  map_match <-
    data.frame(
      year = c("2020-21","2021-22","2022-23","2023-24"),
      filenames = c("LAD_Dec2020_BUC","LAD_May2021_BUC","LAD_Dec2022_BUC","LAD_Dec2023_BUC"),
      LA_Column = c("LAD20CD","LAD21CD","LAD22CD","LAD23CD")
    )

  if(!(year %in% map_match$year)){
    stop("The year you have selected is outside the range of maps available. Please contact the package owner to update the maps available")
  }


  rawdata_LA_Col <- .data |>
  dplyr::pull({{LA_col}})

  correct_LA_codes <- codes_match$codes[which(codes_match$country == countries)]

  if(any(stringr::str_detect(rawdata_LA_Col,correct_LA_codes))==FALSE){
    stop("The data you have provided does not have the local authority codes in the correct format for the countries you have selected")
  }


  if(year=="2020-21"){
    LA_map_data <- sf::st_as_sf(dluhctheme::LAD_Dec2020_BUC)
  }else if(year == "2021-22"){
    LA_map_data <- sf::st_as_sf(dluhctheme::LAD_May2021_BUC)
  }else if(year == "2022-23"){
     LA_map_data <- sf::st_as_sf(dluhctheme::LAD_Dec2022_BUC)
  }else{
    LA_map_data <- sf::st_as_sf(dluhctheme::LAD_Dec2023_BUC)
  }




  LA_map_colname <- map_match$LA_Column[which(map_match$year==year)]

  LA_map_data <- LA_map_data |>
    dplyr::rename("LA_Code"=all_of(LA_map_colname)) |>
    dplyr::filter(stringr::str_detect(LA_Code,codes_match$codes[which(codes_match$country==countries)]))

  original_rows <- nrow(.data)

  sfdata  <- .data |>
    dplyr::mutate(LA_Code = {{LA_col}}) |>
    dplyr::left_join(LA_map_data,., by = c("LA_Code")) |>
    sf::st_as_sf()

  matched_rows <- dplyr::inner_join( x = dplyr::mutate(.data,LA_Code = {{LA_col}}),
                                     y = as.data.frame(LA_map_data),
                                     by= c("LA_Code"))

  matched_rows = nrow(matched_rows)

  print(paste0("Your original data had ",original_rows," rows of data"))
  print(paste0("On the map, there were ",matched_rows," matches, from ",nrow(LA_map_data)," LAs on the map"))
  map <-
    ggplot2::ggplot(sfdata) +
    ggplot2::geom_sf(ggplot2::aes(fill = {{variable}}),colour = "black",linewidth=0.02) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_gradient(legendtitle,labels = scales::comma,low = map_colours[1],high = map_colours[2]) +
    ggplot2::ggtitle(label = title,subtitle = subtitle) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11*titlesize,face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9*titlesize),
      legend.position = c(codes_match$legendx[which(codes_match$country==countries)],
                          codes_match$legendy[which(codes_match$country==countries)]),
      legend.title = ggplot2::element_text(color = "black", face = "bold", vjust = 0.8),
      plot.margin = ggplot2::margin(10, 0, 10, 0)) #add space around the plot


  map_London <- sfdata |>
    dplyr::mutate(Code_temp = substr(LA_Code, 1,3))|>
    dplyr::filter(Code_temp == "E09")

  limits = sf::st_bbox(map_London)
  limits_adj <- limits

  limits_adj[1] <- limits[1] - abs(0.03*(limits[3]-limits[1]))
  limits_adj[2] <- limits[2] - abs(0.03*(limits[4]-limits[2]))
  limits_adj[3] <- limits[3] + abs(0.03*(limits[3]-limits[1]))
  limits_adj[4] <- limits[4] + abs(0.03*(limits[4]-limits[2]))


  map <- map +

    ggplot2::geom_rect( xmin = limits_adj[[1]],
               ymin = limits_adj[[2]],
               xmax = limits_adj[[3]],
               ymax = limits_adj[[4]],
               fill=NA,
               colour = "black",
               linewidth = 0.8)

  finalmap <- map |>
    cowplot::ggdraw() +
    cowplot::draw_plot(
      {
        map +
          ggplot2::coord_sf(   xlim = limits_adj[c(1,3)],
                      ylim = limits_adj[c(2,4)],
                      expand = FALSE) +
          ggplot2::theme(legend.position = "none",
                plot.title = ggplot2::element_blank(),
                plot.subtitle = ggplot2::element_blank())
      },
      x=codes_match$x_adjust[which(codes_match$country==countries)],
      y=codes_match$y_adjust[which(codes_match$country==countries)],
      width=codes_match$width[which(codes_match$country==countries)],
      height =codes_match$height[which(codes_match$country==countries)]
    )

  if(save){
    ggplot2::ggsave(finalmap,filename = filepath,width = 5,height = 5,units = "in")
  }
      return(finalmap)

}
