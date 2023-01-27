
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
#'
#' @return a ggplot map, or a png file if save is TRUE
#' @export
#'
#' @examples
#' df <- dluhctheme::Help_to_Buy
#' LA_map(df,variable = Completions,LA_col = LA_Code,year = "2020-21",countries = "E",save = TRUE, filepath = "Help_to_Buy_map.png")
LA_map <- function(.data,variable,LA_col,map_colours = c("#FFFFFF","#012169"),year = 2022,countries = "E",save = FALSE,filepath = NULL){

  library(tidyverse)
  library(sf)


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
      year = c("2020-21","2021-22","2022-23"),
      filenames = c("LAD_Dec2020_BUC","LAD_May2021_BUC","LAD_Dec2022_BUC"),
      LA_Column = c("LAD20CD","LAD21CD","LAD22CD")
    )

  if(!(year %in% map_match$year)){
    stop("The year you have selected is outside the range of maps available. Please contact the package owner to update the maps available")
  }

  rawdata_LA_Col <- .data %>%
  pull({{LA_col}})

  correct_LA_codes <- codes_match$codes[which(codes_match$country == countries)]

  if(any(str_detect(rawdata_LA_Col,correct_LA_codes))==FALSE){
    stop("The data you have provided does not have the local authority codes in the correct format for the countries you have selected")
  }

  library(cowplot)

  if(year==2020){
    data("LAD_Dec2020_BUC")
    LA_map_data <- LAD_Dec2020_BUC
  }else if(year == 2021){
    data("LAD_May2021_BUC")
    LA_map_data <- LAD_May2021_BUC

  }else{
    data("LAD_Dec2022_BUC")
    LA_map_data <- LAD_Dec2022_BUC
  }




  LA_map_colname <- map_match$LA_Column[which(map_match$year==year)]

  LA_map_data <- LA_map_data %>%
    rename("LA_Code"=all_of(LA_map_colname)) %>%
    filter(str_detect(LA_Code,codes_match$codes[which(codes_match$country==countries)]))

  original_rows <- nrow(.data)

  .data  <- .data %>%
    mutate(LA_Code = {{LA_col}}) %>%
    left_join(LA_map_data,., by = c("LA_Code"))

  matched_rows <- nrow(inner_join(.data,as.data.frame(LA_map_data),by=c("LA_Code")))

  print(paste0("Your original data had ",original_rows," rows of data"))
  print(paste0("On the map, there were ",matched_rows," matches"))
  map <-
    ggplot(.data) +
    geom_sf(aes(fill = {{variable}}),colour = "black",size=0.02) +
    theme_void() +
    scale_fill_gradient(labels = scales::comma,low = map_colours[1],high = map_colours[2]) +
    theme(
      plot.title=element_blank(),
      legend.position = c(codes_match$legendx[which(codes_match$country==countries)],
                          codes_match$legendy[which(codes_match$country==countries)]),
      legend.title = element_text(color = "black", face = "bold", vjust = 0.8),
      plot.margin = margin(10, 0, 10, 0)) #add space around the plot


  map_London <- .data %>%
    mutate(Code_temp = substr(LA_Code, 1,3))%>%
    filter(Code_temp == "E09")

  limits = sf::st_bbox(map_London)
  limits_adj <- limits

  limits_adj[1] <- limits[1] - abs(0.03*(limits[3]-limits[1]))
  limits_adj[2] <- limits[2] - abs(0.03*(limits[4]-limits[2]))
  limits_adj[3] <- limits[3] + abs(0.03*(limits[3]-limits[1]))
  limits_adj[4] <- limits[4] + abs(0.03*(limits[4]-limits[2]))


  map <- map +

    geom_rect( xmin = limits_adj[[1]],
               ymin = limits_adj[[2]],
               xmax = limits_adj[[3]],
               ymax = limits_adj[[4]],
               fill=NA,
               colour = "black",
               size = 0.8)

  finalmap <- map %>%
    cowplot::ggdraw() +
    draw_plot(
      {
        map +
          coord_sf(   xlim = limits_adj[c(1,3)],
                      ylim = limits_adj[c(2,4)],
                      expand = FALSE) +
          theme(legend.position = "none",
                plot.title = element_blank())
      },
      x=codes_match$x_adjust[which(codes_match$country==countries)],
      y=codes_match$y_adjust[which(codes_match$country==countries)],
      width=codes_match$width[which(codes_match$country==countries)],
      height =codes_match$height[which(codes_match$country==countries)]
    )

  if(save){
    ggsave(finalmap,filename = filepath)
  }else{
      return(finalmap)
    }
}


