
#' Map Crime
#' 
#' Maps values of a specified indicator across block groups
#' 
#' @encoding UTF-8
#' 
#' @param data Shapefile containing the indicator to be mapped 
#' @param indicator The variable within "data" whose values are to be mapped
#' @param include_underlay Adds a layer of block groups for specified counties.
#' @param discrete Is "indicator" discrete or continuous. Required for correct color palette specification
#' @param roadmap Adds major roads to the map. Input determines for which geographic area roads will be displayed. See osmdata documentation for available options.
#' @param color_palette Adjusts color palette. See scale_color_brewer for some available palettes.
#' @param legend_lab Adjusts legend label. Default is name of the indicator.
#' 
#' @returns A ggplot object
#' 
#' @export
FC_crime_map <- function(data,
                         indicator,
                         underlay_counties=NULL,
                         discrete=FALSE,
                         roadmap=NULL,
                         color_palette = "viridis",
                         legend_lab = NULL) {
  require(tidyverse)
  require(sf)
  require(tigris)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  mapping_data <- st_as_sf(data)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Create base map and add underlay if desired
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(is.null(underlay_counties)){
    bg_map_base <-
      ggplot(data = mapping_data)
  }else if(!is.null(underlay_counties)){
    #Generate shapefile for counties specified as underlay
    county_bgs <- 
      block_groups("Ohio", underlay_counties,year=2023) %>% 
      dplyr::select(c(GEOID,INTPTLAT,INTPTLON,COUNTYFP,TRACTCE,BLKGRPCE)) %>% 
      dplyr::rename(census_code_2020_truncated = "GEOID",
                    latitude_bg = "INTPTLAT",
                    longitude_bg = "INTPTLON",
                    county_fip = "COUNTYFP",
                    tract_fip = "TRACTCE",
                    blockgr_fip = "BLKGRPCE")
    #Add underlay counties to map
    bg_map_base <-
      ggplot(data = mapping_data) +
      geom_sf(data = county_bgs)
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Add indicator as overlay 
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  bg_map_overlay <-
    bg_map_base +
    geom_sf(aes(fill = {{indicator}}),color=NA)
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Specify color palette
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(color_palette=="viridis"){
    bg_map_overlay <-
      bg_map_overlay +
      viridis::scale_fill_viridis(discrete = discrete,direction=-1) 
  }else if(color_palette!="viridis"&discrete==TRUE){
    bg_map_overlay <-
      bg_map_overlay +
      scale_fill_brewer(palette = color_palette) 
  }else if(color_palette!="viridis"&discrete==FALSE){
    bg_map_overlay <-
      bg_map_overlay +
      scale_fill_distiller(palette = color_palette)   
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Add additional theme elements:
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  bg_map_overlay <-
    bg_map_overlay +
    theme_bw() + 
    theme(axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank())
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Add Road map
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!is.null(roadmap)){
    require(osmdata)
    
    big_streets <- getbb(roadmap) %>%
      opq() %>%
      add_osm_feature(key = "highway", 
                      value = c("motorway", "primary", "motorway_link", "primary_link")) %>%
      osmdata_sf()
    
    bg_map_overlay <-
      bg_map_overlay + 
      geom_sf(data = big_streets$osm_lines,
              inherit.aes = FALSE,
              color = "black",
              size=2)
    
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Add Legend label
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(!is.null(legend_lab)){
    bg_map_overlay <-
      bg_map_overlay +
      labs(fill = legend_lab)
  }
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #Function output:
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  return(bg_map_overlay)
}


