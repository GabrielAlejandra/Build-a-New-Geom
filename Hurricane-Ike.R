install.packages("tidyverse")
install.packages("geosphere")
install.packages("ggmap")
install.packages("ggplot2")
library("readr")
#library("tidyverse")
#library("geosphere")
library("ggplot2")
library("ggmap")
library("magrittr")


#' Read the data base

ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                         "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf("ebtrk_atlc_1988_2015.txt", 
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

ext_tracks

#' Data cleaning: Combine storm and year into a varibale storm_id, changing longitude to be 0 to -180 W hemisphere, 0-180 in E hemisphere
#' combine columns describing the date and time to create a single variable with the date-time of each observation.

ext_tracks <- ext_tracks %>%
  dplyr::mutate(storm_id=paste(storm_name,"-",year,sep=""),
                longitude=ifelse(longitude>180,360-longitude,-longitude),
                date = as.POSIXct(paste(year,month,day,hour),format="%Y %m %d %H")
  )


ext_tracks

#' Convert the data to a “long” format, with separate rows for each of the three wind speeds for wind radii.

ext_tracks_34 <- ext_tracks %>% 
  dplyr::mutate(wind_speed=34) %>%  
  dplyr::rename(ne=radius_34_ne,nw=radius_34_nw,se=radius_34_se,sw=radius_34_sw) %>% 
  dplyr::select(storm_id,date,latitude,longitude,wind_speed,ne,nw,se,sw)

ext_tracks_50 <- ext_tracks %>% 
  dplyr::mutate(wind_speed=50) %>%  
  dplyr::rename(ne=radius_50_ne,nw=radius_50_nw,se=radius_50_se,sw=radius_50_sw) %>% 
  dplyr::select(storm_id,date,latitude,longitude,wind_speed,ne,nw,se,sw)

ext_tracks_64 <- ext_tracks %>% 
  dplyr::mutate(wind_speed=64) %>%  
  dplyr::rename(ne=radius_64_ne,nw=radius_64_nw,se=radius_64_se,sw=radius_64_sw) %>% 
  dplyr::select(storm_id,date,latitude,longitude,wind_speed,ne,nw,se,sw)

ext_tracks<-rbind(ext_tracks_34,ext_tracks_50,ext_tracks_64)

ext_tracks


#' Filter out hurricane observation tables for  Ike.

Ike_obs<-dplyr::filter(ext_tracks,storm_id=="IKE-2008",date=="2008-09-13 12:00:00")

#' Stat for creating wind radii chart data
#' 
#' This stat takes the hurricane observation file containing the latitude, longitude, ne, nw, se and sw
#' distances and via the function compute_group creates the x and y data for a wind radius chart.  

#' @param data A data frame containing the required aesthetics passed from the geom associated with the stat
#'
#' @param scale A list with the range for the x and y axes
#' 
#' @return compute_group returns a data frame with the wind radii map x and y coordinates by group for each wind speed group

StatHurr <- ggproto("StatHurr", Stat,
                    compute_group = function(data, scales) {
                      
                      # naming variables for the center x,y coordinates
                      xob<-data$x[1]
                      yob<-data$y[1]
                      
                      # creating 34 knot outer wind chart coordinates via geosphere funcgtion destPoint, including other required columns.  
                      ne<-geosphere::destPoint(c(xob,yob),b=0:90,d=dplyr::filter(data,fill==34)$ne*1852)
                      se<-geosphere::destPoint(c(xob,yob),b=90:180,d=dplyr::filter(data,fill==34)$se*1852)
                      sw<-geosphere::destPoint(c(xob,yob),b=180:270,d=dplyr::filter(data,fill==34)$sw*1852)
                      nw<-geosphere::destPoint(c(xob,yob),b=270:360,d=dplyr::filter(data,fill==34)$nw*1852)
                      poly_34<-cbind(group=1L,colour="34",fill="34",as.data.frame(rbind(c(xob,yob),ne,se,sw,nw))) #Include center as first row so can refer to it in geom for rscale
                      
                      # same for 50 know middle wind chart.  
                      ne<-geosphere::destPoint(c(xob,yob),b=0:90,d=dplyr::filter(data,fill==50)$ne*1852)
                      se<-geosphere::destPoint(c(xob,yob),b=90:180,d=dplyr::filter(data,fill==50)$se*1852)
                      sw<-geosphere::destPoint(c(xob,yob),b=180:270,d=dplyr::filter(data,fill==50)$sw*1852)
                      nw<-geosphere::destPoint(c(xob,yob),b=270:360,d=dplyr::filter(data,fill==50)$nw*1852)
                      poly_50<-cbind(group=2L,colour="50",fill="50",as.data.frame(rbind(c(xob,yob),ne,se,sw,nw)))
                      
                      # same for 64 know inner wind chart.  
                      ne<-geosphere::destPoint(c(xob,yob),b=0:90,d=dplyr::filter(data,fill==64)$ne*1852)
                      se<-geosphere::destPoint(c(xob,yob),b=90:180,d=dplyr::filter(data,fill==64)$se*1852)
                      sw<-geosphere::destPoint(c(xob,yob),b=180:270,d=dplyr::filter(data,fill==64)$sw*1852)
                      nw<-geosphere::destPoint(c(xob,yob),b=270:360,d=dplyr::filter(data,fill==64)$nw*1852)
                      poly_64<-cbind(group=3L,colour="64",fill="64",as.data.frame(rbind(c(xob,yob),ne,se,sw,nw)))
                      
                      # combine the data for all the wind charts, rename the lon lat created by destPoint to x, y
                      wind_cht<-data.frame(rbind(poly_34,poly_50,poly_64))
                      colnames(wind_cht)[4]<-"x"
                      colnames(wind_cht)[5]<-"y"
                      wind_cht
                    },
                    
                    required_aes = c("x", "y","fill","ne","nw","se","sw")
)

#' Stat_* function that builds the layer for ggplot functions (parameter descriptions taken from ggplot documentation)
#' 
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' 
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot. A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created.  A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data.
#' 
#' @param geom The geometric object to use display the data
#' 
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function
#' 
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' 
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' 
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' 
#' @return 

stat_hurr <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", na.rm = FALSE, show.legend = NA, 
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatHurr, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Geom for creating polygon group with wind radii charts
#' 
#' This Geom is based on polygon geom and takes the data from StatHurr, scales it, and creates the polygonGrob via the draw_group
#' function.  The optional aesthetic rscale is introduced that scales the wind radii back to a certain percent (rscale) of the
#' maximum radii.  (parameter descriptions taken from ggplot documentation)
#' 
#' @data a data frame passed by StatHurr after converting to wind radii charts
#'
#' @panel_scales a list containing information about the scales in the current panel. 
#' 
#' @coord a coordinate specification
#' 
#' @return 

GeomHurricane <- ggproto("GeomPolygon", Geom,
                         required_aes = c("x", "y"),
                         
                         default_aes = aes(
                           colour = "black", fill = "grey20", size = 0.5,
                           linetype = 1, alpha = .6, rscale = 1.0 
                         ),
                         
                         draw_key = draw_key_polygon,
                         
                         draw_group = function(data, panel_scales, coord) {
                           
                           # Scale the maximum wind chart by rscale factor 
                           data$x <- data$x[1] * (1 - data$rscale) + data$x * data$rscale
                           data$y <- data$y[1] * (1 - data$rscale) + data$y * data$rscale
                           
                           coords <- coord$transform(data, panel_scales)
                           
                           grid::polygonGrob(
                             coords$x, coords$y, 
                             default.units = "native",
                             gp = grid::gpar(
                               col = coords$colour,
                               lwd = coords$size * .pt,
                               fill = scales::alpha(coords$fill, coords$alpha),
                               lty = coords$linetype
                             )
                           )
                         }
)

#' Geom_* function that builds the layer based on the geom specifications (parameter descriptions taken from ggplot documentation)
#' 
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' 
#' @param data The data to be displayed in this layer. There are three options: If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot. A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify for which variables will be created. A function will be called with a single argument, the plot data. The return value must be a data.frame., and will be used as the layer data. 
#'
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' 
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' 
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' 
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes.
#' 
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders.
#' 
#' @return

geom_hurricane <- function(mapping = NULL, data = NULL, stat = "hurr",
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    geom = GeomHurricane, data = data, mapping = mapping, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#
map_data <- get_map("Louisiana", zoom = 6, maptype = "toner-background")
base_map <- ggmap(map_data, extent = "device")

Ike_Map <- (base_map +
                geom_hurricane(data = Ike_obs, aes(x = longitude, y = latitude,
                                                   ne = ne, se = se,
                                                   nw = nw, sw = sw,
                                                   fill = wind_speed,
                                                   color = wind_speed)) +
                ggtitle("Ike windmap for 2008-09-13 12:00:00") +
                theme(plot.margin=grid::unit(c(0.5,0.5,0.5,0.5), "in")) +
                scale_color_manual(name = "Wind speed (kts)",
                                   values = c("red", "orange", "yellow")) +
                scale_fill_manual(name = "Wind speed (kts)",
                                  values = c("red", "orange", "yellow")))

Ike_Map

ggsave("Hurricane Ike_Map.pdf")
