

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, ggrepel, rgeos, stringr, sf, tidyverse, gtools, 
               ggspatial, terra, ggspatial, gridExtra, ggpubr, ggrepel, ggthemes, hrbrthemes,
               RColorBrewer, Boruta, mlbench, randomForest, caret, fasterize, fs, gridExtra, 
               outliers, Hmisc, cclust,
               colorspace, glue, dismo, usdm, classInt, ENMeval, ggsn, ggdendro, dendextend, pvclust, ape, 
               factoextra)

g <- gc(reset = TRUE)
rm(list = ls())

# Function ----------------------------------------------------------------
make_map <- function(var, tpe, nme){
  
  # var <- stck[[1]]
  # tpe <- 'tmp'
  # nme <- 'bio_1'
  
  cat('Start', '\n')
  # Read as a raster
  r20 <- var
  
  # Title
  ttl <- nme
  
  # Condition for the color of the legend / units
  cat('Conditional 2\n')
  if (tpe == 'tmp') {
    clr <- 'Heat'
    unt <- 'Temperature (°C)'
  } else if(tpe == 'ppt'){
    clr <- 'Emrld'
    unt <- 'Precipitation (mm)'
  } else {
    clr <- 'TealGrn'
    unt <- 'ETP (mm)'
  }
  
  # Stack and conversion to table
  tbl <- rasterToPoints(r20, spatial = FALSE)
  tbl <- as_tibble(tbl)
  names(tbl) <- c('lon', 'lat', 'Worldclim v2.0')
  tbl <- gather(tbl, source, value, -lon, -lat)
  
  # To make the map
  gmp <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = value)) + 
    scale_fill_continuous_sequential(palette = clr, rev = TRUE) +
    geom_sf(data = lim, fill = NA) + 
    coord_sf(xlim = extent(lim)[1:2], ylim = extent(lim)[3:4]) +
    ggtitle(label = ttl) +
    facet_wrap(.~source) + 
    theme_ipsum_es() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.8, 'line'), 
          plot.title = element_text(size = 12, face = 'bold', hjust = 0.5), 
          axis.text.x = element_text(size = 6), 
          axis.text.y = element_text(size = 6), 
          axis.title.x = element_text(size = 8), 
          axis.title.y = element_text(size = 8), 
          legend.title = element_text(size = 8, face = 'bold')) + 
    labs(x = 'Longitude', y = 'Latitude') + 
    north(data = lim, symbol = 12) + 
    labs(fill = unt)
  
  out <- glue('../png/maps/climate/current/{ttl}.jpg')
  ggsave(plot = gmp, filename = out, units = 'in', width = 4, height = 8, dpi = 300)
  cat('Done\n')
  
}

# Load data ---------------------------------------------------------------
fles <- dir_ls('../data/raster/climate/current/wc20', regexp = 'tif$')
fles <- grep('bio', fles, value = TRUE)
fles <- as.character(fles)
fles <- mixedsort(fles)
fles <- fles[1:33]
stck <- raster::stack(fles)
lim  <- st_as_sf(raster::getData(name = 'GADM', country = 'VNM', level = 1))

# Make map ----------------------------------------------------------------

map(.x = 1:11, .f = function(k){make_map(var = stck[[k]], tpe = 'tmp', nme = glue('bio_{k}'))})
map(.x = 12:20, .f = function(k){make_map(var = stck[[k]], tpe = 'ppt', nme = glue('bio_{k}'))})
map(.x = 21:33, .f = function(k){make_map(var = stck[[k]], tpe = 'etp', nme = glue('bio_{k}'))})


