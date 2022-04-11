
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(geodata, terra, fs, tidyverse, glue, dismo, raster)

# Function ----------------------------------------------------------------
biov <- function(dir){
  
  # dir <- dirs[1]
  
  cat(basename(dir), '\t')
  fls <- dir_ls(dir)  
  ppt <- grep('prec', fls, value = T) %>% raster::stack()
  tmx <- grep('tmax', fls, value = T) %>% raster::stack()
  tmn <- grep('tmin', fls, value = T) %>% raster::stack()
  
  bio <- dismo::biovars(prec = ppt, tmax = tmx, tmin = tmn)
  raster::writeRaster(x = bio, filename = glue('{dir}/bioc.tif'), overwrite = T)
  cat('Done\n')
  
}

# Load data ---------------------------------------------------------------
dirs <- dir_ls('../raster/future/370/2021-2040')

# Make biovars ------------------------------------------------------------
map(dirs, biov)
biov(dir = dirs[2])
biov(dir = dirs[3])