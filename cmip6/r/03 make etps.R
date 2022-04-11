

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(geodata, terra, fs, tidyverse, glue, raster, rgdal, rgeos, gtools)

source('./00 biofunctions.R')

# Function ----------------------------------------------------------------
etps <- function(dir){
  
  # Start to read the climate data
  cat(basename(dir), '\n')
  fls <- dir_ls(dir) %>% as.character()
  tmx <- grep('tmax', fls, value = T) %>% raster::stack()
  tmn <- grep('tmin', fls, value = T) %>% raster::stack()
  ppt <- grep('prec', fls, value = T) %>% raster::stack()
  tav <- (tmx + tmn) / 2

  # Calculate ETP
  etp <- 0.0013 * 0.408 * srad * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}')
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  
  # Replace NA and Nans by 0
  for(i in 1:12){
    print(i)
    etp[[i]][which(is.nan(etp[[i]][]))] <- 0
    etp[[i]][which(is.na(etp[[i]][]))] <- 0
  }
  
  # Write the final raster
  etp <- round(etp, digits = 0)
  raster::writeRaster(etp, filename = glue('{dir}/etps.tif'), overwrite = T)
  cat('Done!\n')
  
}

# Load data ---------------------------------------------------------------
limt <- geodata::gadm(country = 'VNM', level = 0, path = '../tmpr')
limt <- as(limt, 'Spatial')

dirs <- dir_ls('../raster/future/370/2021-2040')
mask <- raster('../raster/future/370/2021-2040/ACCESS-ESM1-5/prec.tif')[[1]] * 0 + 1

# Solar radiation ---------------------------------------------------------
srad <- dir_ls(path = 'D:/OneDrive - CGIAR/Data/ET_SolRad') %>% 
  grep('et_solrad', ., value = T) %>% 
  map(., read.rstr, vr = 'solrad_') %>% 
  setNames(glue('srad_{1:12}')) %>% 
  raster::stack() %>% 
  raster::crop(., limt) %>% 
  raster::mask(., limt) %>% 
  raster::resample(., mask, method = 'bilinear')

# Make ETP variables ------------------------------------------------------
purrr::map(dirs, etps)



