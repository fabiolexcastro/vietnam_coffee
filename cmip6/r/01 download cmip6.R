


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(geodata, terra, fs, tidyverse, glue)

# Function ----------------------------------------------------------------
down <- function(ssp, prd, mdl){
  
  # ssp <- ssps[3]
  # prd <- prds[1]
  # mdl <- 'CanESM5'
  
  cat(ssp, prd, mdl, '\t', sep = ' ')
  tmx <- geodata::cmip6_tile(lon = 106, lat = 16.6, model = mdl, ssp = ssp, time = prd, var = 'tmax', path = '../tmpr')
  tmx <- terra::crop(tmx, limt)
  tmx <- terra::mask(tmx, limt)
  
  tmn <- geodata::cmip6_tile(lon = 106, lat = 16.6, model = mdl, ssp = ssp, time = prd, var = 'tmin', path = '../tmpr')
  tmn <- terra::crop(tmn, limt)
  tmn <- terra::mask(tmn, limt)
  
  ppt <- geodata::cmip6_tile(lon = 106, lat = 16.6, model = mdl, ssp = ssp, time = prd, var = 'prec', path = '../tmpr')
  ppt <- terra::crop(ppt, limt)
  ppt <- terra::mask(ppt, limt)
  
  out <- glue('../raster/future/{ssp}/{prd}/{mdl}')
  ifelse(!file.exists(out), dir_create(out), print('Already exists'))
  
  cat('To write the raster', '\n')
  raster::writeRaster(x = tmx, filename = glue('{out}/tmax.tif'))
  raster::writeRaster(x = tmn, filename = glue('{out}/tmin.tif'))
  raster::writeRaster(x = ppt, filename = glue('{out}/prec.tif'))
  
}

# Download data -----------------------------------------------------------
dir_create('../tmpr')
limt <- geodata::gadm(country = 'VNM', level = 0, path = '../tmpr')
cntr <- terra::centroids(limt)

# Climate parameters
ssps <- c(126, 245, 370, 585)
prds <- c('2021-2040', '2041-2060', '2061-2080')
mdls <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR", "BCC-CSM2-MR", "CanESM5", "CanESM5-CanOE", "CMCC-ESM2", "CNRM-CM6-1", "CNRM-CM6-1-HR", "CNRM-ESM2-1", "EC-Earth3-Veg", "EC-Earth3-Veg-LR", "FIO-ESM-2-0", "GFDL-ESM4", "GISS-E2-1-G", "GISS-E2-1-H", "HadGEM3-GC31-LL", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "UKESM1-0-LL")

# Interest models 
trgt <- c('MIROC6', 'CanESM5', 'EC-Earth3-Veg', 'ACCESS-ESM1-5')
trgt <- c('MIROC6', 'EC-Earth3-Veg', 'ACCESS-ESM1-5')

# Apply the function ------------------------------------------------------
for(i in 1:length(trgt)){
  down(ssp = ssps[3], prd = prds[1], mdl = trgt[i])
}

