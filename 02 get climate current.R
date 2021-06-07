
# Load libraries ----------------------------------------------------------
require(pacman)
p_load(raster, rgdal, rgeos, tidyverse, gtools)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- '//dapadfs/data_cluster_4/observed/gridded_products/worldclim/Global_30s_v2'
fles <- list.files(root, full.names = T)
vars <- c(paste0('prec_', 1:12), paste0('srad_', 1:12), paste0('tmax_', 1:12), paste0('tmean_', 1:12), paste0('tmin_', 1:12))
vars <- paste0(vars, '.tif$')
vtnm <- shapefile('../data/shp/base/vtm_adm1.shp')

# Grep variables
fles <- grep(paste0(vars, collapse = '|'), fles, value = T)
fles <- mixedsort(fles)

# Read as a stack ---------------------------------------------------------
stck <- raster::stack(fles)
stck <- raster::crop(stck, vtnm)
stck <- raster::mask(stck, vtnm)

# Write files -------------------------------------------------------------
outp <- '../data/raster/climate/current/wc20'
dir.create(outp, recursive = TRUE)
Map('writeRaster', x = unstack(stck), filename = paste0(outp, '/', names(stck), '.tif'), overwrite = TRUE)




