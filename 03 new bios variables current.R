

# Generate new variables ETP and others
# Author: Fabio Castro-Llanos 
# June 07 2021

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, terra, future, furrr)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions  --------------------------------------------------------------
source('./02 functions.R')

# Load data ---------------------------------------------------------------
fles <- list.files('../data/raster/climate/current/wc20', pattern = '.tif$', full.names = T)
fles <- mixedsort(fles)

# Srad conversion ---------------------------------------------------------
srad <- grep('srad', fles, value = TRUE)
srad <- mixedsort(srad)
srad <- stack(srad)
names(srad) <- paste('srad_', 01:12, sep='') 
srad <- srad * 0.408/1000
dout <- '../data/raster/climate/current/wc20/'
Map('writeRaster', x = unstack(srad), filename = paste0(dout, 'srad_', 1:12, '_ok.tif'), overwrite = T)

srad <- grep('srad', fles, value = TRUE) %>% grep('_ok.tif', ., value = TRUE) %>% mixedsort()

# Read temperature and precipitation --------------------------------------
tavg <- read_raster(x = 'tmean')
tmin <- read_raster(x = 'tmin')
tmax <- read_raster(x = 'tmax')
prec <- read_raster(x = 'prec')
zero <- tmin[[1]] * 0 + 1 

# Create bioclimatic 20 ---------------------------------------------------
precbin <- reclassify(prec, c(-Inf, 40, 1, 40, Inf, NA))
prectwo <- addLayer(precbin, precbin)
allp <- stack()
for(i in 1:12){
  oney <- prectwo[[i:(i + 11)]]
  drym <- cumsum(oney)
  maxn <- max(drym, na.rm = TRUE)
  allp <- addLayer(allp, maxn)
}

bio_20 <- max(allp, na.rm = T)
writeRaster(bio_20, paste( '../data/raster/climate/current/wc20/', 'bio_20.asc'))
rm(precbin, prectwo, allp)

# Create ETP Variables ----------------------------------------------------
etps <- 0.0023 * srad * sqrt(reclassify(tmax - tmin, c(-Inf, 0, 0))) * (tavg + 17.8)
names(etps) <- str_replace(names(etps), 'layer.' , 'pet_')
etps <- etps * c(31,29,31,30,31,30,31,31,30,31,30,31)
etps <- round(etps, 0)
Map('writeRaster', x = unstack(etps), filename = paste0(dout, 'etp_', 1:12, '.tif'), overwrite = TRUE)

# Other variables ---------------------------------------------------------
dfct <- prec - etps
dftm <- cbind(as.matrix(dfct), as.matrix(tmin), as.matrix(tmax))
bios <- t(apply(dftm, 1, cumTemp))
nmes <- paste0('bio_', 30:33)

# Write biovariables 30 to 33 ---------------------------------------------
map(.x = 1:ncol(bios), .f = function(k){
  print(k)
  lyer <- zero
  values(lyer) <- bios[,k]
  writeRaster(lyer, filename = paste0(dout, nmes[k], '.tif'), overwrite = FALSE)
})

# ETP bioclimatics --------------------------------------------------------
etpr <- cbind(as.matrix(etps),as.matrix(prec),as.matrix(tavg))
etbi <- t(apply(etpr, 1, etpvars))
nmes <- paste0('bio_', 21:29)

# Write biovariables 21 to 29 ---------------------------------------------
map(.x = 1:ncol(etbi), .f = function(k){
  print(k)
  lyer <- zero
  values(lyer) <- etbi[,k]
  writeRaster(lyer, filename = paste0(dout, nmes[k], '.tif'), overwrite = FALSE)
})


# Bioclimatic 34 ----------------------------------------------------------

# 6 meses consecutivos más húmedos
ssn <- list(1:6, 2:7, 3:8, 4:9, 5:10, 6:11, 7:12, 8:13, 9:14, 10:15, 11:16, 12:17)
prectwo <- addLayer(prec, prec)
prectwo_tb <- rasterToPoints(prectwo) %>% as_tibble()
prectwo_tb <- prectwo_tb %>% mutate(gid = 1:nrow(.)) %>% dplyr::select(gid, x, y, everything())
coords <- prectwo_tb %>% dplyr::select(1:3)
prectwo_tb <- prectwo_tb  %>% dplyr::select(gid:prec_5.2)

get_max <- function(pix){
  
  vls <- prectwo_tb %>% slice(pix) %>% .[4:ncol(.)] %>% as.numeric()
  max <- map(.x = 1:length(ssn), .f = function(k){sum(vls[ssn[[k]]])}) %>% unlist() %>% .[which.max(.)]
  return(max)
  
}

plan(cluster, workers = 3, gc = TRUE)
rslt <- coords %>% mutate(max_prec = furrr::future_map(.x = gid, .f = get_max))
future:::ClusterRegistry("stop"); gc(reset = T)
rsl3 <- rslt %>% unnest(max_prec) 
rsl4 <- rsl3 %>% dplyr::select(2:4) %>% rasterFromXYZ()
writeRaster(rsl4, paste0('../data/raster/climate/worldclim/crnt/bio_35.csv'))

