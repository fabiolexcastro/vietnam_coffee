
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse,
               glue, gtools, fs, RCurl)

g <- gc(reset = TRUE)
rm(list = ls())

# Function ----------------------------------------------------------------
read.rstr <- function(fl, vr){
  cat(vr, '\n')
  rs <- grep(vr, fl, value = TRUE)
  rs <- mixedsort(rs)
  rs <- as.character(rs)
  rs <- raster::stack(rs)
  return(rs)
}

# Load data ---------------------------------------------------------------
fles <- dir_ls(path = '../data/raster/climate/current/wc20', regexp = '.tif')
tmax <- read.rstr(fl = fles, vr = 'tmax')
tmin <- read.rstr(fl = fles, vr = 'tmin')
prec <- read.rstr(fl = fles, vr = 'prec')
tavg <- tmax + tmin / 2

limt <- getData(name = 'GADM', country = 'VNM', level = 0)
dout <- '../data/raster/climate/current/wc20'

# Extratterestre solar radiation ------------------------------------------
srad <- dir_ls(path = 'D:/OneDrive - CGIAR/Data/ET_SolRad')
srad <- read.rstr(fl = srad, vr = 'solrad_')
names(srad) <- glue('srad_{1:12}')

srad <- raster::crop(srad, limt)
srad <- raster::mask(srad, limt)

# Resampling srad 
srad <- raster::resample(x = srad, y = tmax, method = 'bilinear')

# Bioclimatic 20 ----------------------------------------------------------
precbin <- reclassify(prec, c(-Inf, 50, 1, 50, Inf, NA))
prectwo <- addLayer(precbin, precbin)
allp <- stack()
for(i in 1:12){
  oney <- prectwo[[i:(i + 11)]]
  drym <- cumsum(oney)
  maxn <- max(drym, na.rm = TRUE)
  allp <- addLayer(allp, maxn)
}

bio_20 <- max(allp, na.rm = TRUE)
writeRaster(bio_20, glue('{dout}/bio_20.tif'))

# Make ETP variables ------------------------------------------------------
etps <- 0.0013 * 0.408 * srad * (tavg + 17) * (tmax - tmin - 0.0123 * prec) ^ 0.76
names(etps) <- glue('etp_{1:12}')
etps <- round(etps, 0)
plot(etps)

for(i in 1:12){
  print(i)
  etps[[i]][which(is.nan(etps[[i]][]))] <- 0
  etps[[i]][which(is.na(etps[[i]][]))] <- 0

}

etps <- etps * c(31,29,31,30,31,30,31,31,30,31,30,31)
etps <- raster::crop(etps, limt)
etps <- raster::mask(etps, limt)

plot(prec[[1]] * 0)
plot(etps[[9]] * 10, add = TRUE)
Map('writeRaster', x = unstack(etps), filename = glue('{dout}/etp_{1:12}.tif')) # , overwrite = TRUE

etps <- dir_ls(glue('{dout}'), regexp = '.tif$')
etps <- grep('etp', etps, value = TRUE)
etps <- as.character(etps)
etps <- mixedsort(etps)
etps <- raster::stack(etps)

# ETP bioclimatics --------------------------------------------------------
source('00 bioclimatic fucntions.R')
etpr <- cbind(as.matrix(etps),as.matrix(prec),as.matrix(tavg))
etbi <- t(apply(etpr, 1, etpvars))

nmes <- paste0('bio_', 21:29)
zero <- prec[[1]]
zero <- zero * 0 + 1
names(zero) <- 'zero'

map(.x = 1:ncol(etbi), .f = function(k){
  print(k)
  lyer <- etps[[1]]
  values(lyer) <- etbi[,k]
  writeRaster(lyer, filename = glue('{dout}/{nmes[k]}.tif'), overwrite = TRUE)
})

# Biovariables 30 to 33 ---------------------------------------------------
dfct <- prec - etps
dftm <- cbind(as.matrix(dfct), as.matrix(tmin), as.matrix(tmax))
bios <- t(apply(dftm, 1, cumTemp))
nmes <- paste0('bio_', 30:33)

map(.x = 1:ncol(bios), .f = function(k){
  print(k)
  lyer <- etps[[1]]
  values(lyer) <- bios[,k]
  writeRaster(lyer, filename = glue('{dout}/{nmes[k]}.tif'), overwrite = TRUE)
})


