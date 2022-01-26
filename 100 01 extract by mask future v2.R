

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, terra, fs, 
               glue, gtools)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)
source('00 bioclimatic fucntions.R')

# Functios ----------------------------------------------------------------
read_rst <- function(x) grep(x, fle, value = TRUE) %>% raster::stack() %>% raster::crop(., vnm) %>% raster::mask(., vnm)

# Load data ---------------------------------------------------------------
prd <- '2041_2060'
fls <- dir_ls(glue('../1.Data/climate/future/cmip6/ssp370/{prd}/wc2.1_30s'), regexp = '.tif$')

grep('bio_19_', fls, value = TRUE) %>% 
  as.character()

# Get the name of each model
mdl <- basename(fls) %>% 
  str_split(string = ., pattern = '_') %>% 
  sapply(., function(k) k[5]) %>% 
  unique()
mdl <- as.character(na.omit(mdl))

# Administrative limit
vnm <- raster::getData(name = 'GADM', country = 'VNM', level = 1)

# Srad general ------------------------------------------------------------
srad <- dir_ls('D:/CIAT/DATA/SRAD/ET_SolRad') %>% 
  grep('et_solrad', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  raster::stack() %>% 
  raster::crop(., vnm) %>% 
  raster::mask(., vnm)

dir.create('../data/raster/srad_extr')
Map('writeRaster', x = unstack(srad), filename = glue('../data/raster/srad_extr/srad_{1:12}.tif'))

# Create normal bioclimatic -----------------------------------------------
map(.x = 14:length(mdl), .f = function(k){
  
  cat(mdl[k], '\n')
  fle <- grep(paste0(mdl[k], '_ssp370'), fls, value = TRUE) %>% 
    grep(paste0(c('prec', 'tmax', 'tmin'), collapse = '|'), ., value = TRUE) %>% 
    as.character()
  
  cat('Read main variables\n')
  ppt <- grep('prec', fle, value = TRUE) %>% as.character() %>% raster::stack() %>% raster::crop(., vnm) %>% raster::mask(., vnm)
  tmx <- grep('tmax', fle, value = TRUE) %>% as.character() %>% raster::stack() %>% raster::crop(., vnm) %>% raster::mask(., vnm)
  tmn <- grep('tmin', fle, value = TRUE) %>% as.character() %>% raster::stack() %>% raster::crop(., vnm) %>% raster::mask(., vnm)
  
  cat('To create the bioclimatic variables\n')
  bio <- dismo::biovars(prec = ppt, tmax = tmx, tmin = tmn)
  
  cat('To write the bioclimatic variables\n')
  out <- glue('{unique(dirname(fle))}/bio_{1:19}_{mdl[k]}.tif')
  Map('writeRaster', x = unstack(bio), filename = out, overwrite = TRUE)
  cat('Done\n')
  
})

# Check the results -------------------------------------------------------
dir_ls('../1.Data/climate/future/cmip6/ssp370/2041_2060/wc2.1_30s', regexp = '.tif$') %>% 
  # grep(paste0(bios, collapse = '|'), ., value = TRUE) %>% 
  grep('bio', ., value = TRUE) %>% 
  as.character() %>% 
  mixedsort() %>% 
  basename() %>% 
  data.frame(name = .) %>% 
  mutate(name = gsub('.tif', '', name),
         variable = str_sub(string = name, start = 1, end = 6), 
         variable = gsub('_$', '', variable), 
         model = str_sub(string = name, start = 7, end = nchar(name)),
         model = gsub('^_', '', model)) %>% 
  group_by(model) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  mutate(period = '2041-2060') %>% 
  filter(count > 19)


# Create etp variables ---------------------------------------------------
# fls <- dir_ls('../data/raster/climate/future/cmip6/ssp370/2041_2060/wc2.1_30s', regexp = 'tif$')

map(.x = 1:length(mdl), .f = function(k){
  
  cat('Start ', mdl[k], '\n')
  fle <- mdl[k] %>% 
    grep(., fls, value = TRUE) %>% 
    as.character()
  
  cat('To read the raster\n')
  ppt <- read_rst(x = 'prec')
  tmx <- read_rst(x = 'tmax')
  tmn <- read_rst(x = 'tmin')
  tav <- (tmx + tmn) / 2
  srd <- raster::resample(srad, ppt, method = 'bilinear')
  
  cat('To calculate the ETP variables\n')
  etp <- 0.0013 * 0.408 * srd * (tav + 17) * (tmx - tmn - 0.0123 * ppt) ^ 0.76
  names(etp) <- glue('etp_{1:12}') 
  etp <- etp * c(31,29,31,30,31,30,31,31,30,31,30,31)
  etp <- round(etp, 0)
  
  cat('To check if there is NA values')
  for(i in 1:12){
    cat(i, '\n')
    etp[[i]][which(is.nan(etp[[i]][]))] <- 0
  }
  
  cat('To write these rasters\n')
  Map('writeRaster', x = unstack(etp), filename = glue('{unique(dirname(fls))}/etp_{1:12}_{mdl[k]}.tif'), overwrite = TRUE)
  cat('Done!\n')
  
})


# Create bioclimatic variables --------------------------------------------
map(.x = 4:length(mdl), .f = function(k){
  
  cat('Start ', mdl[k], '\n')
  fle <- mdl[k] %>% 
    grep(., fls, value = TRUE) %>% 
    as.character()
  
  cat('To read the raster\n')
  ppt <- read_rst(x = 'prec')
  tmx <- read_rst(x = 'tmax')
  tmn <- read_rst(x = 'tmin')
  etp <- read_rst(x = 'etp')
  
  cat('To calculate other bioclimatic variables\n')
  dfc <- ppt - etp
  dft <- cbind(as.matrix(dfc), as.matrix(tmn), as.matrix(tmx))
  bio <- t(apply(dft, 1, cumTemp))
  dot <- glue('{unique(dirname(fls))}/bio_{30:33}_{mdl[k]}.tif')
  zro <- ppt[[1]]
  
  map(1:ncol(bio), function(i){
    cat(i, '\n')
    lyr <- zro
    values(lyr) <- bio[,i]
    writeRaster(lyr, filename = dot[i], overwrite = TRUE)
  })
  
  cat('To calculate other bioclimatic variables\n') 
  mtx <- cbind(as.matrix(etp), as.matrix(ppt), as.matrix(tav))
  etb <- t(apply(mtx, 1, etpvars))
  dot <- glue('{unique(dirname(fls))}/bio_{21:29}_{mdl[k]}.tif')
  zro <- ppt[[1]]
  
  map(1:ncol(etb), function(i){
    cat(i, '\n')
    lyr <- zro
    values(lyr) <- etb[,i]
    writeRaster(lyr, filename = dot[i], overwrite = TRUE)
  })
  
  cat('Finish!\n')
  
})

