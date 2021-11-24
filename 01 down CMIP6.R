
# -------------------------------------------------------------------------
# Source: https://gist.github.com/ani-ghosh/4504bf639628247395cafd554eacf407
# -------------------------------------------------------------------------

# Functions ---------------------------------------------------------------
cmip6_world <- function(model, ssp, time, var, res, path, v) {
  
  cat(model, '\n')
  
  res <- as.character(res)
  stopifnot(res %in% c("2.5", "5", "10", "0.5"))
  stopifnot(var %in% c("tmin", "tmax", "prec", "bio", "bioc"))
  ssp <- as.character(ssp)
  stopifnot(ssp %in% c("126", "245", "370", "585"))
  stopifnot(model %in% c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "GFDL-ESM4", "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0"))
  # stopifnot(model %in% c("ACCESS-CM2","BCC-CSM2-MR","CIESM","CNRM-CM6-1","CNRM-ESM2-1","FIO-ESM-2-0","HadGEM3-GC31-LL","INM-CM5-0",
  #                        "MIROC-ES2L","MPI-ESM1-2-LR", "ACCESS-ESM1-5","CanESM5","CMCC-ESM2","CNRM-CM6-1-HR","FGOALS-g3",
  #                        "GISS-E2-1-G","INM-CM4-8", "IPSL-CM6A-LR","MPI-ESM-1-2-HAM","NESM3"))
  stopifnot(time %in% c("2021-2040", "2041-2060", "2061-2080"))
  
  # Some combinations do not exist. Catch these here.
  
  if (var == "bio") var <- "bioc"
  stopifnot(dir.exists(path))
  
  fres <- ifelse(res==0.5, "30s", paste0(res, "m"))
  path <- file.path(path, paste0("wc2.1_", fres, "/"))
  dir.create(path, showWarnings = FALSE)
  tif <- paste0("wc2.1_", fres, "_", var, "_", model, "_ssp", ssp, "_", time, ".tif")
  ptif <- file.path(path, tif)
  
  if(!missing(v)){
    ptif <- file.path(path, paste0("subset_", tif))
  }
  
  if (!file.exists(ptif)) {
    url <- file.path(.wcurl, fres, model, paste0("ssp", ssp), tif)
    if(!missing(v)){
      u1 <- file.path("/vsicurl", url)
      r <- terra::rast(u1)
      r <- crop(r, v, snap = "out", filename = ptif)
    } else {
      ok <- try(utils::download.file(url, ptif, mode="wb"), silent=TRUE)
      if (class(ok) == "try-error") {stop("download failed")}
      if (!file.exists(ptif)) {stop("download failed")}
    }
  }
  rast(ptif)
}

# URL Main
.wcurl <- "https://biogeo.ucdavis.edu/cmip6/"

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, terra)

# Models
mdls <- c("ACCESS-CM2","BCC-CSM2-MR","CIESM","CNRM-CM6-1","CNRM-ESM2-1","FIO-ESM-2-0",
          "HadGEM3-GC31-LL","INM-CM5-0","MIROC-ES2L","MPI-ESM1-2-LR", 
          "CanESM5","CMCC-ESM2","CNRM-CM6-1-HR","FGOALS-g3", "ACCESS-ESM1-5",
          "GISS-E2-1-G","INM-CM4-8", "IPSL-CM6A-LR","MPI-ESM-1-2-HAM","NESM3")

mdls <- c("BCC-CSM2-MR", "CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1", "GFDL-ESM4", 
          "IPSL-CM6A-LR", "MIROC-ES2L", "MIROC6", "MRI-ESM2-0")

# Download administrative limit -------------------------------------------

# Vietnam
vnm <- raster::getData(name = 'GADM', country = 'VNM', level = 0)

# Parameters control ------------------------------------------------------
sp <- 370
pr <- '2021-2040'
rs <- 0.5
pt <- '../data/raster/climate/future/cmip6/ssp370/2021_2040'
lm <- vnm

# For ---------------------------------------------------------------------
map(.x = 1:length(mdls), .f = function(i){
  cat(mdls[i], '\n')
  rsl <- tryCatch(expr = {
    tmx <- cmip6_world(model = mdls[i], ssp = sp, time = pr, 
                       var = "tmax", res = rs, path = pt, v = lm)
    tmn <- cmip6_world(model = mdls[i], ssp = sp, time = pr, 
                       var = "tmin", res = rs, path = pt, v = lm)
    prc <- cmip6_world(model = mdls[i], ssp = ssp, time = prd, 
                       var = "prec", res = rsl, path = pth, v = lim)
  }, error = function(e){
    return(print('Error problem'))
  })
  cat("Finish..., Everything is Ok\n")
})


# Copy this scritp to the Github 
dinp <- '01 down CMIP6.R'
dout <- 'D:/GITHUB/vietnam/01 down CMIP6.R'
file.copy(from = dinp, to = dout)
