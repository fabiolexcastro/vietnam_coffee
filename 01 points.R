

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
pnts <- st_read('../data/tbl/coffee_points_19-04-2021/coffee_points_19-04-2021.shp')

# Project -----------------------------------------------------------------
pnts_prj <- st_transform(x = pnts, crs = st_crs(4326))
pnts_geo <- st_coordinates(pnts_prj)
pnts_geo <- as.data.frame(pnts_geo)
pnts_geo <- as_tibble(pnts_geo)
coordinates(pnts_geo) <- ~ X + Y

# Write -------------------------------------------------------------------
st_write(pnts_prj, '../data/shp/points/points_geo.shp')
st_write(pnts, '../data/shp/points/points_prj.shp')

# ADM 1 -------------------------------------------------------------------
adm <- raster::getData(name = 'GADM', country = 'VNM', level = 1)
shapefile(adm, '../data/shp/base/vtm_adm1.shp')
pnts_adm <- raster::intersect(pnts_geo, adm)

# Points 1 
pnts_adm

# Land use ----------------------------------------------------------------
polg_znes <- st_read('../data/shp/points/ToShare/LamDong_LU_2015_reprojected/lu_2015_reprojected/LU15_comp_f2.shp')
nrow(polg_znes)
types <- unique(polg_znes$NAME)
cffe_znes <- polg_znes %>% filter(NAME == 'Coffee')
plot(st_geometry(cffe_znes))

cffe_znes
st_write(cffe_znes, '../data/shp/points/ToShare/LamDong_LU_2015_reprojected/lu_2015_reprojected/coffee.shp')

# Files -------------------------------------------------------------------
fls <- list.files('../data/shp/points/ToShare/landuse', full.names = T, pattern = '.shp$')
shp <- map(.x = fls, .f = st_read)
lapply(shp, names)
dlc <- shp[[1]] %>% filter(HT_TEN == 'Ca phe') %>% st_transform(., crs = st_crs(4326))
dnn <- shp[[2]] %>% filter(HT_ten == 'Ca phe') %>% st_transform(., crs = st_crs(4326))
gla <- shp[[3]] %>% filter(HT_ten == 'Ca phe') %>% st_transform(., crs = st_crs(4326))

st_write(dlc, '../data/shp/points/ToShare/coffee/DLac_LU_utm.shp')
st_write(dnn, '../data/shp/points/ToShare/coffee/DNong_LUutm.shp')
st_write(gla, '../data/shp/points/ToShare/coffee/GLai_LUutm.shp')
