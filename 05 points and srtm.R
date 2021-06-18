
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools, RColorBrewer)

g <- gc(reset = T)
rm(list = ls())

# Fucntions to use --------------------------------------------------------
make_map_points <- function(pnt, nme){
  
  # pnt <- pnt_1km
  # nme <- 'Dup 1 km'
  
  cat('Get values SRTM\n')
  pnt$srtm <- raster::extract(srt, pnt[,c('Lon', 'Lat')])

  cat('Intersection\n')
  pnt <- st_as_sf(pnt, coords = c('Lon', 'Lat'), crs = st_crs(4326))
  pnt <- st_intersection(pnt, st_as_sf(vtn))
  pnt <- pnt %>% dplyr::select(srtm, VARNAME_1)
  
  cat('Frequency')
  frq <- pnt %>% as_tibble %>% group_by(VARNAME_1) %>% summarise(count = n()) %>% ungroup() %>% arrange(desc(count)) %>% mutate(VARNAME_1 = factor(VARNAME_1, levels = VARNAME_1))
  
  cat('To make the map')
  gg <- ggplot() + 
    geom_sf(data = pnt, aes(color = srtm), size = 0.5) + 
    geom_sf(data = vtn %>% st_as_sf(), fill = NA, col = 'slategray3') +
    scale_color_gradientn(colors = brewer.pal(n = 8, name = 'BrBG')) +
    scale_x_discrete(breaks = c(103, 105, 107, 109)) +
    theme_bw() + 
    theme(legend.position = 'bottom', 
          legend.key.width = unit(2, 'line'),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 11),
          axis.title.y = element_text(size = 11),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 11)) +
    labs(x = 'Lon', y = 'Lat', color = 'm.a.s.l', caption = paste0('Presences: ', nrow(pnt))) +
    coord_sf(xlim = vtn %>% st_as_sf() %>% filter(VARNAME_1 %in% pull(frq, 1)) %>% extent() %>% as.vector() %>% .[c(1,2)],
             ylim = vtn %>% st_as_sf() %>% filter(VARNAME_1 %in% pull(frq, 1)) %>% extent() %>% as.vector() %>% .[c(3,4)]) 
  
  ggsave(plot = gg, filename = paste0('../png/maps/points_', nme, '.png'), units = 'in', width = 5, height = 8, dpi = 300)
  return(frq)
  
}

# Load data ---------------------------------------------------------------

# Vietnam 
vtn <- shapefile('../data/shp/base/vtm_adm1.shp')
pnt_1km <- read_csv('../data/tbl/pnts/point_all_v1_rmDup_1km.csv')
pnt_5km <- read_csv('../data/tbl/pnts/point_all_v1_rmDup_5km.csv')

# Altitude
srt <- raster::getData(name = 'SRTM', lon = as.numeric(pnt_1km[1,1]), lat = as.numeric(pnt_1km[1,2]))

# Production
prd <- st_read('../data/shp/harvested/hrv_13_18.shp')

# Make map the points -----------------------------------------------------
frq_1km <- make_map_points(pnt = pnt_1km, nme = 'Dup 1 km')
frq_5km <- make_map_points(pnt = pnt_5km, nme = 'Dup 5 km')

g1 <- ggplot(data = frq_1km, aes(x = VARNAME_1, y = count)) + geom_col() + theme_ipsum_es() + labs(x = '', y = 'Freq') + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
g2 <- ggplot(data = frq_5km, aes(x = VARNAME_1, y = count)) + geom_col() + theme_ipsum_es() + labs(x = '', y = 'Freq') + theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
ggsave(plot = g1, filename = '../png/grp/geom_count_n_1km.png', units = 'in', width = 7, height = 4, dpi = 300)
ggsave(plot = g2, filename = '../png/grp/geom_count_n_5km.png', units = 'in', width = 7, height = 4, dpi = 300)

# Mosaic ggplot ------------------------------------------------------------
vtn <- full_join(st_as_sf(vtn), frq_5km, by = 'VARNAME_1') 
prd <- prd %>% dplyr::select(VARNAME_1, hrv, prd)
vtn <- full_join(vtn, as_tibble(prd) %>% dplyr::select(-geometry), by = c('VARNAME_1' = 'VARNAME_1'))
vtn <- vtn %>% mutate(prop = count / sum(hrv, na.rm = TRUE))
vtn <- vtn %>% mutate(prop_nrm = prop / sum(prop, na.rm = TRUE))
st_write(vtn,  '../data/shp/harvested/points_hrv_frq.shp')

g_frq <- ggplot() + 
  geom_sf(data = vtn, fill = NA) +
  geom_sf(data = vtn, aes(fill = count)) +
  scale_fill_gradientn(colors = brewer.pal(n = 7, name = 'YlOrBr'), na.value = 'white') +
  coord_sf() +
  theme_bw() + 
  theme(legend.position = 'top', 
        legend.key.width = unit(1.5, 'line')) +
  labs(x = 'Lon', y = 'Lat', fill = 'Presences\n(n)') +
  scale_x_discrete(breaks = c(103, 105, 107, 109)) 

g_hrv <- ggplot() + 
  geom_sf(data = vtn, fill = NA) +
  geom_sf(data = vtn, aes(fill = hrv)) +
  scale_fill_gradientn(colors = brewer.pal(n = 7, name = 'YlOrBr'), na.value = 'white') +
  coord_sf() +
  theme_bw() + 
  theme(legend.position = 'top', 
        legend.key.width = unit(1.8, 'line')) +
  labs(x = 'Lon', y = 'Lat', fill = 'Harvested area\n(Ha)') +
  scale_x_discrete(breaks = c(103, 105, 107, 109)) 

# Generate pseuabsences ----------------------------------------------------------
mask <- raster('../data/raster/climate/current/wc20/bio_1.tif') * 0 
vtn_shp <- shapefile('../data/shp/harvested/points_hrv_frq.shp')
bias.raster <- rasterize(vtn_shp, mask, field = 'prop_nrm')
speciescell <- raster::extract(bias.raster, pnt_5km[,1:2], cellnumber = T)
bias.raster[speciescell[,1]] <- NA
back <- dismo::randomPoints(mask = bias.raster, n = nrow(pnt_5km), prob = TRUE)
back <- as_tibble(back)
plot(bias.raster)
points(back$x, back$y, pch = 16, col = 'red')

back_sf <- back %>% st_as_sf(., coords = c('x', 'y'), crs = st_crs(4326))
back_sf <- back_sf %>% mutate(pseudoabsences = 0.05)

# Map pseudoabsences ------------------------------------------------------

g_bck <- ggplot() + 
  geom_sf(data = vtn, fill = NA) +
  geom_sf(data = back_sf, size = 0.2, col = 'brown') +
  coord_sf() +
  theme_bw() + 
  theme(legend.position = 'top') +
  labs(x = 'Lon', y = 'Lat', fill = 'Background\n(n)') +
  scale_x_discrete(breaks = c(103, 105, 107, 109)) 

# Join 
g_all <- ggarrange(g_frq, g_hrv, g_bck, ncol = 3, nrow = 1)
ggsave(plot = g_all, filename = '../png/grp/freq_hrv_bck.png', units = 'in', width = 12, height = 9, dpi = 300)

# Presences versus pseudoabsences -----------------------------------------
back_sf <- st_intersection(back_sf, vtn)
back_frq <- back_sf %>% as_tibble() %>% group_by(VARNAME_1) %>% summarise(count_bck = n()) %>% ungroup()
pnts_frq <- pnt_5km %>% st_as_sf(., coords = c('Lon', 'Lat'), crs = st_crs(4326)) %>% st_intersection(., vtn) %>% as_tibble %>% group_by(VARNAME_1) %>% summarise(count_occ = n()) %>% ungroup()

all_frq <- full_join(pnts_frq, back_frq, by = 'VARNAME_1')
all_frq <- vtn %>% as_tibble %>% dplyr::select(VARNAME_1, hrv) %>% inner_join(., all_frq, by = 'VARNAME_1')
sum(all_frq$count_bck, na.rm = T)
sum(all_frq$count_occ, na.rm = T)

write.csv(all_frq, '../data/tbl/pnts/frq_occ_bck_hrv.csv', row.names = F)

