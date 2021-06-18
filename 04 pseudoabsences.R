

# Bias background processing
# Author: Fabio Castro-Llanos 
# June 11 2021

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, gtools,
               RColorBrewer, hrbrthemes, ggspatial, ggthemes, ggpubr, ggrepel)

g <- gc(reset = T)
rm(list = ls())

# Load data ---------------------------------------------------------------
vtnm <- st_read('../data/shp/base/vtm_adm1.shp')
pnt1 <- st_read('../data/shp/points/points_geo.shp')
pnt2 <- st_read('../data/shp/points/ToShare/coffee/DLac_LU_utm.shp')
pnt3 <- st_read('../data/shp/points/ToShare/coffee/DNong_LUutm.shp')
pnt4 <- st_read('../data/shp/points/ToShare/coffee/GLai_LUutm.shp')
mask <- raster('../data/raster/climate/current/wc20/bio_1.tif')
prds <- st_read('../data/shp/harvested/hrv_13_18.shp')

# Add indicator -----------------------------------------------------------
pnt1 <- pnt1 %>% 
  mutate(point = 'points_coffee') %>% 
  dplyr::select(Class, Year)

pnt2 <- pnt2 %>% 
  mutate(point = 'DLAC_LU_UTM') %>% 
  dplyr::select(point)

pnt3 <- pnt3 %>% 
  mutate(point = 'DNONG_LUutm') %>% 
  dplyr::select(point)

pnt4 <- pnt4 %>% 
  mutate(point = 'GLai_LUutm') %>% 
  dplyr::select(point)

# Join all the points -----------------------------------------------------
pnts <- bind_rows(pnt1, pnt2, pnt3, pnt4)
pnts <- st_centroid(pnts)
st_write(pnts, '../data/shp/points/coffee_only/coffee_all.shp')

# Remove duplicate by cell ------------------------------------------------
clln <- raster::extract(mask, st_coordinates(pnts), cellnumbers = T)
clls <- xyFromCell(mask, clln[,'cells'])
dupv <- duplicated(clls[,c('x', 'y')])
pncl <- as_tibble(st_coordinates(pnts)[!dupv,])
pncl <- pncl %>% mutate(gid = 1:nrow(.))
pncl <- pncl %>% st_as_sf(., coords = c('X', 'Y'), crs = st_crs(4326))
st_write(pncl, '../data/shp/points/coffee_only/coffee_rmDup2.shp', append = TRUE)
rm(pnt1, pnt2, pnt3, pnt4, clln, clls, dupv)
# Harvested area versus points --------------------------------------------

pncl_vtnm <- st_intersection(x = pncl, y = vtnm)
freq <- table(pncl_vtnm$NAME_1) %>% 
  as.data.frame() %>% 
  setNames(c('name', 'freq'))

vtnm_freq <- inner_join(vtnm, freq, by = c('NAME_1' = 'name'))
prds

pncl_shp <- pncl
pncl_shp$gid <- 1
coordinates(pncl_shp) <- ~ X + Y
vntm_sp <- as(vtnm, 'Spatial')
pncl$name_1 <- raster::extract(vntm_sp, pncl[,1:2]) %>% pull(VARNAME_1)
pncl_cnt <- pncl %>% group_by(name_1) %>% summarise(count = n()) %>% ungroup()
vtnm_freq <- inner_join(vtnm, pncl_cnt, by = c('VARNAME_1' = 'name_1'))

gg_1 <- ggplot() +
  geom_sf(data = vtnm, fill = NA) +
  geom_sf(data = vtnm_freq, aes(fill = count)) +
  scale_fill_gradientn(colors = brewer.pal(n = 7, name = 'YlOrBr')) +
  ggtitle(label = 'Number of presences (n)') +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2, 'line'),
        panel.grid = element_blank())  +
  labs(fill = 'Presences (n)') +
  scale_x_discrete(breaks = c(103, 105, 107, 109))

pncl_sf <- pncl %>% st_as_sf(., coords = c('X', 'Y'), crs = st_crs(4326))

gg_2 <- ggplot() +
  geom_sf(data = prds, aes(fill = hrv)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 7, name = 'Oranges')) +
  ggtitle(label = 'Harvest area (ha)') + 
  geom_sf(data = vtnm, fill = NA) +
  theme_bw() +
  theme(legend.position = 'bottom', 
        legend.key.width = unit(2, 'line'),
        panel.grid = element_blank())  +
  labs(fill = 'Harvested \narea(ha)') + 
  scale_x_discrete(breaks = c(103, 105, 107, 109))

pncl_vtnm_2 <- pncl_vtnm

gg_3 <- ggplot() +
  geom_sf(data = pncl_sf, col = 'brown', size = 0.8) + 
  geom_sf(data = vtnm, fill = NA) +
  ggtitle(label = 'Coffee points') + 
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.grid = element_blank()) +
  scale_x_discrete(breaks = c(103, 105, 107, 109))

gg_all <- ggarrange(gg_1, gg_2, gg_3, 
                    ncol = 3, nrow = 1)

ggsave(plot = gg_all, filename = '../png/maps/Map 03 presences harvested 5km.png', units = 'in',
       width = 12, height = 7, dpi = 300)

# Just one map -----------------------------------------------------------

# Presences

lon = vtnm %>% st_centroid() %>% st_coordinates() %>% as.data.frame %>% pull(1) 
lat = vtnm %>% st_centroid() %>% st_coordinates() %>% as.data.frame %>% pull(2)

crds <- vtnm %>% 
  mutate(x = lon, y = lat) %>% 
  as.data.frame %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  filter(NAME_1 %in% c("??k L?k",  "??k Nông", "Gia Lai",  "Kon Tum",  "Lâm ??ng"))

gg_4 <- ggplot() +
  geom_sf(data = pncl_vtnm, col = 'brown', size = 0.8) + 
  geom_sf(data = vtnm, fill = NA) +
  ggtitle(label = 'Coffee points') + 
  geom_text_repel(data = crds, aes(x = x, y = y, label = NAME_1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.grid = element_blank()) +
  scale_x_discrete(breaks = c(107, 108, 109)) +
  coord_sf(xlim = extent(pncl_vtnm)[1:2], ylim = extent(pncl_vtnm)[3:4])

ggsave(plot = gg_4, filename = '../png/maps/Map 04 points localization.png', units = 'in', 
       width = 5, height = 7, dpi = 300)


# Production
prd_10 <- prds %>% 
  filter(hrv > 0) %>% 
  as.data.frame %>% 
  as_tibble %>% 
  dplyr::select(-geometry) %>% 
  dplyr::select(NAME_1, hrv) %>% 
  arrange(desc(hrv)) %>% 
  mutate(NAME_1 = factor(NAME_1, levels = NAME_1)) %>% 
  top_n(x = ., n = 10, wt = hrv)
gg_5 <- ggplot(data = prd_10, aes(x = NAME_1, y = hrv)) + 
  geom_col() + 
  labs(x = '', y = 'Harvested area (ha)') + 
  theme_ipsum_es() + 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))  
ggsave(plot = gg_5, filename = '../png/grp/top_10_hrv.png', units = 'in', width = 11, height = 7, dpi = 300)

sum(prds$hrv)
sum(prd_10$hrv)

# Map production top 10 
prds_10 <- prds %>% 
  filter(NAME_1 %in% pull(prd_10, 1)) 

gg_6 <- ggplot() + 
  geom_sf(data = prds_10, aes(fill = hrv)) + 
  geom_sf(data = vtnm, fill = NA, col = 'slategray') + 
  scale_fill_gradientn(colours = brewer.pal(name = 'YlOrBr', n = 5)) +
  coord_sf(xlim = extent(prds_10)[1:2], ylim = c(10.8, 21.8)) +
  labs(x = 'Lon', y = 'Lat', fill = 'Harvested\nArea (ha)') +
  theme_bw() +
  theme(legend.position = 'top',
        legend.key.width = unit(1.8, 'line')) +
  scale_x_discrete(breaks = c(103, 105, 107, 109)) 

ggsave(plot = gg_6, filename = '../png/maps/Map 05 hrv top 10.png', units = 'in', width = 4, height = 7, dpi = 300)

# Production versus presences count
vtnm_freq <- vtnm_freq %>% dplyr::select(NAME_0, NAME_1, freq)
inner_join(prds, vtnm_freq)
prds_2 <- full_join(prds, vtnm_freq %>% as.data.frame %>% dplyr::select(-geometry), by = 'NAME_1')
prds_2 <- prds_2 %>% filter(hrv > 0)
prds_2 <- prds_2 %>% dplyr::select(NAME_1, freq, hrv)

as.data.frame(prds_2) %>% 
  dplyr::select(-geometry) %>% 
  filter(!is.na(freq)) %>% 
  write.csv(., '../data/tbl/smm/smm_pnt_prd.csv', row.names = F)

as.data.frame(prds_2) %>% 
  dplyr::select(-geometry) %>% 
  mutate(hrv = round(hrv, 0)) %>% 
  arrange(desc(freq))

prds_3 <- prds_2 %>% 
  mutate(hrv = round(hrv, 0)) %>% 
  mutate(freq = ifelse(is.na(freq), 0, freq)) %>% 
  mutate(freq_prop = round(freq / sum(freq), 4),
         hrv_prop = round(hrv / sum(hrv), 4),
         global_prob = round(freq_prop / hrv_prop, 4))

prds_3 <- as(prds_3, 'Spatial')
mask <- mask * 0 + 1

# Remove duplicated by cell mask 5
mask_5km <- raster::getData(name = 'worldclim', var = 'prec', res = 2.5, lon = 105.3186, lat = 20.04625)
mask_5km <- mask_5km[[1]]
mask_5km <- mask_5km * 0 + 1
mask_5km <- mask_5km %>% raster::crop(., as(vtnm, 'Spatial')) %>% raster::mask(., as(vtnm, 'Spatial'))

clln <- raster::extract(mask_5km, st_coordinates(pncl), cellnumbers = T)
clls <- xyFromCell(mask_5km, clln[,'cells'])
dupv <- duplicated(clls[,c('x', 'y')])
pncl <- as_tibble(st_coordinates(pncl)[!dupv,])

bias.raster <- rasterize(prds_3, mask, field = 'hrv_prop')
speciescell <- raster::extract(bias.raster, pncl[,1:2], cellnumber = T)
bias.raster[speciescell[,1]] <- NA
back <- dismo::randomPoints(mask = bias.raster, n = nrow(pncl), prob = TRUE)
back <- as.data.frame(back)
plot(bias.raster)
points(back$x, back$y, pch = 16, col = 'red')

back <- back %>% as_tibble() %>% mutate(pb = 0)
back
write.csv(back, '../data/tbl/back/back_points_5km.csv', row.names = FALSE)

# Map Background - Harvested
gg_back <- ggplot() + 
  geom_point(data = back, aes(x = x, y = y), size = 0.5, col = 'brown') +
  geom_sf(data = vtnm, fill = NA) +
  theme_bw() + 
  theme(panel.grid = element_blank()) +
  scale_x_discrete(breaks = c(103, 105, 107, 109)) +
  labs(x = 'Lon', y = 'Lat')

ggsave(plot = gg_back, filename = '../png/maps/Map 06 Background 5 km.png',
       units = 'in', width = 6, height = 9, dpi = 300)

back_adm <- raster::extract(as(vtnm, 'Spatial'), back[,1:2])
back_adm <- back_adm %>% pull(VARNAME_1) %>% table() %>% as.data.frame() %>% setNames(c('adm1', 'freq'))
back_adm <- back_adm %>% arrange(desc(freq)) %>% mutate(adm1 = factor(adm1, levels = adm1))

gg <- ggplot(data = back_adm, aes(x = adm1, y = freq)) + 
  geom_col() + 
  geom_text(aes(label = freq), position = position_dodge(width=0.9), vjust=-0.25) +
  theme_ipsum_es() + 
  labs(x = '', y = 'Frequency (n)')

ggsave(plot = gg, filename = paste0('../png/grp/back_adm1.png'), units = 'in',
       width = 9, height = 6, dpi = 300)


