

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, terra, fs, 
               glue, gtools, terra, geodata, colorspace, ggthemes, 
               ggspatial, RColorBrewer)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
vars <- c('bio_1.tif', 'bio_12.tif')
vtnm <- geodata::gadm(country = 'VNM', level = 0, path = tempdir())
vtnm <- terra::vect('../1.Data/shp/base/vtm_adm1.shp')

# Baseline ----------------------------------------------------------------
bsln.fles <- dir_ls('../1.Data/climate/current/wc20', regexp = '.tif$')
bsln.fles <- grep(paste0(vars, collapse = '|'), bsln.fles, value = TRUE)
bsln.fles <- as.character(bsln.fles)

bsln.bi01 <- terra::rast(bsln.fles[1])
bsln.bi12 <- terra::rast(bsln.fles[2])

# Future ------------------------------------------------------------------
ftre.fles <- dir_ls('../1.Data/climate/future/cmip6/ssp370/2021_2040/wc2.1_30s')
ftre.fles <- grep(paste0(c('bio_1_', 'bio_12_'), collapse = '|'), ftre.fles, value = TRUE)
ftre.fles <- as.character(ftre.fles)
ftre.fles <- mixedsort(ftre.fles)

# SSPs
ssps <- grep('bio_1_', ftre.fles, value = TRUE) %>% 
  basename() %>% 
  str_split(., '_') %>% 
  sapply(., `[[`, 3) %>% 
  gsub('.tif', '', .) %>% 
  mixedsort()

# Values 
bi01.vles <- grep('bio_1_', ftre.fles, value = TRUE) %>% 
  map(.x = ., .f = terra::rast) %>% 
  map(.x = ., .f = terra::as.data.frame) %>% 
  map(.x = ., .f = as_tibble) %>% 
  map(.x = ., .f = function(x) x %>% setNames(c('value'))) %>% 
  bind_rows()
bi12.vles <- grep('bio_12_', ftre.fles, value = TRUE) %>% 
  map(.x = ., .f = terra::rast) %>% 
  map(.x = ., .f = terra::as.data.frame) %>% 
  map(.x = ., .f = as_tibble) %>% 
  map(.x = ., .f = function(x) x %>% setNames(c('value'))) %>% 
  bind_rows()

hist(bi01.vles$value)
hist(bi12.vles$value)
range(bi12.vles$value)

hcl_palettes(plot = TRUE)

# Function ----------------------------------------------------------------
make.map <- function(ssp, var, unt){
  
  # ssp <- ssps[4]
  # var <- 'bio_1_'
  # unt <- '°C'
  
  cat('Start ', ssp, '\n')
  crn <- terra::rast(grep(paste0(gsub('_$', '', var), '.tif'), bsln.fles, value = TRUE))
  crn <- terra::crop(crn, vtnm) %>% terra::mask(., vtnm)
  ftr <- terra::rast(grep(var, grep(ssp, ftre.fles, value = TRUE), value = TRUE))
  ftr <- terra::crop(ftr, vtnm) %>% terra::mask(., vtnm)
  rng <- range(as.numeric(na.omit(c(crn[], ftr[]))))
  trr <- terra::rast(raster::stack(raster(crn), raster(ftr)))
  tbl <- as_tibble(terra::as.data.frame(trr,  xy = TRUE))
  colnames(tbl) <- c('x', 'y', 'Current', 'Future')
  tbl <- gather(tbl, period, value, -x, -y)
  tbl <- mutate(tbl, period = factor(period, levels = c('Current', 'Future')))
  
  cat('To make the map\n')
  ggp <- ggplot() + 
    geom_tile(data = tbl, aes(x = x, y = y, fill = value)) +
    facet_wrap(.~period) +
    # scale_fill_binned_diverging(palette = 'Blue-Yellow 2') +
    scale_fill_gradientn(colors = brewer.pal(name = 'YlOrRd', n = 9)) +
    geom_sf(data = st_as_sf(vtnm), fill = NA, col = 'white') +
    coord_sf() + 
    theme_pander() +
    theme(legend.position = 'bottom', 
          legend.key.width = unit(1.5, 'cm'), 
          strip.text.x = element_text(size = 14, face = 'bold')) +
    scale_x_discrete(breaks = c(102, 104, 106, 108)) +
    labs(x = '', y = '', fill = glue('{unt}')) + 
    annotation_north_arrow(location = 'tr', which_north= 'true', style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'bl', bar_cols = c("grey60", "white"), text_family = "ArcherPro Book") 
  
  cat('To make the map of the difference\n')
  dfr <- trr[[2]] - trr[[1]] 
  dfr <- as_tibble(terra::as.data.frame(dfr, xy = TRUE))
  colnames(dfr) <- c('x', 'y', 'value')
  
  ggd <- ggplot() + 
    geom_tile(data = dfr, aes(x = x, y = y, fill = value)) +
    scale_fill_binned_diverging(palette = 'Blue-Red') +
    # scale_fill_gradientn(colors = brewer.pal(name = 'YlOrRd', n = 9)) +
    geom_sf(data = st_as_sf(vtnm), fill = NA, col = 'white') +
    coord_sf() + 
    ggtitle(label = 'Difference') +
    scale_x_discrete(breaks = c(102, 104, 106, 108)) +
    labs(x = '', y = '', fill = glue('Difference {unt}')) +
    theme_pander() +
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 14, face = 'bold', hjust = 0.5),
          legend.key.width = unit(1.3, 'cm')) + 
    annotation_north_arrow(location = 'tr', which_north= 'true', style = north_arrow_fancy_orienteering()) +
    annotation_scale(location = 'bl', bar_cols = c("grey60", "white"), text_family = "ArcherPro Book") 
  
  cat('Join both maps\n')
  lyt <- matrix(c(1, 1, 2, 1, 1, 2), byrow = TRUE, ncol = 3, nrow = 2)
  gga <- gridExtra::grid.arrange(ggp, ggd, layout_matrix = lyt)
  ggsave(plot = gga, filename = glue('../6.Png/maps/map_bsl_ftr_{ssp}.png'), units = 'in', width = 12, height = 7, dpi = 300)
  
}


# Make maps ---------------------------------------------------------------

map(.x = 3:length(ssps), .f = function(i){make.map(ssp = ssps[i], var = 'bio_1_', unt = '°C')})






