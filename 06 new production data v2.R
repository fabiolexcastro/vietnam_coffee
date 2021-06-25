
# Target: Join between production table and shapefile


# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, readxl, rgdal, rgeos, stringr, stringi, sf, tidyverse, gtools)
g <- gc(reset = T); rm(list = ls())

# Load data ---------------------------------------------------------------
adm2 <- st_read('../data/shp/base/v2/VNM_adm2.shp')
adm3 <- st_read('../data/shp/base/v2/VNM_adm3.shp')
tble <- read_excel('../data/tbl/hrv/raw/Area_Production_2000-2018_District.xlsx', sheet = 3)

# Tidy characters - table -------------------------------------------------
tble <- mutate(tble, NAME_adm1 = stri_trans_general(NAME_adm1, 'Latin-ASCII'))
tble_1 <- tble[grep('Huyen' ,pull(tble, 2)),]  %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tble_2 <- tble[grep('TP', pull(tble, 2)),] %>% mutate(NAME_adm2 = gsub('TP. ', '', NAME_adm2))
tble_3 <- tble[grep('Thanh pho', pull(tble, 2)),] %>% mutate(NAME_adm2 = gsub('Thanh pho ', '', NAME_adm2))

# Only the different fields
tble_4 <- tble[-grep(paste0(c('Huyen', 'TP', 'Thanh pho '), collapse = '|'), pull(tble, 2)),] 
tble <- bind_rows(tble_1, tble_2, tble_3, tble_4); rm(tble_1, tble_2, tble_3)
tble <- tble %>% mutate(NAME_adm2 = stri_trans_general(NAME_adm2, 'Latin-ASCII'))
write.csv(tble, '../data/tbl/hrv/process/Area_Production_2000-2018_District_v4.csv', row.names = FALSE)

# Tidy shapefile ----------------------------------------------------------
adm2 <- adm2 %>% dplyr::select(NAME_0, NAME_1, NAME_2, VARNAME_2)
adm2 <- adm2 %>% mutate(NAME_1 = stri_trans_general(NAME_1, 'Latin-ASCII'), NAME_2 = stri_trans_general(NAME_2, 'Latin-ASCII'))
adm3 <- adm3 %>% dplyr::select(NAME_0, NAME_1, NAME_2, NAME_3, VARNAME_3)
adm3 <- st_join(x = adm3, y = adm2)
adm3 <- adm3 %>% dplyr::select(-NAME_0.x, -NAME_0.y)
adm3 <- adm3 %>% dplyr::select(NAME_3, VARNAME_2, VARNAME_3)

# -------------------------------------------------------------------------
# Dien Bien
# -------------------------------------------------------------------------
tbl_dien_bien <- tble %>% filter(NAME_adm1 %in% 'Dien Bien') 
tbl_dien_bien <- tbl_dien_bien %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_dien_bien <- tbl_dien_bien %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))

sft_dien_bien <- adm3 %>% filter(VARNAME_2 %in% 'Dien Bien')
sft_tbl_dien_bien <- inner_join(sft_dien_bien, tbl_dien_bien, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_dien_bien <- anti_join(tbl_dien_bien, sft_tbl_dien_bien, by = c('NAME_adm2' = 'VARNAME_3'))

# -------------------------------------------------------------------------
# Quang Tri
# -------------------------------------------------------------------------
tbl_quang_tri <- tble %>% filter(NAME_adm1 %in% 'Quang Tri')
sft_quang_tri <- adm3 %>% filter(VARNAME_2 %in% 'Quang Tri')
tbl_quang_tri <- tbl_quang_tri %>% mutate(NAME_adm2 = gsub('Dakrong', 'Da Krong', NAME_adm2))
tbl_quang_tri <- tbl_quang_tri %>% mutate(NAME_adm2 = gsub('TX. Quang Tri', 'Quang Tri Township', NAME_adm2))

sft_tbl_quang_tri <- inner_join(sft_quang_tri, tbl_quang_tri, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_quang_tri <- anti_join(tbl_quang_tri, sft_tbl_quang_tri, by = c('NAME_adm2' = 'VARNAME_3'))

# -------------------------------------------------------------------------
# Dak Lak
# -------------------------------------------------------------------------
tbl_dak_lak <- tble %>% filter(NAME_adm1 %in% 'Dak Lak') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2), NAME_adm2 = gsub(':', '', NAME_adm2))
tbl_dak_lak <- tbl_dak_lak %>% mutate(NAME_adm2 = gsub('Krong pac', 'Krong Pac |Krong Pak', NAME_adm2))
sft_dak_lak <- adm3 %>% filter(VARNAME_2 %in% 'Dak Lak|Dac Lac')

sft_tbl_dak_lak <- inner_join(sft_dak_lak, tbl_dak_lak, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_dak_lak <- anti_join(tbl_dak_lak, sft_tbl_dak_lak, by = c('NAME_adm2' = 'VARNAME_3'))

trb_tbl_dak_lak %>% pull(2) %>% sort()
sft_dak_lak %>% pull(3) %>% sort()

# -------------------------------------------------------------------------
# Son La
# -------------------------------------------------------------------------
tbl_son_la <- tble %>% filter(NAME_adm1 %in% 'Son La')%>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_son_la <- tbl_son_la %>% mutate(NAME_adm2 = gsub('Thanh pho ', '', NAME_adm2))
sft_son_la <- adm3 %>% filter(VARNAME_2 %in% 'Son La')

sft_tbl_son_la <- inner_join(sft_son_la, tbl_son_la, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_son_la <- anti_join(tbl_son_la, sft_son_la, by = c('NAME_adm2' = 'VARNAME_3'))
nrow(tbl_son_la)
nrow(sft_tbl_son_la)

trb_tbl_son_la %>% pull(2) %>% sort()
sft_son_la %>% pull(3) %>% sort()
tble %>% pull(1) %>% unique()

# -------------------------------------------------------------------------
# Than Hoa
# -------------------------------------------------------------------------
tbl_than_hoa <- tble %>% filter(NAME_adm1 %in% 'Thanh Hoa')
sft_than_hoa <- adm3 %>% filter(VARNAME_2 %in% 'Thanh Hoa')

sft_tbl_than_hoa <- inner_join(sft_than_hoa, tbl_than_hoa, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_than_hoa <- anti_join(tbl_than_hoa, sft_tbl_than_hoa, by = c('NAME_adm2' = 'VARNAME_3'))

trb_tbl_than_hoa %>% pull(2) %>% sort()
sft_than_hoa %>% pull(3) %>% sort()

# -------------------------------------------------------------------------
# Nghe An
# -------------------------------------------------------------------------
tbl_nghe_an <- tble %>% filter(NAME_adm1 %in% 'Nghe An') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
sft_nghe_an <- adm3 %>% filter(VARNAME_2 %in% 'Nghe An')

sft_tbl_nge_an <- inner_join(sft_nghe_an, tbl_nghe_an, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_nge_an <- anti_join(tbl_nghe_an, sft_tbl_nge_an, by = c('NAME_adm2' = 'VARNAME_3'))

trb_tbl_nge_an %>% pull(2) %>% sort()
sft_nghe_an %>% pull(3) %>% sort()

# -------------------------------------------------------------------------
# Quang Binh
# -------------------------------------------------------------------------
tbl_quang_bing <- tble %>% filter(NAME_adm1 %in% 'Quang Binh') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_quang_bing <- tbl_quang_bing %>% mutate(NAME_adm2 = gsub('Thanh pho ', '', NAME_adm2))
sft_quang_bing <- adm3 %>% filter(VARNAME_2 %in% 'Quang Binh')

sft_tbl_quang_bing <- inner_join(sft_quang_bing, tbl_quang_bing, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_quang_bing <- anti_join(tbl_quang_bing, sft_tbl_quang_bing, by = c('NAME_adm2' = 'VARNAME_3'))

as.data.frame(trb_tbl_quang_bing)
trb_tbl_quang_bing %>% pull(2) %>% sort()
sft_quang_bing %>% pull(3) %>% sort()
tble %>% pull(1) %>% unique()
adm3 %>% pull(2) %>% unique() %>% sort()

# -------------------------------------------------------------------------
# Thua Thien Hue
# -------------------------------------------------------------------------
tbl_thua_thien <- tble %>% filter(NAME_adm1 %in% 'Thua Thien Hue') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_thua_thien <- tbl_thua_thien %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))
sft_thua_thien <- adm3 %>% filter(VARNAME_2 %in% 'Thua Thien - Hue')

sft_tbl_thua_thien <- inner_join(sft_thua_thien, tbl_thua_thien, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_thua_thien <- anti_join(tbl_thua_thien, sft_tbl_thua_thien, by = c('NAME_adm2' = 'VARNAME_3'))
trb_tbl_thua_thien %>% pull(2)

# -------------------------------------------------------------------------
# Binh Dinh - ADM2
# -------------------------------------------------------------------------
tbl_binh_dinh <- tble %>% filter(NAME_adm1 %in% 'Binh Dinh')
sft_binh_dinh <- adm2 %>% filter(VARNAME_2 %in% 'Binh Dinh')

sft_tbl_binh_dinh <- inner_join(sft_binh_dinh, tbl_binh_dinh, by = c('VARNAME_2' = 'NAME_adm1'))

# -------------------------------------------------------------------------
# Phu Yen
# -------------------------------------------------------------------------
tbl_phu_yen <- tble %>% filter(NAME_adm1 %in% 'Phu Yen')
sft_phu_yen <- adm2 %>% filter(VARNAME_2 %in% 'Phu Yen')

sft_tbl_phu_yen <- inner_join(sft_phu_yen, tbl_phu_yen, by = c('VARNAME_2' = 'NAME_adm1'))

# -------------------------------------------------------------------------
# Khanh Hoa
# -------------------------------------------------------------------------
tble %>% pull(1) %>% unique()
tbl_khan_hoa <- tble %>% filter(NAME_adm1 %in% 'Khanh Hoa') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
sft_khan_hoa <- adm3 %>% filter(VARNAME_2 %in% 'Khanh Hoa')

sft_tbl_khan_hoa <- inner_join(sft_khan_hoa, tbl_khan_hoa, by = c('VARNAME_3' = 'NAME_adm2'))

# -------------------------------------------------------------------------
# Ninh Thuan
# -------------------------------------------------------------------------
tbl_ninh_thuan <- tble %>% filter(NAME_adm1 %in% 'Ninh Thuan')
sft_ninh_thuan <- adm2 %>% filter(VARNAME_2 %in% 'Ninh Thuan')

sft_tbl_ninh_thuan <- inner_join(sft_ninh_thuan, tbl_ninh_thuan, by = c('VARNAME_2' = 'NAME_adm1'))

# -------------------------------------------------------------------------
# Kon Tum
# -------------------------------------------------------------------------
tbl_kon_tum <- tble %>% filter(NAME_adm1 %in% 'Kon Tum') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_kon_tum <- tbl_kon_tum %>% mutate(NAME_adm2 = gsub("Ia H'Drai", 'Ia Grai', NAME_adm2))
sft_kon_tum <- adm3 %>% filter(VARNAME_2 %in% 'Kon Tum')

sft_tbl_kon_tum <- inner_join(sft_kon_tum, tbl_kon_tum, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_kon_tum <- anti_join(tbl_kon_tum, sft_tbl_kon_tum, by = c('NAME_adm2' = 'VARNAME_3'))

sft_kon_tum %>% pull(3) %>% sort()
tbl_kon_tum %>% pull(2) %>% sort()
trb_tbl_kon_tum %>% pull(2) %>% sort()

# -------------------------------------------------------------------------
# Gia Lai
# -------------------------------------------------------------------------
tbl_gia_lai <- tble %>% filter(NAME_adm1 %in% 'Gia Lai') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_gia_lai <- tbl_gia_lai %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))
tbl_gia_lai <- tbl_gia_lai %>% mutate(NAME_adm2 = gsub('Thi Xa ', '', NAME_adm2))
tbl_gia_lai <- tbl_gia_lai %>% mutate(NAME_adm2 = gsub('Thanh pho  ', '', NAME_adm2))
tbl_gia_lai <- tbl_gia_lai %>% mutate(NAME_adm2 = gsub('KBang', 'Kbang', NAME_adm2))
sft_gia_lai <- adm3 %>% filter(VARNAME_2 %in% 'Gia Lai')

sft_tbl_gia_lai <- inner_join(sft_gia_lai, tbl_gia_lai, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_gia_lai <- anti_join(tbl_gia_lai, sft_tbl_gia_lai, by = c('NAME_adm2' = 'VARNAME_3'))

sft_gia_lai %>% pull(3) %>% sort()
tbl_gia_lai %>% pull(2) %>% sort()
trb_tbl_gia_lai %>% pull(2) %>% sort()

# -------------------------------------------------------------------------
# Dak Nong
# -------------------------------------------------------------------------
tbl_dak_nong <- tble %>% filter(NAME_adm1 %in% 'Dak Nong') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_dak_nong <- tbl_dak_nong %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))
sft_dak_nong <- adm3 %>% filter(VARNAME_2 %in% 'Dac Nong')

sft_tbl_dak_nong <- inner_join(sft_dak_nong, tbl_dak_nong, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_dak_nong <- anti_join(tbl_dak_nong, sft_tbl_dak_nong, by = c('NAME_adm2' = 'VARNAME_3'))

sft_dak_nong %>% pull(3) %>% sort()
tbl_dak_nong %>% pull(2) %>% sort()
trb_tbl_dak_nong %>% pull(2) %>% sort()


# -------------------------------------------------------------------------
# Lam Dong ----------------------------------------------------------------
# -------------------------------------------------------------------------
tbl_lam_dong <- tble %>% filter(NAME_adm1 %in% 'Lam Dong') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_lam_dong <- tbl_lam_dong %>% mutate(NAME_adm2 = gsub('Thanh pho ', '', NAME_adm2))
tbl_lam_dong <- tbl_lam_dong %>% mutate(NAME_adm2 = gsub('Di Linh', 'Duc Linh', NAME_adm2))
sft_lam_dong <- adm3 %>% filter(VARNAME_2 %in% 'Lam Dong')

sft_tbl_lam_dong <- inner_join(sft_lam_dong, tbl_lam_dong, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_dak_nong <- anti_join(tbl_lam_dong, sft_tbl_lam_dong, by = c('NAME_adm2' = 'VARNAME_3'))

sft_lam_dong %>% pull(3) %>% sort()
tbl_lam_dong %>% pull(2) %>% sort()
trb_tbl_dak_nong %>% pull(2) %>% sort()


# -------------------------------------------------------------------------
# Binh Phuo
# -------------------------------------------------------------------------
tbl_binh_phuo <- tble %>% filter(NAME_adm1 %in% 'Binh Phuoc')
tble %>% pull(1) %>% unique()
# Tiene puro NAs

# -------------------------------------------------------------------------
# Binh Duong
# -------------------------------------------------------------------------
tbl_binh_duong <- tble %>% filter(NAME_adm1 %in% 'Binh Duong') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_binh_duong <- tbl_binh_duong %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))
sft_binh_duong <- adm3 %>% filter(VARNAME_2 %in% 'Binh Duong')

sft_tbl_bing_duong <- inner_join(sft_binh_duong, tbl_binh_duong, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_bing_duong <- anti_join(tbl_binh_duong, sft_tbl_bing_duong, by = c('NAME_adm2' = 'VARNAME_3'))

# -------------------------------------------------------------------------
# Dong Nai
# -------------------------------------------------------------------------
tbl_dong_nai <- tble %>% filter(NAME_adm1 %in% 'Dong Nai') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_dong_nai <- tbl_dong_nai %>% mutate(NAME_adm2 = gsub('Thanh pho ', '', NAME_adm2))
tbl_dong_nai <- tbl_dong_nai %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))
sft_dong_nai <- adm3 %>% filter(VARNAME_2 %in% 'Dong Nai')

sft_tbl_dong_nai <- inner_join(sft_dong_nai, tbl_dong_nai, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_dong_nai <- anti_join(tbl_dong_nai, sft_tbl_dong_nai, by = c('NAME_adm2' = 'VARNAME_3'))

sft_dong_nai %>% pull(3) %>% sort()
tbl_dong_nai %>% pull(2) %>% sort()
trb_tbl_dong_nai %>% pull(2) %>% sort()

# -------------------------------------------------------------------------
# Ba Ria - Vung Tau
# -------------------------------------------------------------------------
tbl_baria <- tble %>% filter(NAME_adm1 %in% 'Ba Ria - Vung Tau') %>% mutate(NAME_adm2 = gsub('Huyen ', '', NAME_adm2))
tbl_baria <- tbl_baria %>% mutate(NAME_adm2 = gsub('Thanh pho ', '', NAME_adm2))
tbl_baria <- tbl_baria %>% mutate(NAME_adm2 = gsub('Thi xa ', '', NAME_adm2))
sft_baria <- adm3 %>% filter(VARNAME_2 %in% 'Ba Ria - VTau|Ba Ria-Vung Tau')

sft_tbl_baria <- inner_join(sft_baria, tbl_baria, by = c('VARNAME_3' = 'NAME_adm2'))
trb_tbl_baria <- anti_join(tbl_baria, sft_tbl_baria, by = c('NAME_adm2' = 'VARNAME_3'))

sft_baria %>% pull(3) %>% sort()
tbl_baria %>% pull(2) %>% sort()
trb_tbl_baria %>% pull(2) %>% sort()


# -------------------------------------------------------------------------
# Join
# -------------------------------------------------------------------------

sft_tbl_dien_bien
sft_tbl_quang_tri
sft_tbl_dak_lak
sft_tbl_son_la
sft_tbl_than_hoa
sft_tbl_nge_an
sft_tbl_quang_bing
sft_tbl_thua_thien
sft_tbl_binh_dinh # ADM2
sft_tbl_phu_yen
sft_tbl_khan_hoa
sft_tbl_ninh_thuan
sft_tbl_kon_tum
sft_tbl_gia_lai
sft_tbl_dak_nong
sft_tbl_lam_dong
sft_tbl_bing_duong
sft_tbl_dong_nai
sft_tbl_baria

all_sft <- list(sft_tbl_dien_bien, sft_tbl_quang_tri, sft_tbl_dak_lak, sft_tbl_son_la, sft_tbl_than_hoa,
     sft_tbl_nge_an, sft_tbl_nge_an, sft_tbl_quang_bing, sft_tbl_thua_thien,
     sft_tbl_phu_yen, sft_tbl_khan_hoa, sft_tbl_ninh_thuan, sft_tbl_kon_tum, sft_tbl_gia_lai, 
     sft_tbl_dak_nong, sft_tbl_lam_dong, sft_tbl_bing_duong, sft_tbl_dong_nai, sft_tbl_baria) %>% 
  bind_rows()

st_write(all_sft, '../data/shp/harvested/harvested_adm2_v2.shp')
plot(st_geometry(all_sft))

