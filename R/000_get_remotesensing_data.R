# ------  to be used to download remote sensing data ----------------------
# ------------------------- load libraries to be used ---------------------
libs = c('rerddap','tidyverse','sf','raster','marmap');lapply(libs,library,character.only=TRUE)

# ------------  start exploring and extracting data -----------------------
sst_search = rerddap::ed_search(query = 'sst global monthly')
proc_chl = "pmlEsaCCI42OceanColorMonthly"
proc_sst = "jplMURSST41mday"
chl_info = info(proc_chl)
sst_info = info(proc_sst)



# ----------- utility function to download data ---------------------------
get_chl = function(date1,date2){
  griddap(chl_info,
          time = c(date1, date2),fields = 'chlor_a',
          latitude = c(-26, -13),
          longitude = c(-179.6, -170),fmt = 'nc',
          store = disk(path = 'data/raw/env/chl',overwrite = TRUE),read = FALSE)
}

get_sst = function(date1,date2){
  griddap(sst_info,
          time = c(date1, date2),fields = 'sst',
          latitude = c(-26, -13),
          longitude = c(-179.6, -170),fmt = 'nc',
          store = disk(path = 'data/raw/env/sst',overwrite = TRUE),read = FALSE)
}

# ------  get all data ----------------------------------------------------
raw_date = c('2001-01-01','2019-12-31')
down_chl_df = tibble(date1 = paste(2002:2018,'01','01',sep = '-'),date2 = paste(2002:2018,'12','31',sep = '-'),
                     years = 1:17)
down_sst_df = tibble(date1 = c('2002-06-16',paste(2003:2018,'01','16',sep = '-')),
                     date2 = c('2002-12-16',paste(2003:2018,'12','16',sep = '-')),
                     year = 1:17)

chl_df_split = down_chl_df%>%group_split(years)
sst_df_split = down_sst_df%>%group_split(year)

(chl_sel_02 <- griddap(chl_info,
                time = c('2002-01-01', '2002-12-31'),fields = 'chlor_a',
                latitude = c(-26, -13),
                longitude = c(-179.6, -170),fmt = 'nc',store = disk(path = 'analysis')
))

purrr::map(.x = chl_df_split[15:17],.f = ~get_chl(date1 = .x$date1,date2 = .x$date2))
purrr::map(.x = sst_df_split,.f = ~get_sst(date1 = .x$date1,date2 = .x$date2))

# -------------- get bathymetry -------------------------------------------
bathy_nc = getNOAA.bathy(lon1 = -179.6,lon2 = -170,lat1 = -26,lat2 = -13,
                         resolution = 5,keep = TRUE,path = 'data/raw/env')

bathy_rast = marmap::as.raster(bathy_nc)
raster::writeRaster(x = bathy_rast,filename = here::here('data/raw/env','tonga_bathy.grd'),overwrite = TRUE)

t_bathy = raster('data/raw/env/tonga_bathy.grd')

