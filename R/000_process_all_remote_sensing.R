# ------------  to be used to process all the remote sensing data ---------
# --------------- load libraries to be used  ------------------------------
libs = c("tidyverse",'raster','sf','ncdf4','lubridate'); lapply(libs, library, character.only = TRUE)

#  ------- load and process the data to be used ---------------------------
all_sst = list.files('data/raw/env/sst',full.names = TRUE)
all_chl = list.files('data/raw/env/chl',full.names = TRUE)


# --------   load base map related data -----------------------------------
load('analysis/base_map_related.RData')
tonga_eez_sp = Tonga_eez%>%as_Spatial()

catch_grd = raster(nrows =8,ncols=12,xmn = -179.6,xmx =-170,ymn =-26,ymx=-13)
# --------------- utility for data processing -----------------------------
refernce_time = '1970-01-01 00:00:00'

convert_date_time = function(x,refer = refernce_time){
  cor_date=as_datetime(x,origin=refer)%>%as_date()
  cor_date
}

read_process_dat = function(the_file,agg_fact,which = 'sst'){
  the_raster = raster::stack(the_file)
  #raster_mask = raster::mask(x = the_raster,mask = tonga_eez_sp)
  raster_mask = the_raster
  names_rast = names(raster_mask)
  date_tmp = tibble(index = 1:length(names_rast))
  date_tmp = date_tmp%>%
    mutate(dates = purrr::map(str_remove_all(names_rast,'X')%>%as.numeric(),convert_date_time))%>%
    unnest(cols = dates)
  names(raster_mask) = date_tmp$dates
  t_year = year(date_tmp$dates)[1]
  
  rast_agg = raster::aggregate(raster_mask,fact = agg_fact)
  
  raster::writeRaster(rast_agg,filename = here::here('data/tidy/env',which,paste(which,'_',t_year,'.grd',sep = '')),
                      overwrite = TRUE)
  
}

#  ------------ read process and write all rasters ------------------------
purrr::map(as.list(all_chl),.f = ~read_process_dat(the_file =   .x,agg_fact = 8,which = 'chl'))
purrr::map(as.list(all_sst),.f = ~read_process_dat(the_file = .x,agg_fact = 33,which = 'sst'))

date_store = tibble(index = 1:length(t_name))
date_store = date_store%>%
  mutate(dates = purrr::map(str_remove_all(t_name,'X')%>%as.numeric(),convert_date_time))%>%
  unnest(cols = dates)
names(t_rast) = date_store$dates

# aggregate to 20NM (0.33333 deg) .333333/res(dat)
t_rast = raster::stack(all_chl[1], varname = 'chlor_a')
t_ag = raster::aggregate(t_rast,fact = 33)
ch_agg = raster::aggregate(t_chl,fact=8)

# ------ read, process and write bathymetry data --------------------------
depth_rast = raster('data/raw/env/tonga_bathy.grd')
#depth_crp = raster::mask(x = depth_rast,mask = tonga_eez_sp)
depth_agg = raster::aggregate(depth_rast,fact =4)
raster::writeRaster(depth_agg,filename = 'data/tidy/env/bathy/bathy_agg.grd',overwrite = TRUE)

