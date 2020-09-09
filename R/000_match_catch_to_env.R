# -------- linking cactch data to the environmental data ------------------
# ----------------- load libraries to be used -----------------------------
libs = c("tidyverse",'raster','sf','ncdf4','lubridate'); lapply(libs, library, character.only = TRUE)
# ---------------  load catch data ----------------------------------------
proc_dat = read_csv('data/tidy/bio/processed_bio_data.csv')

# ------- process  data ---------------------------------------------------
proc_sel = proc_dat%>%
  dplyr::select(c(1:2,5:6,seq(9,15,by=2)))%>%
  pivot_longer(cols = Albacore_mt:Yellowfin_mt,names_to = 'Species',values_to = 'catch')%>%
  mutate(Species = str_remove_all(string = Species,pattern = '_mt'))%>%
  complete(Year,Month,nesting(Lon,Lat,Species),fill =list(catch=0))%>%
  group_by(Lon,Lat,Year,Month,Species)%>%
  summarise(m_cpue = sum(catch,na.rm = TRUE),.groups = 'drop')

proc_sel_match = proc_sel%>%
  dplyr::select(-m_cpue)%>%
  nest(data = c(Lon,Lat))

# ----------- load base map related data ----------------------------------
load('analysis/base_map_related.RData')

#  ----------- start loading all env data ---------------------------------
sst_all =  list.files('data/tidy/env/sst',pattern = '.grd',full.names = TRUE)
chl_all = list.files('data/tidy/env/chl',pattern = '.grd',full.names = TRUE)
bathy = list.files('data/tidy/env/bathy',pattern = '.grd',full.names = TRUE)

# ------ prep raster data for matching -------------------------------------
all_years = proc_sel_match%>%dplyr::select(Year)%>%distinct()%>%pull()

sst_files = tibble(year = str_extract_all(sst_all,'\\d{4}')%>%as.numeric(),
                   files = as.list(sst_all))
chl_files = tibble(year = str_extract_all(chl_all,'\\d{4}')%>%as.numeric(),
                  files = as.list(chl_all))

# ------------------ utility functions ------------------------------------
get_ext_rast = function(rast_file){
   the_stack = raster::stack(rast_file)
   dates = str_remove_all(names(the_stack),'X')%>%anytime::anydate()
   al_dat = tibble(date = dates,al_rast = raster::unstack(the_stack))
   al_dat
}

extract_envs = function(points, rasts){
  if(!is.null(rasts)){
    t_extract = raster::extract(rasts,as.matrix(points),method = 'simple')
    t_extract
  }else{
    t_extract = NA
  }
  t_extract
}

sst_files = sst_files%>%
  mutate(envs = purrr::map(.x = files,.f = get_ext_rast))%>%
  unnest(cols = envs)%>%mutate(Year = year(date),Month = month(date))%>%
  dplyr::select(Year,Month,al_rast)%>%dplyr::rename(sst = 3)

chl_files = chl_files%>%
  mutate(envs = purrr::map(.x = files, .f = get_ext_rast))%>%
  unnest(cols = envs)%>%mutate(Year = year(date),Month = month(date))%>%
  dplyr::select(Year,Month,al_rast)%>%dplyr::rename(chl = 3)


bathy_sk = raster::raster('data/tidy/env/bathy/bathy_agg.grd')
depth_files = expand.grid(Month = 1:12,Year = all_years)%>%
  dplyr::select(Year,Month)%>%as_tibble()%>%
  mutate(bathy = list(bathy_sk))



# --------------------- prediction grid -----------------------------------
cust_extract = function(x) x%>%rasterToPoints()%>%as_tibble()%>%dplyr::rename(depth = layer)
base_grd = depth_files%>%
  mutate(grd = purrr::map(.x = bathy,.f = cust_extract))%>%
  dplyr::select(-bathy)
pred_grd = base_grd%>%
  left_join(sst_files)%>%
  left_join(chl_files)%>%
  mutate(sst_vals = purrr::map2(.x = sst,.y = grd,.f = ~extract_envs(points = .y[,c(1:2)],rasts = .x)),
        chl_vals = purrr::map2(.x = chl,.y = grd,.f = ~extract_envs(points = .y[,c(1:2)],rasts = .x)))%>%
  dplyr::select(-c(sst,chl))%>%
  dplyr::rename(sst = sst_vals, chl = chl_vals)%>%
  unnest(cols = c(grd,sst,chl))


# --------------- write prediction grid -----------------------------------
save(list = c('pred_grd'),file = 'analysis/pred_grid_all.RData')

# ------ join catch to raster data and extract values ---------------------
catch_env = proc_sel_match%>%left_join(sst_files)%>%
  left_join(chl_files)%>%left_join(depth_files)%>%
  mutate(sst_vals = purrr::map2(.x = data,.y = sst,.f = ~extract_envs(points = .x,rasts = .y)),
         chl_vals = purrr::map2(.x = data,.y = chl,.f = ~extract_envs(points = .x,rasts = .y)),
         depth = purrr::map2(.x = data,.y = bathy,.f = ~extract_envs(points = .x,rasts = .y)))

# ---------- extract environmental data for the catch data ----------------
catch_env_all = catch_env%>%
  dplyr::select(Year,Month,Species,data,sst_vals,chl_vals,depth)%>%
  unnest(cols = c(data,sst_vals,chl_vals,depth))
catch_all_spp_env = catch_env_all%>%
  left_join(proc_sel)


# ----------------- utiity function to crop and extract -------------------
load('analysis/base_map_related.RData')
tonga_eez_sp = Tonga_eez%>%as_Spatial()

crop_extract_env = function(rast,mask_layer = tonga_eez_sp){
  if(!is.null(rast)){
    maskd = raster::mask(rast,mask = mask_layer)
    env_vals = maskd%>%raster::rasterToPoints()%>%as_tibble()%>%dplyr::rename(value = 3)
    env_vals
  }
  
}


# ------------- crrop all env to Tonga EEZ --------------------------------
all_env_crop = proc_sel_match%>%
  dplyr::select(Year,Month)%>%
  distinct()%>%left_join(sst_files)%>%
  left_join(chl_files)%>%left_join(depth_files)

sst_crop = all_env_crop%>%
  dplyr::select(Year,Month,sst)%>%
  mutate(vals = purrr::map(.x = sst,.f = crop_extract_env))%>%
  dplyr::select(-sst)%>%unnest(cols = vals)
chl_crop = all_env_crop%>%
  dplyr::select(Year,Month,chl)%>%
  mutate(vals = purrr::map(.x = chl,.f = crop_extract_env))%>%
  dplyr::select(-chl)%>%unnest(cols = vals)

depth_crop = all_env_crop%>%
  dplyr::filter(Year==2002 & Month==1)%>%
  dplyr::select(Year,Month,bathy)%>%
  mutate(depth = purrr::map(.x = bathy,.f = crop_extract_env))%>%
  dplyr::select(-bathy)%>%
  unnest(cols = depth)

chl_plt = chl_crop%>%
  ggplot()+
  geom_tile(aes(x,y,fill=log(value)))+
  geom_sf(data = Tonga_eez,fill=NA)+
  labs(y = 'latitude',x = 'longitude')+
  scale_fill_gradientn(colours = fields::tim.colors(100),
                       name = expression("chorophyll (log(" * mg^-3 * "))"),
                       guide = 'colorbar')+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  facet_grid(Month ~ Year)+theme_bw()+
  theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        legend.title = element_text(size = 12,color='black'))

sst_plt = sst_crop%>%
  ggplot()+
  geom_tile(aes(x,y, fill = value))+
  labs(y = 'latitude',x = 'longitude')+
  geom_sf(data = Tonga_eez,fill = NA)+
  scale_fill_gradientn(colours = fields::tim.colors(100),name = expression("temperature (" * degree * C *")"),
                       guide = 'colorbar')+
  #guides(color = guide_colourbar(title = expression("temperature (" * degree * C *")")))+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  facet_grid(Month ~ Year)+theme_bw()+
  theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        legend.title = element_text(size = 12,color='black'))

depth_plt = depth_crop%>%
  ggplot()+
  geom_tile(aes(x,y,fill=value))+
  labs(x = 'longitude',y = 'latitude')+
  geom_sf(data = Tonga_eez,fill=NA)+
  scale_fill_gradientn(colours = etopo.colors(100),name = "depth (m)",
                       guide = 'colorbar')+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  theme_bw()+
  theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        legend.title = element_text(size = 12,color='black'))

ggsave(filename = 'figures/newly_processed_sst.png',plot = sst_plt,device = 'png',
       width = 60,height = 35,units = 'cm')
ggsave(filename = 'figures/newly_processed_chl.png',plot = chl_plt,device = 'png',
       width = 60,height = 35,units = 'cm')
ggsave(filename = 'figures/newly_processed_depth.png',plot = depth_plt,device = 'png',
       width = 25,height = 25,units = 'cm')


# --------------- write  all data to file ---------------------------------
save(list = c('depth_crop','sst_crop','chl_crop'),file = 'analysis/processed_all_env.RData')
write_csv(x = catch_all_spp_env,path = 'data/tidy/bio/newly_processed_bio_env.csv',na = c('NA'))

