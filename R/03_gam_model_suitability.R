# ------- to  be used for habitat suitability modeling using GAM ----------
# ----------------- load libraries to be used -----------------------------
libs = c("tidyverse",'raster','sf','ncdf4','lubridate','mgcv','mgcViz'); lapply(libs, library, character.only = TRUE)
# ---------------  load catch data ----------------------------------------
col_types_dat = 'ddcdddddd'
proc_dat = read_csv('data/tidy/bio/newly_processed_bio_env.csv',col_types = col_types_dat)%>%
  dplyr::rename(lon = Lon,lat= Lat,sst = sst_vals,chl = chl_vals)

# ------- process  data ---------------------------------------------------
proc_sel = proc_dat%>%
  drop_na()%>%
  mutate(occs = case_when(
    m_cpue >0 ~ 1,
    TRUE ~ 0
  ))%>%mutate(Month = factor(Month),Year = factor(Year))%>%
  nest(data = c(1:2,4:10))

# -------------   check for  multi-collinearity ---------------------------
car::vif(lm(depth~lon+lat+Year+Month+sst+chl,data=proc_sel$data[[1]]))


# ----------------  build gam model ---------------------------------------
gam_1 = "occs ~ s(sst,k = 3)"
gam_2 = "occs ~ s(sst,k = 3) + s(chl,k = 3)"
gam_3 = "occs ~ s(sst,k = 3) + s(chl,k = 3) + s(depth,k =3)"
gam_4 = "occs ~ s(sst,k = 3) + s(chl,k = 3) + s(depth,k =3) + s(lon,k= 3)"
gam_5 = "occs ~ s(sst,k = 3) + s(chl,k = 3) + s(depth,k =3) + s(lat, k= 3)"
gam_6 = "occs ~ s(sst,k = 3) + s(chl,k = 3) + s(depth,k =3) + s(lon,lat)"
gam_7 = "occs ~ s(sst,k = 3) + s(chl,k = 3) + s(depth,k =3) +  s(lon,lat) + Year"
gam_8 = "occs ~ s(sst,by = Month,k = 3) + s(chl,k = 3) + s(depth, k = 3) +s(lon,lat) + Year"
gam_9 = "occs ~ s(sst,by = Month,k =3) + s(chl,by = Month,k = 3) + s(depth,k =3) + s(lon,lat) +Year"
all_models = c(gam_1,gam_2,gam_3,gam_4,gam_5,gam_6,gam_7,gam_8,gam_9)

model_species = tibble(models = 1:length(all_models),
                       formulas = as.list(all_models))

# ------- process  data for modelling ---------------------------------------------------
n_na = function(x) !is.na(x)
unq_vals = purrr::map(proc_dat%>%dplyr::select(1:2,5:6,20:28),.f = function(x =.x)  x%>%unique()%>%n_na()%>%sum())
unique_observations = as_tibble(unq_vals)


# -----------  custom utility function fit gam ----------------------------
custom_gam =  function(dat,forms = model_species){
  mod_fits = forms%>%
    mutate(gam_fits = purrr::map(.x = formulas,.f = ~gam(formula = as.formula(.x),data = dat,family = binomial)),
           aucs = purrr::map(.x = gam_fits,.f = ModelMetrics::auc))
  mod_fits
}

# ----------------  gam modelling -----------------------------------------
system.time(gam_all <- proc_sel%>%
  mutate(gams = purrr::map(.x = data,.f = ~custom_gam(dat = .x))))

all_model_perfs = gam_all%>%dplyr::select(-data)%>%
   unnest(gams)%>%dplyr::select(Species,models,formulas,aucs)%>%
  unnest(cols = c(aucs,formulas))

all_model_perfs%>%
  mutate(formulas = forcats::fct_reorder(formulas,aucs))%>%
  ggplot()+
  geom_point(aes(y = formulas,x = aucs))+
  facet_wrap(~Species)+
  theme_bw()+
  theme(strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text.x = element_text(colour = 'black', size = 12),
        axis.text.y = element_text(color = 'black',size = 8),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        laegend.title = element_text(size = 12,color='black'))


# ------------   prepare  prediction grid ---------------------------------
load('analysis/pred_grid_all.RData')

pred_grid = pred_grd%>%
  dplyr::rename(lon = x,lat = y)%>%
  mutate(Year = factor(Year),Month = factor(Month))

all_spp_pred = tibble(Species = proc_sel%>%dplyr::select(Species)%>%pull(),pred_df = list(pred_grid))

sel_gam_model = gam_all%>%dplyr::select(-data)%>%unnest(cols = gams)%>%
  filter(models==7)%>%dplyr::select(Species,gam_fits)

# ------ gam model response curves ----------------------------------------
cust_gam_plt = function(the_model){
  t_plt = mgcViz::getViz(the_model)
  list(print(plot(t_plt,allTerms = TRUE),pages = 1))
}
cust_gam_diag = function(the_model){
  t_plt = mgcViz::getViz(the_model)
  qq(t_plt, method = "simul1", a.qqpoi = list("shape" = 1), a.ablin = list("linetype" = 2))+labs(title = '') 
}

gam_all_resp_diag = sel_gam_model%>%
  mutate(resp =  purrr::map(.x = gam_fits,.f = ~cust_gam_plt(the_model = .x)),
         diags = purrr::map(.x = gam_fits, .f = ~cust_gam_diag(the_model = .x)))

  
# -------------------------  model prediction -----------------------------
gams_pred_df = sel_gam_model%>%left_join(all_spp_pred)

pred_gams = gams_pred_df%>%
  mutate(pred = purrr::map2(.x = gam_fits,.y = pred_df,.f = ~predict(object = .x,newdata = .y,type='response')))%>%
  dplyr::select(Species,pred_df,pred)%>%
  unnest(cols = c(pred_df,pred))

# ----------- distribution maps -------------------------------------------
load('analysis/base_map_related.RData')

alb_dist = pred_gams%>%filter(Species=='Albacore')%>%
  ggplot()+
  geom_tile(aes(lon,lat,fill=pred))+
  geom_sf(data = Tonga_eez,fill=NA)+
  labs(y = 'latitude',x = 'longitude')+
  scale_fill_gradientn(colours = fields::tim.colors(100),
                       name = 'probability of\n occurrence',
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

bet_dist = pred_gams%>%filter(Species=='Bigeye')%>%
  ggplot()+
  geom_tile(aes(lon,lat,fill=pred))+
  geom_sf(data = Tonga_eez,fill=NA)+
  labs(y = 'latitude',x = 'longitude')+
  scale_fill_gradientn(colours = fields::tim.colors(100),
                       name = 'probability of\n occurrence',
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

skj_dist = pred_gams%>%filter(Species=='Skipjack')%>%
  ggplot()+
  geom_tile(aes(lon,lat,fill=pred))+
  geom_sf(data = Tonga_eez,fill=NA)+
  labs(y = 'latitude',x = 'longitude')+
  scale_fill_gradientn(colours = fields::tim.colors(100),
                       name = 'probability of\n occurrence',
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

yfn_dist = pred_gams%>%filter(Species=='Yellowfin')%>%
  ggplot()+
  geom_tile(aes(lon,lat,fill=pred))+
  geom_sf(data = Tonga_eez,fill=NA)+
  labs(y = 'latitude',x = 'longitude')+
  scale_fill_gradientn(colours = fields::tim.colors(100),
                       name = 'probability of\n occurrence',
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
# ------------ write dist maps --------------------------------------------

ggsave(filename = 'figures/albacore_habitat_suitabilty.png',plot = alb_dist,device = 'png',
       width = 60,height = 35,units = 'cm')
ggsave(filename = 'figures/skipjack_habitat suitability.png',plot = skj_dist,device = 'png',
       width = 60,height = 35,units = 'cm')
ggsave(filename = 'figures/bigeye_habitat_suitability.png',plot = bet_dist,device = 'png',
       width = 60,height = 35,units = 'cm')
ggsave(filename = 'figures/yellowwfin_habitat_suitability.png',plot = yfn_dist,device = 'png',
       width = 60,height = 35,units = 'cm')



