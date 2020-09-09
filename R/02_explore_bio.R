# ------  to be used to explore biological data ---------------------------
libs = c('tidyverse','sf');lapply(libs,library,character.only=TRUE)

# ---------- load data to be used -----------------------------------------
proc_dat = read_csv('data/tidy/bio/processed_bio_data.csv')

# ------- process  data ---------------------------------------------------
proc_sel = proc_dat%>%
  dplyr::select(1:2,5:6,10,12,14,16)%>%
  pivot_longer(cols = Albacore_cpue:Yellowfin_cpue,names_to = 'Species',values_to = 'cpue')%>%
  mutate(Species = str_remove_all(string = Species,pattern = '_cpue'))%>%
  group_by(Lon,Lat,Year,Month,Species)%>%
  summarise(m_cpue = mean(cpue,na.rm = TRUE),.groups = 'drop')

proc_sel_sf = st_as_sf(proc_sel,coords = c('Lon','Lat'),crs = 4326)
# ------- load base map related objects -----------------------------------
load('analysis/base_map_related.RData')

ggplot()+
  geom_sf(data = al_tonga_shape,fill = 'gray50')+
  geom_sf(data = Tonga_eez,fill =NA)+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  coord_sf(crs = 4326)+
  theme_bw()

yft_plt = proc_sel_sf%>%
  dplyr::filter(Species=='Yellowfin')%>%
  ggplot()+
  geom_sf(aes(color = m_cpue),size=1)+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  scale_color_gradientn(colours = fields::tim.colors(100),name = 'mean cpue')+
  geom_sf(data = al_tonga_shape,fill = 'gray50')+
  geom_sf(data = Tonga_eez,fill=NA)+
  coord_sf(crs = 4326)+
  facet_grid(Month~ Year)+
  theme_bw()+
  theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        legend.title = element_text(size = 12,color='black'))
bet_plt = proc_sel_sf%>%
  dplyr::filter(Species=='Bigeye')%>%
  ggplot()+
  geom_sf(aes(color = m_cpue),size=1)+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  scale_color_gradientn(colours = fields::tim.colors(100),name = 'mean cpue')+
  geom_sf(data = al_tonga_shape,fill = 'gray50')+
  geom_sf(data = Tonga_eez,fill=NA)+
  coord_sf(crs = 4326)+
  facet_grid(Month~ Year)+
  theme_bw()+
theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
      strip.background = element_rect(fill = 'white'),
      axis.text = element_text(colour = 'black', size = 12),
      axis.title = element_text(size = 12,color='black'),
      legend.text = element_text(color='black',size = 12),
      legend.title = element_text(size = 12,color='black'))
alb_plt = proc_sel_sf%>%
  dplyr::filter(Species=='Albacore')%>%
  ggplot()+
  geom_sf(aes(color = m_cpue),size=1)+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  scale_color_gradientn(colours = fields::tim.colors(100),name = 'mean cpue')+
  geom_sf(data = al_tonga_shape,fill = 'gray50')+
  geom_sf(data = Tonga_eez,fill=NA)+
  coord_sf(crs = 4326)+
  facet_grid(Month~ Year)+
  theme_bw()+
  theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        legend.title = element_text(size = 12,color='black'))
skj_plt = proc_sel_sf%>%
  dplyr::filter(Species=='Skipjack')%>%
  ggplot()+
  geom_sf(aes(color = m_cpue),size=1)+
  scale_x_continuous(n.breaks = 2,breaks = seq(-177,-173,by=3))+
  scale_y_continuous(n.breaks = 3,breaks = seq(-24,-16,by=3))+
  scale_color_gradientn(colours = fields::tim.colors(100),name = 'mean cpue')+
  geom_sf(data = al_tonga_shape,fill = 'gray50')+
  geom_sf(data = Tonga_eez,fill=NA)+
  coord_sf(crs = 4326)+
  facet_grid(Month~ Year)+
  theme_bw()+
  theme(text = element_text(size=12),strip.text = element_text(size = 12,color = 'black'),
        strip.background = element_rect(fill = 'white'),
        axis.text = element_text(colour = 'black', size = 12),
        axis.title = element_text(size = 12,color='black'),
        legend.text = element_text(color='black',size = 12),
        legend.title = element_text(size = 12,color='black'))

ggsave(filename = 'figures/yellowfin_tuna_cpue.png',plot = yft_plt,device = 'png',
       width = 45,height = 45,units = 'cm',dpi = 250)
ggsave(filename = 'figures/bigeye_tuna_cpue.png',plot = bet_plt,device = 'png',
       width = 45,height = 45,units = 'cm',dpi = 250)
ggsave(filename = 'figures/albacore_tuna_cpue.png',plot = alb_plt,device = 'png',
       width = 45,height = 45,units = 'cm',dpi = 250)
ggsave(filename = 'figures/skipjack_tuna_cpue.png',plot = skj_plt,device = 'png',
       width = 45,height = 45,units = 'cm',dpi = 250)
