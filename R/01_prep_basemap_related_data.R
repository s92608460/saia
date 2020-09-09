# ------- to be used to prepare basemap related data  ---------------------
# --------- load libraries to be used -------------------------------------
libs = c('tidyverse','sf','raster','rgdal','rworldmap', 'rworldxtra');lapply(libs, library,character.only = TRUE)

# ------------- load and prep basemap -------------------------------------
world <- getMap(resolution = "high")

world_sf <- st_as_sf(world)
al_tonga_shape <- world_sf%>%dplyr::filter(NAME=='Tonga')%>%
  st_make_valid()%>%
  group_by(NAME)%>%
  summarise(.groups ='drop')

# crs 3460 for Fiji
al_tonga_shape%>%ggplot()+
  geom_sf()+
  coord_sf(crs =  4326)+theme_bw()

# ------ get and process eez shape file -----------------------------------
glob_eez = readOGR(dsn = "spatial/shape", layer = "eez_v10", stringsAsFactors = TRUE)
glob_eez_sf = st_as_sf(glob_eez)
Tonga_eez = glob_eez_sf%>%dplyr::filter(Territory1=='Tonga')%>%st_make_valid()%>%
  group_by(Territory1)%>%summarise(.groups = 'drop')
Tonga_eez%>%
  ggplot()+
  geom_sf(fill = NA)+
  coord_sf(crs = 4326)+theme_bw()

# ------ write base map related to file -----------------------------------
save(list = c('Tonga_eez','al_tonga_shape'),file = 'analysis/base_map_related.RData')
