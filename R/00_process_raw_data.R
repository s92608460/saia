# -----------   to be used to process  raw biological data ----------------
# ------------  load libraries --------------------------------------------
libs = c('tidyverse','lubridate');lapply(libs,library,character.only = TRUE)

# ------------------  load data to be used --------------------------------
raw_dat = read_csv('data/raw/bio/data.csv')
ren_fun = function(x){str_to_sentence(x)}

names_map = tibble(old = c("Alb_mt", "Alb_cpue", "Bet_mt", "Bet_cpue", "Skj_mt", 
                   "Skj_cpue", "Yft_mt", "Yft_cpue", "Tot_mt", "Tot_cpue"),
new =c("Albacore_mt", "Albacore_cpue", "Bigeye_mt", "Bigeye_cpue", "Skipjack_mt", 
"Skipjack_cpue", "Yellowfin_mt", "Yellowfin_cpue", "Total_mt", "Total_cpue"))
# ----------------  process  raw data -------------------------------------
proc_dat = raw_dat%>%
  dplyr::rename(lon = lond, lat = latd, time_of_day =time,year = yy, month = mm, no_hooks = `#_of_hks`)%>%
  dplyr::rename_all(.funs = ren_fun)%>%
  rename_at(names_map$old,~names_map$new)

# ---------------  write processed data to file ---------------------------
write_csv(x = proc_dat,path = 'data/tidy/bio/processed_bio_data.csv')


  
