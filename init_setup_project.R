################## Prepare and setup project ############################################
libs_ini <- c('purrr','here');lapply(libs_ini,library, character.only=T)

dir_list = c('R',here('data/raw/bio'),here('data/raw/env/sst'),here('data/raw/env/chl'),
             here('data/tidy/bio'),here('data/tidy/env/sst'),here('data/tidy/env/chl'),
             here('data/tidy/env/bathy'),
             'report',
             'figures','analysis', here('spatial/shape'), here('spatial/shape_processed'))

cust_dir_create <- function(x){ifelse(dir.exists(x),dir.exists(x),dir.create(x,recursive = TRUE))}

map(dir_list,cust_dir_create)
