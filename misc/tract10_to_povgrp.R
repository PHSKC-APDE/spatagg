library('sf')
library('dplyr')
library('rads')
library('spatagg')

# load 2010 tract shapes
t10 = st_read('//dphcifs/apde-cdip/Shapefiles/Census_2010/tract/kc_tract.shp')
t10 = t10 %>% filter(GEOID10 != '53033990100') #watyer only shape

# load 2020 tracts
t20 = st_read('//dphcifs/apde-cdip/Shapefiles/Census_2020/tract/kc_tract.shp')

# Load poverty groupings
pg = rads.data::misc_poverty_groups
pg = pg %>% filter(geo_type == 'Tract')

# create geographies for tract based poverty groups
t20 = merge(t20, pg, all.x = T, by.x = 'GEOID', by.y = 'geo_id')
pvgrp = t20 %>% group_by(pov200grp) %>% summarize()

# create xwalk
xw = create_xwalk(t10, pvgrp, 'GEOID10', 'pov200grp',method = 'point pop')
