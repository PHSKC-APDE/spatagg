library('sf')
library('spatagg')
library('dplyr')
library('data.table')
hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/HRA_2010.shp") %>% st_make_valid()
tract = st_read("//dphcifs/APDE-CDIP/Shapefiles/Census_2020/tract/kc_tract.shp") %>%
  st_transform(st_crs(hra))

tract$pop = 1

# With different land/water boundaries
tract2hra = create_xwalk(tract, hra, 'GEOID', 'name', method = 'fractional overlap')
setDT(tract2hra)
res = crosswalk(tract, 'GEOID','pop', xwalk_df = tract2hra, rescale = T)

v1 = sum(res[,'est'])/sum(tract$pop)

# Sync the boundaries
tract  = st_intersection(tract, summarize(hra))
tract2hra = create_xwalk(tract, hra, 'GEOID', 'name', method = 'fractional overlap')
setDT(tract2hra)
res = crosswalk(tract, 'GEOID','pop', xwalk_df = tract2hra, rescale = T)
v2 = sum(res[,'est'])/sum(tract$pop)

#without scaling
res = crosswalk(tract, 'GEOID','pop', xwalk_df = tract2hra, rescale = F)
v3 = sum(res[,'est'])/sum(tract$pop)


print(v1)
print(v2)
print(v3)
