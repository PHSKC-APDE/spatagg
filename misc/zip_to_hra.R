library('sf')
library('spatagg')
library('dplyr')
library('kcparcelpop')
out = 'C:/Users/dcasey/local_documents/scratch'
hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/HRA_2010.shp") %>% st_make_valid() %>% group_by(name) %>% summarize()
zips = st_read("//phdata01/drof_data/DOH DATA/ADCi_WAStateZIPCode_Shapefile/J24007_King_County_Public_Health/adci_wa_zip_confidential/adci_wa_zip_confidential.shp")
zips = zips %>% st_transform(st_crs(hra)) %>% group_by(POSTCODE) %>% summarize() %>% st_make_valid()
zips = st_filter(zips, hra)

# reduce overlaps to make things work
z2hra_fo = create_xwalk(zips[, 'POSTCODE'], hra[, c('name')], 'POSTCODE', 'name',method = 'fractional overlap', min_overlap = .1)
z2hra_pp = create_xwalk(zips[, 'POSTCODE'], hra[, c('name')], 'POSTCODE', 'name',method = 'point pop', min_overlap = .1, point_pop = kcparcelpop::parcel_pop, pp_min_overlap = .2)

saveRDS(z2hra_fo, file.path(out, 'zip2hra_fo.rds'))
saveRDS(z2hra_pp, file.path(out, 'zip2hra_pp.rds'))

# New HRAs
hra = st_read("C:/Users/dcasey/King County/DPH-APDEData - HRA/newhras_4_clean.gpkg")
hra = st_intersection(hra, summarize(zips)) # standardize land/not land

# reduce overlaps to make things work
pp = st_transform(kcparcelpop::parcel_pop, st_crs(hra))
z2hra_fo = create_xwalk(zips[, 'POSTCODE'], hra[, c('new')], 'POSTCODE', 'new',method = 'fractional overlap', min_overlap = .1)
z2hra_pp = create_xwalk(zips[, 'POSTCODE'], hra[, c('new')], 'POSTCODE', 'new',method = 'point pop', min_overlap = .1, point_pop = pp, pp_min_overlap = .2)

saveRDS(z2hra_fo, file.path(out, 'zip2hra_fo_new.rds'))
saveRDS(z2hra_pp, file.path(out, 'zip2hra_pp_new.rds'))




library('mapview')
m1 = hra %>% filter(new == 'Issaquah')
mapview(zips) + mapview(m1)
setDT(z2hra_pp)
blah2 = merge(z2hra_pp[target_id == 'Issaquah'], blah, all.x = T, by.x = 'source_id', by.y = 'geo_id')
