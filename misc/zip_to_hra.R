library('sf')
library('spatagg')
library('dplyr')
library('kcparcelpop')
out = 'C:/Users/dcasey/local_documents/scratch'
hra = st_read("S:/WORK/REQUESTS/Maps/Shapefiles/HRA-HealthReportingAreas/HRA_2010Block_Clip.shp") %>% st_make_valid() %>% group_by(HRA2010v2_) %>% summarize()
zips = st_read("//phdata01/drof_data/DOH DATA/ADCi_WAStateZIPCode_Shapefile/J24007_King_County_Public_Health/adci_wa_zip_confidential/adci_wa_zip_confidential.shp")
zips = zips %>% st_transform(st_crs(hra)) %>% group_by(POSTCODE) %>% summarize() %>% st_make_valid()
zips = st_filter(zips, hra)

# reduce overlaps to make things work
z2hra_fo = create_xwalk(zips[, 'POSTCODE'], hra[, c('HRA2010v2_')], 'POSTCODE', 'HRA2010v2_',method = 'fractional overlap', min_overlap = .1)
z2hra_pp = create_xwalk(zips[, 'POSTCODE'], hra[, c('HRA2010v2_')], 'POSTCODE', 'HRA2010v2_',method = 'point pop', min_overlap = .1, point_pop = kcparcelpop::parcel_pop, pp_min_overlap = .2)

saveRDS(z2hra_fo, file.path(out, 'zip2hra_fo.rds'))
saveRDS(z2hra_pp, file.path(out, 'zip2hra_pp.rds'))
