library('data.table')
library('dplyr')
library('spatagg')
library('openxlsx')
library('sf')

# Load estimates from Lin
a = read.xlsx("C:/Users/dcasey/local_documents/table1.xlsx")
setDT(a)

# Clean up some HRA naming differences
a[hracode == 'Fed Way-Dash Point/Woodmont', hracode := 'Fed Way-Dash Pt']
a[grep('Fairwood', hracode), hracode := 'Fairwood']

# Load HRA shapefile
hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/HRA_2010.shp")

# THE plibrary KCCD shapefile is a lil wonky. Start from a different one (blocks to KCCD)
kccd2 = st_read("C:/Users/dcasey/local_downloads/kccd/Districting_Plan_2021_Adopted_12082021.shp")
kccd2 = kccd2 %>% group_by(DM_PLAN) %>% summarize() %>% rename(kccd_id = DM_PLAN )
kccd2 = st_transform(kccd2, st_crs(hra))

# attach the estimates to the HRA shapefile
hra = merge(hra, a, all.x = T, by.x = 'name', by.y = 'hracode')

# Ensure standard CRS
pp = st_transform(kcparcelpop::parcel_pop, st_crs(hra))

# Find the population weighted conversion factors from HRA to KCCD
hra_to_kccd = create_xwalk(source = hra,
                           target = kccd2,
                           source_id = 'name',
                           target_id = 'kccd_id',
                           method = 'point pop',
                           point_pop = pp)

# Compute KCCD level estimates (from HRA source) based on estimated proportions
b1 = crosswalk(source = a, source_id = 'hracode', est = 'Yes', proportion = TRUE, xwalk_df = hra_to_kccd)

# Compute KCCD level estimates based on Counts. Here the Counts in each KCCD are estimated as well
a[, people := Yes * sample_n]
b2.1 = crosswalk(source = a, source_id = 'hracode', est = 'people', proportion = FALSE, xwalk_df = hra_to_kccd)
b2.2 = crosswalk(source = a, source_id = 'hracode', est = 'sample_n', proportion = FALSE, xwalk_df = hra_to_kccd)
setDT(b2.1); setDT(b2.2);
b2 = merge(b2.1[, .(kccd = target_id, YesN = est)], b2.2[, .(kccd = target_id, N = est)], all = T, by = 'kccd')
b2[, est_via_N := YesN/N]

# Take the two sets of estimates (proportion and counts) and compare them
# They should be pretty close. Usually within 1 percentage point
b = merge(b2, b1, all = T, by.x = 'kccd', by.y = 'target_id')
