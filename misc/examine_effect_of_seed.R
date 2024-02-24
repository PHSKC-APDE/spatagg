library('spatagg')
library('sf')
library('rads.data')
library('rads')
library('data.table')
library('rads')
library('ggplot2')

N_samples = 2000
prob_of_sick = .3


zip = st_read("//dphcifs/APDE-CDIP/Shapefiles/ZIP/zipcode.shp")
hra = st_read("//dphcifs/APDE-CDIP/Shapefiles/HRA/hra_2020.shp")
zip = st_transform(zip, st_crs(hra))
z2h = rads.data::spatial_zip_to_hra20_pop
hrapop = get_population(T, 2022, geo_type = 'hra')
hra = merge(hra, hrapop, all.x = T, by.x ='id', by.y = 'geo_id_code')

# Generate several thousand points
peeps = st_sample(hra, round(N_samples * hra$pop/sum(hra$pop)))
peeps = st_as_sf(peeps)
peeps$pid = seq_len(nrow(peeps))
# Assign condition or not
peeps$sick = sample(c(0,1), nrow(peeps), T, prob = c(1-prob_of_sick,prob_of_sick))

# assign_zip
peeps = st_join(peeps, zip[, 'ZIPCODE'])

# assign true HRA
peeps = st_join(peeps, hra[, 'id'])

pdt = data.table(peeps[, c('ZIPCODE', 'sick', 'pid', 'id'), drop = T])
setnames(pdt, 'id', 'hra')
pdt[, iter := 0]

# create other versions of HRA assignments
# by rerunning assign cases with different seeds
iters = lapply(1:100, function(i){
  set.seed(i)
  
  r = spatagg::assign_cases(pdt, 'ZIPCODE', z2h)
  alt = copy(pdt[, .(ZIPCODE, sick, pid)])
  alt[, hra := r]
  alt[, iter := i]
  alt
  
  
})

iters = rbindlist(iters)

# For each iter - hra, find the number of "sick" people
res = iters[!is.na(hra), .(frac = sum(sick)/.N, denom = .N), .(iter, hra)]

# true fraction of sick
sicktrue = pdt[, .(frac = sum(sick)/.N), by = hra]
setorder(sicktrue, -frac)

# order things for the graph
sicktrue[, hraf := factor(hra, hra)]
res[, hraf := factor(hra, sicktrue$hra)]

# compute percentage sick, with staged aggregation
rzip = pdt[, .(sick = sum(sick), N = .N), .(ZIPCODE= as.integer(ZIPCODE))]
rzip_num = spatagg::crosswalk(rzip, 'ZIPCODE', est = 'sick', xwalk_df = z2h, rescale = F)
rzip_denom = spatagg::crosswalk(rzip, 'ZIPCODE', est = 'N', xwalk_df = z2h, rescale = F)
setDT(rzip_num); setDT(rzip_denom);
rzip_r = merge(rzip_num[, .(hra = target_id, numer = est)], 
               rzip_denom[, .(hra = target_id, denom = est)], all = T, by = 'hra')
rzip_r[, frac := numer/denom]
rzip_r[, hraf := factor(hra, sicktrue$hra)]

# Purple is truth, navy is from microdata -> zip -> HRA
# Comparing the navy points to the boxplot captures the seed effect
ggplot(res, aes(y = frac, x = hraf, group = hraf)) + geom_boxplot() +
  geom_point(data = sicktrue, color = 'yellow') +
  geom_point(data = rzip_r, color = 'navy') +
  ggtitle('Permutation boxplot', 'fraction sick') +
  xlab('HRA') + ylab('Proportion')

# See how well it recovers the true distribution of points by HRA
resh = copy(res)[, perc := denom/sum(denom), .(iter)]
reshtrue = pdt[, .(N_hra = .N), hra]
reshtrue[, perc := N_hra/sum(N_hra)]
setorder(reshtrue, -perc)
reshtrue[, hraf := factor(hra, hra)]
resh[, hraf := factor(hra, reshtrue$hraf)]
ggplot(resh, aes(y = perc, x = hraf, group = hraf)) + geom_boxplot() +
  geom_point(data = pdt[, .(perc = .N/nrow(pdt)), by = .(hraf = factor(hra, ressum$hra))], color = 'purple') +
  ggtitle('Permutation boxplot', 'location of samples') +
  xlab('HRA') + ylab('Proportion')

# percent of points within a ZIP going to HRAs
zh = iters[, .N, .(ZIPCODE, hra, iter)]
zh[, z2hp := N/sum(N), by = .(iter, ZIPCODE)]
zhavg  = zh[, mean(z2hp), .(ZIPCODE = as.numeric(ZIPCODE), hra)]
z2htrue = z2h[, .(ZIPCODE = ZIP, hra = hra20_id, z2hp = s2t_fraction)]
zhcomp = merge(z2htrue, zhavg, all.x = T, by = c('ZIPCODE', 'hra'))
ggplot(zhcomp, aes(x = z2hp, V1)) + geom_point() + geom_abline(slope = 1)
