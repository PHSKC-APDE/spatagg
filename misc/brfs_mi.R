library('survey')
library('data.table')
library('spatagg')
library('mitools')
library('dtsurvey')
library('rads')
library('ggplot2')

options(survey.lonely.psu="adjust")
brfs = readRDS("//dphcifs/APDE-CDIP/BRFSS/prog_all/2022/kc2022.rds")
set.seed(1)

# create some different variations for MI
alts = lapply(1:10, function(i){
  
  z2h = assign_cases(brfs, 'zipcode', rads.data::spatial_zip_to_hra20_pop)
  r = copy(brfs)
  r[, hra_code := z2h]
  setDF(r[, .(seqno, x_ststr, finalwt1, zipcode, hra_code, diab2)])
  # convert to survey
  # survey::svydesign(ids = ~seqno, strata = ~x_ststr, weights = ~finalwt1, data = r)
  
})

# Spatagg direct from ZIP to HRA
## Create survey object
brfs_sur = dtsurvey(brfs, 'seqno', 'x_ststr', 'finalwt1')

## compute at ZIP level
ddiab = rads::calc(brfs_sur, what = 'diab2', by = 'zipcode')
ddiab = ddiab[!is.na(mean) & !is.na(mean_se)]

## zip to hra via proportion
## using the proportion option since I don't think converting the numerator/denominator 
## separately captures the error as well.
## TODO: make sure the above logic is correct
ddiab_hra_prop = crosswalk(ddiab,'zipcode', est = 'mean', se = 'mean_se',
                      proportion = T,rescale = F, xwalk_df = rads.data::spatial_zip_to_hra20_pop)
setDT(ddiab_hra_prop)

## zip to hra via counts
## convert to counts
ddiab[, cnt := denominator * mean]
ddiab[, cnt_se := denominator * mean_se]
ddiab_hra_cnt = crosswalk(ddiab,'zipcode', est = 'cnt', se = 'cnt_se',
                           proportion = F,rescale = F, xwalk_df = rads.data::spatial_zip_to_hra20_pop)
setDT(ddiab_hra_cnt)
## convert back to props
ddiab_hra_cnt_denom = crosswalk(ddiab,'zipcode', est = 'denominator',
                                proportion = F,rescale = F, xwalk_df = rads.data::spatial_zip_to_hra20_pop)

ddiab_hra_cnt = merge(ddiab_hra_cnt, ddiab_hra_cnt_denom, all.x = T, by = 'target_id')
ddiab_hra_cnt[, mean := est.x/est.y][, mean_se := se/est.y]
# survey MI
mi_svy = svydesign(id = ~seqno, strata = ~x_ststr, weights = ~ finalwt1, data = mitools::imputationList(alts), nest = T)
ddiab_mi_svy = with(mi_svy, svyby(~diab2,~hra_code,FUN = svymean, na.rm = T))
ddiab_mi_svy_comp = mitools::MIcombine(ddiab_mi_svy)
ddiab_mi_svy_comp = data.frame(ddiab_mi_svy_comp)

## Single run of assign cases
bsur_gz = brfs_sur[zipcode %in% rads.data::spatial_zip_city_region_scc$zip]
single = rads::calc(bsur_gz, what = 'diab2', by = 'hra20_id')
single = single[!is.na(hra20_id)]

# combine
combo = rbind(
  ddiab_hra_cnt[, .(hra20_id = target_id, mean, se = mean_se, version = 'Z2H Count')],
  ddiab_hra_prop[, .(hra20_id = target_id, mean = est, se, version = 'Z2H Prop')],
  data.table(hra20_id = 1:61, mean = ddiab_mi_svy_comp$coefficients, 
             se = sqrt(diag(ddiab_mi_svy_comp$variance)), version = 'SVY MI'),
  single[, .(hra20_id, mean, se = mean_se, version = 'Assign Cases')]
  
  )

# clean up
combo[, version := factor(version, c('Z2H Prop', 'Z2H Count', 'Assign Cases', 'SVY MI'))]
combo[, upper := 1.96 * se + mean]
combo[, lower := mean - 1.96 * se]

h2r = rads.data::spatial_hra20_to_region20

combo = merge(combo, h2r, all.x = T, by = 'hra20_id')

g = ggplot(combo, aes(y = version, x = mean, color = version)) +
  geom_errorbarh(aes(xmin = lower, xmax = upper), size = 1.2) +
  geom_point() + 
  scale_color_brewer(type = 'qual', palette = 2) +
  # facet_wrap(~region_name, ncol = 1, scales = 'free_x') +
  facet_wrap(~hra20_id) +
  theme_bw() + 
  xlab('Proportion') + ylab('') +
  ggtitle('% of HRA with diabetes (diab2)', 'Varying approaches to convert from ZIP to HRA') 

g
