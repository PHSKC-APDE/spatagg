# 
library('data.table')

a = data.table(source_id = 1:2, target_id = 1, s2t_fraction = 1, isect_amount = c(100,20), tcoverage_amount = 120, target_amount = 120, est = c(.1,.5), est2 = c(10,10))

# count approach
a[,sum(est2 * s2t_fraction)/sum(isect_amount)]

# mean approach
a[, sum(est * isect_amount/target_amount)]

