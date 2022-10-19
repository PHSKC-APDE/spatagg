# Set up data to test with
library('sf')
library('data.table')
set.seed(1)
#create a test grid of bottom left points
grid_size = 10
grid = expand.grid(x = seq_len(grid_size)-1, y = seq_len(grid_size)-1)

poly_points = function(x, y, step =1){
  mat = matrix(c(x, y, # bottom left
                 x+step, y, # bottom right
                 x+step, y+step, # top right
                 x, y+step, # top left
                 x, y # close
  ),ncol = 2, byrow = TRUE)
}

# remove the outbounds of the grid as they'll get computed
source_poly = lapply(seq_len(nrow(grid)), function(i) {
  x = grid[i,'x']
  y = grid[i, 'y']

  r = data.frame(id = i)
  r$geom = sf::st_sfc(sf::st_polygon(list(poly_points(x,y,1))))
  
  sf::st_sf(r, sf_column_name = 'geom')

})
source_poly = do.call(rbind, source_poly)

# The target grid will overlap the source grid and kind of go beyond it.
tgrid = expand.grid(x = seq(.5,8.5,2),
                    y = seq(.5,8.5,2))
target_poly = lapply(seq_len(nrow(tgrid)), function(i) {
  x = tgrid[i,'x']
  y = tgrid[i, 'y']
  
  r = data.frame(id = i)
  r$geom = sf::st_sfc(sf::st_polygon(list(poly_points(x,y,2))))
  
  sf::st_sf(r, sf_column_name = 'geom')
  
})
target_poly = do.call(rbind, target_poly)

ogrid = expand.grid(x = seq(0,8,2), y = seq(0,8,2))
overlap_poly = lapply(seq_len(nrow(ogrid)), function(i) {
  x = ogrid[i,'x']
  y = ogrid[i, 'y']
  
  r = data.frame(id = i)
  r$geom = sf::st_sfc(sf::st_polygon(list(poly_points(x,y,2))))
  
  sf::st_sf(r, sf_column_name = 'geom')
  
})
overlap_poly = do.call(rbind, overlap_poly)

# create some pop grids
even_pp = st_centroid(source_poly)
even_pp$pop = 1

# fractional crosswalk between overlapping polygons
# There should be perfect alignment
t1 = create_xwalk(source_poly, target = overlap_poly, source_id = 'id', target_id = 'id')
expect_true(all(t1$s2t_fraction == 1)) #one target is made of 4 sources, all of which fully overlap
expect_equal(names(t1), c('source_id', 'target_id', 's2t_fraction', 'isect_amount', 'tcoverage_amount', 'target_amount'))

# pop overlap between overlapping polygons with equal weight
t2 = create_xwalk(source_poly, overlap_poly, 'id', 'id', method = 'point pop', point_pop = even_pp)
expect_equal(t1, t2)

# frac overlap off center
t3 = create_xwalk(source_poly, target = target_poly, source_id = 'id', target_id = 'id')
t3s = subset(t3, target_id == 1)
# id 1 in target poly overlaps with 9 source polys, the corners are 25% covered, the center is 100%, and the rest are half
# if(requireNamespace('ggplot2')){
#   library('ggplot2')
#   g = ggplot() + geom_sf(data = source_poly, fill = NA) + 
#     geom_sf(data = subset(target_poly, id == 1), fill = NA, color = 'red') +
#     geom_sf(data = even_pp)
#   print(g)
# }
expect_equal(t3s$s2t_fraction, c(.25,.5,.25,.5,1,.5,.25,.5,.25))
 
# pop overlap off center
# if(requireNamespace('ggplot2')){
#   g = ggplot() + geom_sf(data = source_poly, fill = NA) + 
#     geom_sf(data = target_poly, fill = NA, color = 'red') +
#     geom_sf(data = even_pp)
#   print(g)
# }
t4 = create_xwalk(source_poly, target_poly, 'id', 'id', method = 'point pop', point_pop = even_pp)
expect_equal(t1, t2)

# expect error when source and target don't overlap
expect_error(create_xwalk(source_poly[1,], target = target_poly[2,], source_id = 'id', target_id = 'id'), pattern = 'Spatial overlap')

# error when source and/or target don't overlap with point pop
expect_error(create_xwalk(source_poly, target_poly, 'id', 'id', method = 'point pop', point_pop = even_pp[40,]), 'point_pop bbox')
expect_error(create_xwalk(source_poly[12,], overlap_poly[1,], 'id', 'id', method = 'point pop', point_pop = even_pp[c(1:3,11:13, 21:23),]), 'point_pop bbox only overlaps with 56.25 % of target')

# check the percentage
a = bbox_to_sf(st_bbox(overlap_poly[1,]), 'op')
b = bbox_to_sf(st_bbox(even_pp[c(1:3,11:13, 21:23),]), 'pp')
a$oparea = as.numeric(st_area(a))
b$area = as.numeric(st_area(b))
isect = st_intersection(a,b)
isect$end = as.numeric(st_area(isect))

# % of the resulting intersection that covers the bbox of op
expect_equal(.5625,round(isect$end/isect$oparea,4))


# test for internally overlapping source polys
p1 = sf::st_sfc(sf::st_polygon(list(poly_points(0,0))))
p2 = sf::st_sfc(sf::st_polygon(list(poly_points(.1,.1,.5))))
p3 = sf::st_sfc(sf::st_polygon(list(poly_points(1,0))))
p4 = sf::st_sfc(sf::st_polygon(list(poly_points(2,0))))
p5 = sf::st_sfc(sf::st_polygon(list(poly_points(.9,.5,.2))))
iolap = data.frame(id = 1:5)
iolap$geom = c(p1, p2, p3, p4, p5)
iolap = sf::st_sf(iolap, sf_column_name = 'geom')
expect_error(check_internal_consistency(iolap))
expect_error(create_xwalk(iolap, overlap_poly[1,], 'id', 'id', method = 'point pop', point_pop = even_pp), 'overlapping bits')

# test a random distribution of points
pop_pts = st_sample(rbind(target_poly, source_poly), 200)
pop_pts = st_sf(data.frame(id = 1:200, pop = runif(200,0,10), geom = pop_pts))
t5 = create_xwalk(source_poly, target_poly, 'id', 'id', method = 'point pop', point_pop = pop_pts)
setDT(t5)

# target amount is correct
# use tid 23 for kicks
t5.1 = st_intersection(pop_pts, target_poly[23,])
expect_true(all(sum(t5.1$pop) == t5[target_id==23, target_amount]))

# for a given source that intersects with target, confirm the isect amount
t5.2 = st_intersection(target_poly[23,], source_poly[t5[target_id == 23, source_id],])
t5.2 = st_intersection(pop_pts, t5.2)
setDT(t5.2)
t5.2 = t5.2[, .(pop = sum(pop)), keyby = id.1]
setorder(t5, source_id)
expect_equal(t5[target_id == 23 , isect_amount], t5.2[,pop])

# confirm tcoverage_amount
expect_equal(t5[target_id == 23, unique(tcoverage_amount)], t5.2[, sum(pop)])

# confirm s2t_fraction
t5.3 = st_intersection(pop_pts, source_poly[t5[target_id == 23, source_id],])
setDT(t5.3)
t5.3 = t5.3[, .(sp_pop = sum(pop)), by = id.1]
t5.3 = merge(t5.3, t5.2, all.x = T, by = 'id.1')
expect_equal(t5[target_id == 23,s2t_fraction], t5.3[, pop/sp_pop])

# test out the actual crosswalk function
# full coverage
src = data.table(id = 1:3, est_count = 10, est_N = c(20,30,40))
src[, est_mean := est_count/est_N]
whole_cover = data.table(source_id = 1:3, target_id = 1, s2t_fraction = 1, isect_amount = c(20,30,40), tcoverage_amount = 90, target_amount = 90)
t6.1 = crosswalk(src, 'id', est = 'est_count', proportion = FALSE, xwalk_df = whole_cover)
t6.2 = crosswalk(src, 'id', est = 'est_mean', proportion = TRUE, xwalk_df = whole_cover)
expect_equal(data.frame(target_id = 1, est = t6.1$est/whole_cover$target_amount[1]),t6.2)
expect_equal(t6.1$est, 30)

# full coverage from partial overlap
partial_overlap = data.table(source_id = 1:3, target_id = 1, 
                           s2t_fraction = .75, isect_amount = c(20,30,40)*.75, 
                           tcoverage_amount = 90*.75, target_amount = 90*.75)
t7.1 = crosswalk(src, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_overlap)
t7.2 = crosswalk(src, 'id', est = 'est_mean', proportion = TRUE, xwalk_df = partial_overlap)
expect_equal(data.frame(target_id = 1, est = t7.1$est/partial_overlap$target_amount[1]),t7.2)
expect_equal(t7.1$est, 30 * .75)

# partial cover
partial_cover = data.table(source_id = 1:3, target_id = 1, 
                             s2t_fraction = 1, isect_amount = c(20,30,40), 
                             tcoverage_amount = 90, target_amount = 100) #when only part of target is covered
t8.1 = crosswalk(src, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, rescale = TRUE)
t8.2 = crosswalk(src, 'id', est = 'est_mean', proportion = TRUE, xwalk_df = partial_cover, rescale = TRUE)
t8.3 = crosswalk(src, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, rescale = FALSE)
t8.4 = crosswalk(src, 'id', est = 'est_mean', proportion = TRUE, xwalk_df = partial_cover, rescale = FALSE)

expect_equal(t8.1$est, 30 * 100/90)
expect_equal(t8.3$est, 30)

# the mean estimates * weights
# proportion estimates don't get scaled anymore
expect_equal(t8.2$est, sum(10/c(20,30,40) * c(20,30,40)/100))
expect_equal(t8.2, t8.4) # rescaling doesn't work when proportion = TRUE

# without scaling
t9.1 = crosswalk(src, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, rescale = FALSE)
t9.2 = crosswalk(src, 'id', est = 'est_mean', proportion = TRUE, xwalk_df = partial_cover, rescale = FALSE)
expect_equal(t9.1$est, 30)

# the mean estimates * weights
expect_equal(t9.2$est, sum(10/c(20,30,40) * c(20,30,40)/100))

# try with one by
src_by = rbind(src, src)
src_by[, bvar := c(1,1,1,2,2,2)]
t10 = crosswalk(src_by, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, by = 'bvar')
expect_equal(t10[1,'est'], t10[2,'est'])
expect_equal(t10[1,'est'], t8.1[1,'est'])

# id needs to be unique within a group
src_by = rbind(src_by, src_by)
expect_error(crosswalk(src_by, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, by = 'bvar'), 'duplicate values')

# try with two bys
src_by[, bvar2 := sample(1:2, nrow(src_by), replace = T)]
src_by = src_by[src_by[, .I[which.max(.I)], by = c('bvar', 'bvar2')]$V1,]
t11.1 = crosswalk(src_by, 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, by = c('bvar', 'bvar2'))
t11.2 = crosswalk(src_by[1], 'id', est = 'est_count', proportion = FALSE, xwalk_df = partial_cover, by = c('bvar', 'bvar2'))
row.names(t11.1) = NULL
row.names(t11.2) = NULL
expect_equal(subset(t11.1,bvar == t11.2[, 'bvar'] & bvar2 == t11.2[, 'bvar2'])$est, t11.2$est)

# make sure the 1000 draw thing works
# whole coverage
src_se = copy(src)[, se := .05]
src_se[, ct_se := 3]
t12.1 = crosswalk(src_se, 'id', est = 'est_mean', proportion = TRUE, se = 'se', xwalk_df = whole_cover)
t12.2 = crosswalk(src_se, 'id', est = 'est_mean', proportion = TRUE, xwalk_df = whole_cover)
t12.3 = crosswalk(src_se, 'id', est = 'est_N', proportion = FALSE, se = 'ct_se', xwalk_df = whole_cover)
t12.4 = crosswalk(src_se, 'id', est = 'est_N', proportion = FALSE, xwalk_df = whole_cover)
t12.chk = lapply(split(src_se, by = 'id'), function(x) rnorm(1000, x[, est_mean], x[, se]))
t12.chk = lapply(t12.chk, function(x) x/3) # equal weighting
t12.chk = t(as.matrix(data.frame(t12.chk)))
t12.chk = sd(colSums(t12.chk))

# the computed se should be similar to the numerical attempt
expect_equal(round(t12.chk,2), round(t12.1$se,2))

# should 12.2 and 12.3 be the same? probably not since I'd want the se to be consistent with the mean
# lets assume at least rounded they are close
expect_equal(round(t12.3$est), t12.4$est)
expect_equal(round(t12.1$est,2), round(t12.2$est,2))


