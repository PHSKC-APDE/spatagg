# Set up data to test with
library('sf')

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
tinytest::expect_true(all(t1$s2t_fraction == 1)) #one target is made of 4 sources, all of which fully overlap
tinytest::expect_equal(names(t1), c('source_id', 'target_id', 's2t_fraction', 'isect_amount', 'tcoverage_amount', 'target_amount'))

# pop overlap between overlapping polygons with equal weight
t2 = create_xwalk(source_poly, overlap_poly, 'id', 'id', method = 'point pop', point_pop = even_pp)
tinytest::expect_equal(t1, t2)

# frac overlap off center
t3 = create_xwalk(source_poly, target = target_poly, source_id = 'id', target_id = 'id')
t3s = subset(t3, target_id == 1)
# id 1 in target poly overlaps with 9 source polys, the corners are 25% covered, the center is 100%, and the rest are half
if(requireNamespace('ggplot2')){
  library('ggplot2')
  g = ggplot() + geom_sf(data = source_poly, fill = NA) + 
    geom_sf(data = subset(target_poly, id == 1), fill = NA, color = 'red') +
    geom_sf(data = even_pp)
  print(g)
}
tinytest::expect_equal(t3s$s2t_fraction, c(.25,.5,.25,.5,1,.5,.25,.5,.25))
 
# pop overlap off center
if(requireNamespace(ggplot2)){
  g = ggplot() + geom_sf(data = source_poly, fill = NA) + 
    geom_sf(data = target_poly, fill = NA, color = 'red') +
    geom_sf(data = even_pp)
  print(g)
}
t4 = create_xwalk(source_poly, target_poly, 'id', 'id', method = 'point pop', point_pop = even_pp)
tinytest::expect_equal(t1, t2)

# expect error when source and target don't overlap
tinytest::expect_error(create_xwalk(source_poly[1,], target = target_poly[2,], source_id = 'id', target_id = 'id'), pattern = 'Spatial overlap')

# error when source and/or target don't overlap with point pop
tinytest::expect_error(create_xwalk(source_poly, target_poly, 'id', 'id', method = 'point pop', point_pop = even_pp[40,]), 'point_pop bbox')
tinytest::expect_error(create_xwalk(source_poly[12,], overlap_poly[1,], 'id', 'id', method = 'point pop', point_pop = even_pp[c(1:3,11:13, 21:23),]), 'point_pop bbox only overlaps with 56.25 % of target')

# check the percentage
a = bbox_to_sf(st_bbox(overlap_poly[1,]), 'op')
b = bbox_to_sf(st_bbox(even_pp[c(1:3,11:13, 21:23),]), 'pp')
a$oparea = as.numeric(st_area(a))
b$area = as.numeric(st_area(b))
isect = st_intersection(a,b)
isect$end = as.numeric(st_area(isect))

# % of the resulting intersection that covers the bbox of op
tinytest::expect_equal(.5625,round(isect$end/isect$oparea,4))


# test for internally overlapping source polys
p1 = sf::st_sfc(sf::st_polygon(list(poly_points(0,0))))
p2 = sf::st_sfc(sf::st_polygon(list(poly_points(.1,.1,.5))))
p3 = sf::st_sfc(sf::st_polygon(list(poly_points(1,0))))
p4 = sf::st_sfc(sf::st_polygon(list(poly_points(2,0))))
p5 = sf::st_sfc(sf::st_polygon(list(poly_points(.9,.5,.2))))
iolap = data.frame(id = 1:5)
iolap$geom = c(p1, p2, p3, p4, p5)
iolap = sf::st_sf(iolap, sf_column_name = 'geom')
tinytest::expect_error(check_internal_consistency(iolap))

















