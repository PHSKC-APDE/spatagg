#' Create an xwalk weight matrix to crosswalk estimates between non-nesting geographies
#' 
#' @param source sf data.frame. Sf object containing the source geographies
#' @param target sf data.frame. sf object containng the target geographies
#' @param method character. Method for generating the crosswalk estimate. One of "point pop" and "fractional overlap" 
#' @param source_id character. Column that has the unique ids in source
#' @param target_id character. Column that has the unique ids in target
#' @param point_pop sf data.frame with a "pop" column representing the number of people living at/around that point
#' @param min_overlap numeric between 0 and 1. Percent of the bounding boxes of source and target that need to overlap. Otherwise, throw an error
#' @param ... additional arguments passed to underlying methods. xwalk_polap or xwalk_folap
#' @export
#' 
#' 
create_xwalk = function(source, target, source_id = 'GEOID', target_id = 'GEOID',
                        method = 'fractional overlap', 
                        point_pop = if(requireNamespace('kcparcelpop')) kcparcelpop::parcel_pop else NULL,
                        min_overlap = .75, ...){
  # Argument checking
  stopifnot(inherits(source, 'sf'))
  stopifnot(inhereits(target, 'sf'))
  method = match.arg(method, c('point pop', 'fractional overlap'))
  
  stopifnot(min_overlap>=0 & min_overlap <= 1)
  
  # confirm compatible CRSs
  if(st_crs(target) != st_crs(source)){
    stop('`target` and `source` must have the same CRS')
  }
  
  # confirm source_id and target_id are unique identifiers
  validate_col(source, source_id, 'source')
  validate_col(target, target_id, 'target')
  
  # check to make sure the geographies overlap with each other
  sbb = st_sf(data.frame(id = 'source', geom = st_as_sfc(st_bbox(source))))
  tbb = st_sf(data.frame(id = 'target', geom = st_as_sfc(st_bbox(target))))
  olap = xwalk_folap(sbb, tbb)
  
  if(olap$fraction<min_overlap){
    stop(paste0('Spatial overlap between source and target is less than: ', min_overlap))
  }
  
  if(method == 'fractional overlap'){
    xwalk = xwalk_folap(source = source[, source_id], target = target[, target_id], ...)
    names(xwalk) = c('target_id', 'source_id', 'fraction')
  }else{
    xwalk = xwalk_polap(source, target, source_id, target_id, threshold = threshold, point_pop,)
  }
  
  
  return(xwalk)
  
}

#' Create an xwalk matrix based on fractional overlap
#' 
#' @param source sf data.frame. Sf object containing the source geographies
#' @param target sf data.frame. sf object containng the target geographies
#' @param threshold proportion between 0 and 1. THe minimum amount of overlap to consider as legit
#' @param ... options passed to \code{sf::st_intersection}
#' 
#' @importFrom sf st_intersection st_area st_geometry
#' 
#' @return data.frame with columns from target, source, and a `fraction`
#' denoting the fraction of source that intersects with target spatially.
#' Depending on how the two geography sets overlap, it may be that source doesn't
#' fully cover target (e.g. sum(fraction) != 1)-- especially in combination with
#' the threshold argument.
#' 
xwalk_folap = function(source, target, threshold = 0, ...){
  
  stopifnot('Threshold is out of [0 - 1] bounds' = threshold >=0 & threshold <= 1)
  
  target$start = as.numeric(sf::st_area(target))
  isect = st_intersection(target, source, ...)
  isect$end = as.numeric(sf::st_area(isect))
  
  sf::st_geometry(isect) = NULL
  isect$fraction = isect$end/isect$start
  isect = subset(isect, fraction>threshold)
  
  return(isect)
  
}

#' Create an xwalk matrix based on population overlap
#' 
#' @param source sf data.frame. Sf object containing the source geographies
#' @param target sf data.frame. sf object containng the target geographies
#' @param source_id character. Column that has the unique ids in source
#' @param target_id character. Column that has the unique ids in target
#' @param threshold proportion between 0 and 1. The minimum amount of overlap to consider as legit
#' @param point_pop sf data.frame with a "pop" column representing the number of people living at/around that point
#' @param min_overlap numeric between 0 and 1. Percent of the bounding boxes of source and target that need to overlap with the bbox from point_pp. Otherwise, throw an error
#' @importFrom sf st_join st_crs st_bbox st_as_sfc
#' @import data.table
#' @return data.frame with columns from target, source, and a `fraction`
#' denoting the fraction of the source population that lives in source.
#' Depending on how the two geography sets overlap, it may be that source doesn't
#' fully cover target (e.g. sum(fraction) != 1)-- especially in combination with
#' the threshold argument.
#' 
xwalk_polap = function(source, target, source_id = 'GEOID', target_id = 'GEOID', threshold = 0, point_pop, pp_min_overlap = .75, ...){
  
  stopifnot('Threshold is out of [0 - 1] bounds' = threshold >=0 & threshold <= 1)
  stopifnot(pp_min_overlap>=0 & pp_min_overlap <= 1)
  stopifnot('`point_pop` does not have the same CRS as source' = sf::st_crs(point_pop == st_crs(source)))
  stopifnot('`point_pop` does not have the same CRS as target' = sf::st_crs(point_pop == st_crs(target)))
  stopifnot('`point_pop` does not contain a column called "pop"' = 'pop' %in% names(point_pop))
  
  # confirm source_id and target_id are unique identifiers
  validate_col(source, source_id, 'source')
  validate_col(target, target_id, 'target')

  # check bounding box overlaps between the three geography sets to make sure they are a decent fit
  # sbb to tbb is checked via create_xwalk and if it doesn't overlap it'll be caught via tbb probably
  pbb = st_sf(data.frame(id = 'point pop', geom = sf::st_as_sfc(sf::st_bbox(point_pop))))
  sbb = st_sf(data.frame(id = 'source', geom = sf::st_as_sfc(sf::st_bbox(source))))
  tbb = st_sf(data.frame(id = 'target', geom = sf::st_as_sfc(sf::st_bbox(target))))
  
  s_to_p = xwalk_folap(pbb, sbb)
  if(s_to_p$fraction < pp_min_overlap){
    stop(paste('point_pop bbox only overlaps with', round(s_to_p$fraction,2),
               'of source-- which is less than minimum requirement specified by pp_min_overlap:', pp_min_overlap))
  }
  t_to_p = xwalk_folap(pbb, tbb)
  if(t_to_p$fraction < pp_min_overlap){
    stop(paste('point_pop bbox only overlaps with', round(t_to_p$fraction,2),
               'of target-- which is less than minimum requirement specified by pp_min_overlap:', pp_min_overlap))
  }
  
  # compute the amount of target population that is shared by source
  point_pop = point_pop[, 'pop']
  point_pop = sf::st_join(point_pop, source[, source_id])
  point_pop = sf::st_join(point_pop, target[, target_id])
  names(point_pop)[1:3] <- c('pop', 'source_id', 'target_id')
  sf::st_geometry(point_pop) = NULL
  point_pop = data.table(point_pop)
  point_pop = point_pop[, .(pop = sum(pop)), .(source_id, target_id)]
  
  # Percent of target's population that overlaps with source
  point_pop[, fraction := pop/sum(pop), target_id]
  
  data.table::setDF(point_pop[fraction>threshold])
  
  
  return(point_pop[, c('source_id', 'target_id', 'fraction')])
  
}


