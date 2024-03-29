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
#' @importFrom sf st_geometry_type st_sf st_as_sfc st_bbox
#' 
create_xwalk = function(source, target, source_id = 'GEOID', target_id = 'GEOID',
                        method = 'fractional overlap', 
                        point_pop = if(requireNamespace('kcparcelpop')) kcparcelpop::parcel_pop else NULL,
                        min_overlap = .75, ...){
  # Argument checking
  stopifnot(inherits(source, 'sf'))
  stopifnot(inherits(target, 'sf'))
  method = match.arg(method, c('point pop', 'fractional overlap'))
  
  stopifnot(min_overlap>=0 & min_overlap <= 1)
  
  # make sure the inputs are sensible geographies
  check_internal_consistency(source)
  check_internal_consistency(target)
  
  # confirm compatible CRSs
  if(st_crs(target) != st_crs(source)){
    stop('`target` and `source` must have the same CRS')
  }
  
  # confirm source_id and target_id are unique identifiers
  validate_col(source, source_id, 'source')
  validate_col(target, target_id, 'target')
  
  # check to make sure the geographies overlap with each other
  sbb = sf::st_sf(data.frame(id = 'source', geom = sf::st_as_sfc(sf::st_bbox(source))))
  tbb = sf::st_sf(data.frame(id = 'target', geom = sf::st_as_sfc(sf::st_bbox(target))))
  olap = xwalk_folap(sbb, tbb)
  
  if(nrow(olap) ==0 || olap$s2t_fraction<min_overlap){
    stop(paste0('Spatial overlap between source and target is less than: ', min_overlap))
  }
  
  if(method == 'fractional overlap'){
    xwalk = xwalk_folap(source = source[, source_id], target = target[, target_id], source_id = source_id, target_id = target_id, ...)
  }else{
    
    # make sure point pop is points
    stopifnot(all(sf::st_geometry_type(point_pop) == 'POINT'))
    xwalk = xwalk_polap(source, target, source_id, target_id, point_pop = point_pop, ...)
  }
  rownames(xwalk) <- NULL
  
  return(xwalk)
  
}

#' Create an xwalk matrix based on fractional overlap
#' 
#' @param source sf data.frame. Sf object containing the source geographies
#' @param target sf data.frame. sf object containng the target geographies
#' @param source_id character. Column that has the unique ids in source
#' @param target_id character. Column that has the unique ids in target
#' @param ... options passed to \code{sf::st_intersection}
#' 
#' @importFrom sf st_intersection st_area st_geometry
#' @importFrom data.table setDF setDT setorderv
#' @return data.frame with columns from target, source, and a `fraction`
#' denoting the fraction of source that intersects with target spatially.
#' Depending on how the two geography sets overlap, it may be that source doesn't
#' fully cover target 
#' 
#' 
#' 
xwalk_folap = function(source, target, source_id = 'id', target_id = 'id', ...){
  
  # confirm source_id and target_id are unique identifiers
  validate_col(source, source_id, 'source')
  validate_col(target, target_id, 'target')
  names(source)[names(source) == source_id] <- 'source_id'
  names(target)[names(target) == target_id] <- 'target_id'
  
  target$target_amount = as.numeric(sf::st_area(target))
  source$start = as.numeric(sf::st_area(source))
  isect = withCallingHandlers(
    st_intersection(target,source, ...),
    warning = function(w){
      if(conditionMessage(w) == 'attribute variables are assumed to be spatially constant throughout all geometries'){
        invokeRestart('muffleWarning')
      }
      
    }
  )
  
  isect$end = as.numeric(sf::st_area(isect))
  
  sf::st_geometry(isect) = NULL
  isect$fraction = isect$end/isect$start
  data.table::setDT(isect)
  isect[, coverage_amount := sum(end), by = target_id]
  
  # removing 0 fraction overlaps-- which should just be shared edges.
  isect = isect[fraction>0, .(source_id, target_id, s2t_fraction = fraction, isect_amount = end, tcoverage_amount = coverage_amount, target_amount)]
  data.table::setorderv(isect, c('source_id', 'target_id'))
  data.table::setDF(isect)
  return(isect)
  
}

#' Create an xwalk matrix based on population overlap
#' 
#' @param source sf data.frame. Sf object containing the source geographies
#' @param target sf data.frame. sf object containng the target geographies
#' @param source_id character. Column that has the unique ids in source
#' @param target_id character. Column that has the unique ids in target
#' @param point_pop sf data.frame with a "pop" column representing the number of people living at/around that point
#' @param pp_min_overlap numeric between 0 and 1. Percent of the bounding boxes of source and target that need to overlap with the bbox from point_pp. Otherwise, throw an error
#' @importFrom sf st_join st_crs st_bbox st_as_sfc
#' @import data.table
#' @return data.frame with columns from target, source, and a `fraction`
#' denoting the fraction of the source population that lives in source.
#' Depending on how the two geography sets overlap, it may be that source doesn't
#' fully cover target
#' 
xwalk_polap = function(source, target, source_id = 'id', target_id = 'id', point_pop, pp_min_overlap = .75, ...){
  
  stopifnot(pp_min_overlap>=0 & pp_min_overlap <= 1)
  stopifnot('`point_pop` does not have the same CRS as source' = sf::st_crs(point_pop) == st_crs(source))
  stopifnot('`point_pop` does not have the same CRS as target' = sf::st_crs(point_pop) == st_crs(target))
  stopifnot('`point_pop` does not contain a column called "pop"' = 'pop' %in% names(point_pop))
  
  # confirm source_id and target_id are unique identifiers
  validate_col(source, source_id, 'source')
  validate_col(target, target_id, 'target')

  # check bounding box overlaps between the three geography sets to make sure they are a decent fit
  # sbb to tbb is checked via create_xwalk and if it doesn't overlap it'll be caught via tbb probably
  pbb = st_sf(data.frame(id = 'point pop', geom = sf::st_as_sfc(sf::st_bbox(point_pop))))
  sbb = st_sf(data.frame(id = 'source', geom = sf::st_as_sfc(sf::st_bbox(source))))
  tbb = st_sf(data.frame(id = 'target', geom = sf::st_as_sfc(sf::st_bbox(target))))
  
  s_to_p = xwalk_folap(sbb, pbb)
  if(nrow(s_to_p) ==0 || s_to_p$s2t_fraction < pp_min_overlap){
    stop(paste('point_pop bbox only overlaps with', if(nrow(s_to_p) == 0) 0 else round(s_to_p$s2t_fraction*100,2),
               '% of source-- which is less than minimum requirement specified by pp_min_overlap:', pp_min_overlap*100,'%'))
  }
  t_to_p = xwalk_folap(tbb, pbb)
  if(nrow(t_to_p) == 0 || t_to_p$s2t_fraction < pp_min_overlap){
    stop(paste('point_pop bbox only overlaps with', if(nrow(t_to_p) == 0) 0 else round(t_to_p$s2t_fraction*100,2),
               '% of target-- which is less than minimum requirement specified by pp_min_overlap:', pp_min_overlap*100,'%'))
  }
  
  # compute the amount of target population that is shared by source
  point_pop = point_pop[, 'pop']
  point_pop$ppid = seq_len(nrow(point_pop))
  # One point might belong to two target polygons if they overlap (e.g. at the vertex)
  point_pop = sf::st_join(point_pop, source[, source_id])
  names(point_pop)[names(point_pop) == source_id] = 'source_id'
  point_pop = sf::st_join(point_pop, target[, target_id])
  names(point_pop)[names(point_pop) == target_id] = 'target_id'
  sf::st_geometry(point_pop) = NULL
  point_pop = data.table::data.table(point_pop)
  
  # compute fraction of population in source that also belongs to target
  # ipop
  ipop = point_pop[!is.na(target_id) & !is.na(source_id), .(ipop = sum(pop, na.rm = TRUE)), .(source_id, target_id)]
  
  # compute the amount of population in source
  spop = unique(point_pop[!is.na(source_id), .(source_id,ppid,pop)])[, .(spop = sum(pop,na.rm =TRUE)), source_id]
  # and then calculate how much of that belongs to each target
  sfrac = merge(ipop, spop, all.x = T, by = 'source_id')
  # s2t_fraction
  sfrac[, s2t_fraction := ipop/spop]
  
  # compute the amount of population in target
  # target_amount
  tpop = unique(point_pop[!is.na(target_id), .(target_id, ppid, pop)])[, .(tpop = sum(pop, na.rm =TRUE)), target_id]
  
  # compute the amount of population in target that is also in source
  # tcoverage_amount
  tcov = unique(point_pop[!is.na(target_id) & !is.na(source_id), .(target_id, ppid, pop)])[, .(tcov = sum(pop, na.rm = TRUE)), target_id]
  
  # compile things
  res = merge(sfrac, tpop, all.x = T, by = 'target_id')
  res = merge(res, tcov, all.x = T, by = 'target_id')
  res = res[,.(source_id, target_id, s2t_fraction, isect_amount = ipop,
               tcoverage_amount = tcov, target_amount = tpop)]

  stopifnot(all(res[, .N, .(source_id, target_id)][, N == 1]))
  data.table::setorderv(res, c('source_id', 'target_id'))
  
  res = res[isect_amount >0]
  
  data.table::setDF(res)
  
  
  return(res)
  
}


