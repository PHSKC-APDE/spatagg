#' A function to validate a column exists in a dataset and is unique
#' @param d data.frame
#' @param id id column
#' @param type character. Name/descriptor of x
#' @export
validate_col = function(d, id, type = 'source'){
  if(id %in% names(d)){
    if(anyDuplicated(d[[id]])>0){
      stop(paste0('In ', type, ': ', id, ' contains duplicate values (either in whole of input or a subgroup specified by `by`).
                  It should be unique within each group (even if the group is the whole dataset)'))
    }
  }else{
    stop(paste0('In ', type, ': column named ', id, ' was not found'))
  }
  
  TRUE
}


#' check the internal consitency of an sf file
#' @param x a sf data.frame
#' @param stop logical. Should this function throw an error or return the results?
#' @param return_probs logical. Should the function return a vector of logicals indicating the rows with problems instead of the query results?
#' @export
#' @importFrom sf st_overlaps st_contains
#'
check_internal_consistency = function(x, stop = TRUE, return_probs = FALSE){
  stopifnot(inherits(x,'sf'))
  
  # check overlaps (e.g. one geometry straddles the border of another)
  co = sf::st_overlaps(x)
  
  # check contains (one geometry is inside another)
  cc = sf::st_contains(x)
  
  #identify problems
  co_probs =  vapply(co, function(y) length(y)>0, TRUE)
  cc_probs = vapply(cc, function(y) length(y)>1, TRUE)
  
  if(stop){
    if(any(co_probs) || any(cc_probs)){
      stop(paste('Provided geometries contain overlapping bits. Indicies with a potential problem include:',
                 paste(which(co_probs|cc_probs), collapse = ', ')))
    }
    return(TRUE)
  }else{
    if(return_probs) return(co_probs|cc_probs)
    return(list(co, cc))
  }
  
  
  
}

#' Convert a bounding box to an sf data frame
#' @param bbox a result from st_bbox
#' @export
bbox_to_sf = function(bbox,id = 1){
  sf::st_sf(data.frame(id = id, geom = sf::st_as_sfc(bbox)))
} 

#' Try to eliminate internal/self overlaps
#' @param x sf object
#' @return An sf object that hopefully passes check_internal_consistency checks.
#' @details Users should visually review the resulting object to make sure the hueristics used here weren't too goofy
#' @importFrom sf st_make_valid st_buffer st_is_longlat st_intersection
#' @importFrom lwgeom st_snap_to_grid
#' @importFrom units set_units
#' @export 
reduce_overlaps = function(x){
  
  # # start by making sure there are problems to fix
  # ic = check_internal_consistency(x, stop = F, return_probs = T)
  # if(!any(ic)) return(x)
  
  # try making it valid
  x = sf::st_make_valid(sf::st_buffer(x,0))
  ic = check_internal_consistency(x, stop = F, return_probs = T)
  if(!any(ic)) return(x)
  
  x = st_intersection(x)
  # internal intersections
  x = subset(x, n.overlaps<=1)
  x$n.overlaps = NULL
  x$origins = NULL
  x = st_make_valid(x)
  x = st_buffer(x,0)
  ic = check_internal_consistency(x, stop = F, return_probs = T)
  if(!any(ic)) return(x)
  
  for(m in c(.00001, .0001, .001)){
    x2 = st_buffer(x, units::set_units(-m, 'm'))
    ic = check_internal_consistency(x2, stop = F, return_probs = T)
    if(!any(ic)) return(x2)
  }
  
  stop('Could not make `x` pass check_internal_consistency')

}

