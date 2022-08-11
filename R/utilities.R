#' A function to validate a column exists in a dataset and is unique
#' @param d data.frame
#' @param id id column
#' @param type character. Name/descriptor of x
#' @export
validate_col = function(d, id, type = 'source'){
  if(id %in% names(d)){
    if(anyDuplicated(d[[id]])>0){
      stop(paste0('In ', type, ': ', id, ' contains duplicate values'))
    }
  }else{
    stop(paste0('In ', type, ': column named ', id, ' was not found'))
  }
  
  TRUE
}


#' check the internal consitency of an sf file
#' @param x a sf data.frame
#' @param stop logical. Should this function throw an error or return the results?
#' @export
#' @importFrom sf st_overlaps st_contains
#' 
check_internal_consistency = function(x, stop = TRUE){
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
    return(list(co, cc))
  }
  
  
  
}

#' Convert a bounding box to an sf data frame
#' @param bbox a result from st_bbox
#' @export
bbox_to_sf = function(bbox,id = 1){
  sf::st_sf(data.frame(id = id, geom = sf::st_as_sfc(bbox)))
} 

