#' A function to validate a column exists in a dataset and is unique
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