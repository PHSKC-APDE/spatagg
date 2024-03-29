#' Probabilistically assign rows in a dataset to a "target" geography
#' 
#' @param source data.frame containing rows to transfer to \code{target}
#' @param source_id character. Column of ids that link source to xwalk_df
#' @param xwalk_df data.frame. Must have the following columns: source_id, 
#' target_id, s2t_fraction. Use create_xwalk to generate the crosswalk.
#' @importFrom data.table data.table 
#' @details
#' 
#' This function uses the s2t_fraction to probabilistically assign rows to a target id
#' 
#' set the seed \code{set.seed} to make things deterministically random.
#' 
#' @return a vector of values representing the `target_id` to assign the row
#' 
#' 
#' @export
#' 
assign_cases = function(source, source_id, xwalk_df){

  stopifnot(source_id %in% names(source))
  ids = data.table::data.table(sid = source[[source_id]])
  
  xw = data.table::data.table(xwalk_df[, c('source_id', 'target_id', 's2t_fraction')])
  
  # check to see if anything needs to be rescaled
  schk = xw[, sum(s2t_fraction), source_id]
  schkl = schk[, all.equal(V1, rep(1, .N))]
  if(!isTRUE(schkl)){
    warning('Certain source_ids do not fully overlap with targets. The s2t_fractions will be scaled such that the sum of the fractions equals 1 per source_id')
    xw[, s2t_fraction := s2t_fraction/sum(s2t_fraction), source_id]
  }
  
  # create simple function to sample target probabilistically 
  ssample = function(x, size, replace = T, prob = NULL){
    if(length(unique(x)) == 1 && !is.null(prob) && all(prob == 1)){
      return(x)
    }else{
      return(sample(x,size,replace, prob))
    }
  }

  
  # select a single target per common source id
  for(joint_id in intersect(ids$sid, xw$source_id)){
    ids[sid == joint_id, target_id := ssample(x = xw[source_id == joint_id][['target_id']],
                                              size = nrow(ids[sid == joint_id]),
                                              replace = T,
                                              prob = xw[source_id == joint_id][['s2t_fraction']])]
  }
  
  return(ids[, target_id])
  
  
}