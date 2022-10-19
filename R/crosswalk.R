#' Crosswalk estimates between non-nesting geographies
#' 
#' @param source data.frame containing estimates to transfer to \code{source}
#' @param source_id character. Column of ids that link source to xwalk_df
#' @param est character. Column in \code{source} containing the estimates for transfer
#' @param proportion logical. Does `est` represent proportion (0 - 1) data?
#' @param se character (optional). The standard error of the estimate in \code{source}
#' @param by character (optional). Vector of column names to compute by. A given id must be unique within the combination of by.
#' @param xwalk_df data.frame. Must have the following columns: source_id, target_id, s2t_fraction, isect_amount, tcoverage_amount, target_amount. Use create_xwalk to generate the crosswalk.
#' @param rescale logical. Should the crosswalk weights by scaled to encompass all of target?
#' 
#' @importFrom data.table data.table setnames setDF
#' @details
#' By computations happen all at once-- that is, if c('race','sex') is passed, the computations are by the intersection of race and sex, not as race and then sex.
#' 
#' When rescale is TRUE, and tcoverage_amount != target_amount for a given source -> target pair
#' the crosswalk weights will be scaled up by target_amount/tcoverage_amount
#' 
#' Crosswalk weights for `proportion = FALSE` are `s2t_fraction` optionally scaled via rescale.
#' For `proportion = TRUE`, the crosswalk weights are `isect_amount/target_amount`. The rescale option is irrelevant.
#' 
#' @export
#' 
crosswalk = function(source, source_id, est, proportion = FALSE, se = NULL, by = NULL, xwalk_df, rescale = TRUE){
  
  # convert source to data.table
  source = data.table::data.table(source)
  
  #confirm est, se, and by are all in source
  stopifnot('One of the columns implied by `est`, `se`, and/or `by` does not exist in source' =
              all(c(est, se, by) %in% names(source)))
  source = source[, .SD, .SDcols = c(source_id, by, est, se)]
  data.table::setnames(source, c(est, se), c('est', 'se')[c(T, !is.null(se))])
  
  # check for >0
  stopifnot('est must be >=0' = all(is.na(source[, est]) | source[, est>=0]))
  
  # validate source_id
  source[, .(res = validate_col(.SD,id = source_id, type = 'source')), by = by]
  
  # Legacy naming
  if(proportion){
    est_type = 'mean'
  }else{
    est_type = 'count'
  }
  
  # validate xwalk_df
  stopifnot('xwalk_df is missing at least one required column' =
              all(c("source_id", "target_id", "s2t_fraction", "isect_amount", "tcoverage_amount", 
                    "target_amount") %in% names(xwalk_df)))
  xwalk_df = data.table::data.table(xwalk_df)
  
  # Prep the weights
  if(est_type == 'count'){
    xwalk_df[, weight := s2t_fraction] # fraction of source that falls within target
    if(rescale) xwalk_df[, weight := weight * target_amount/tcoverage_amount, by = 'target_id']
  } else{
    xwalk_df[, weight := isect_amount/target_amount] # fraction of target made up by source
  }
  
  
  # validate est
  stopifnot('est is not numeric' = is.numeric(source[, est]))
  
  # if se is specified, then generate 1000 draws
  if(!missing(se) && !is.null(se)){
    source = source[, .(est = rnorm(1000, est, se), draw_id = paste0('est', seq_len(1000))), by = c(source_id, by)]
    cast_form = paste0(paste(c(source_id, by), collapse = ' + '), ' ~ ', 'draw_id')
    source = dcast(source, cast_form, value.var = 'est')
    estv = paste0('est', seq_len(1000))
  }else{
    estv = 'est'
  }
  
  # add on the xwalk_df
  source = merge(source, xwalk_df[, .(source_id, target_id, weight)], all.x = TRUE, by.x = source_id, by.y = 'source_id', allow.cartesian = TRUE)
  source = source[!is.na(target_id)]
  
  # compute results
  source = source[, lapply(.SD, function(x) sum(x * weight)), by = c('target_id', by), .SDcols = estv]
  data.table::setnames(source, c('target_id', by, estv))
  
  if(length(estv)>1){
    source = melt(source, id.vars = c('target_id', by))
    source = source[, .(est = mean(value), se = sd(value)),  by = c('target_id', by)]
  }
  
  data.table::setDF(source)
  
  return(source)
  
  
  
  
}