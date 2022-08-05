## geo crosswalk funs

#' Get valid geographies
#'
#' Calling this function, with no arguments, returns a vector of the valid string values for geographies used in \code{aggregate_crosswalk()}.
#' The geographies named by this function are the available to and from geographies for re-allocating aggregate population counts between geographies.
#' @returnA string vector of providing the set of valid geographies
#' @export
get_geos <- function(){c("city", "zip5", "tribal_area", "csa", "csa_subarea", "school_district", "county_council_district", "tract", "block", "region")}


#' Show valid geography values
#'
#' Calling this function, with the name of the geography you want to see values for, will return a vector of the distinct
#' values of that geography that will be recognized by the aggregate crosswalk function.  Values in your data that do not
#' exactly match (case sensitive) one of these values will be collapsed into the category "Invalid 'from_geo' values".
#'
#' @param geography The name of the geography for which you want to review valid values, as a string.  It must but one of the
#' geographies named in `get_geos`.
#' @return A vector of all unique valid values for this geography. The vector will be either numeric or character, depending
#' on which geography is requested.
#' @export
show_valid_geo <- function(geography) {
  if(!geography %in% get_geos()) stop(paste0('`geography` argument must be one of: c("', paste(get_geos(), collapse = '", "'), '")'))
  geo_cols <- geo_col_map()
  parcelgeo <- geo_cols$cols[geo_cols$geographies==geography]
  parcelgeo <- ensym(parcelgeo)
  valid_geos <- parceldat %>%
    select(!!parcelgeo) %>%
    distinct() %>%
    na.omit() %>%
    .[,1]
  valid_geos
}

#' Create dataframe mapping human friendly column names to parceldat column names
#' (used in aggregage_crosswalk)
geo_col_map <- function() {
  data.frame(geographies = get_geos(),
             cols = c("DistrictNa", "ZIP5", "Tribe_Name", "CSA_NAME", "CSA_SUBAREA_NAME",
                      "SCHDST_NAME", "kccdst", "TRACT_STR", "BLOCK_STR",  "Region"),
             stringsAsFactors = FALSE)
}

#' Function to clean zip codes: Replace string and missing zips with generic invalid '-9' zip code to preserve crosswalk 'unknown' linkage
#' @param col The only argument to this function is a vector of zip code values to ensure numeric validity
#' @return a numeric vector that replaces non-numeric/missing values with -9
#' @export
clean_numeric <- function(col) {
  col <- as.numeric(col)
  col <- replace_na(col, -9)
  col
}

#' Allocate census population between geographies
#'
#' This is a largely internal function (used in \code{aggregate_crosswalk()}) to re-allocate population counts and percents between the
#' different geographies documented in \code{parceldat}.  It works by identifying the common parcels between geographies and allocating
#' aggregate population proportions based on estimated parcel population. This function only re-allocates the census population counts
#' provided at the parcel level by the King County GIS center; it will not re-allocate other population counts.
#'
#' @param from_geo The geography you want to allocate from. Must be a valid column in \code{parceldat}.
#' @param to_geo The geography you want to allocate to. Must be a valid column in \code{parceldat}
allocate_geography <- function(from_geo, to_geo){
  # note, from and to geographies must align with columns in parceldat, not get_geos character vector

  adf <- parceldat %>%
    ungroup() %>%
    select(ParcelPop, !!from_geo, !!to_geo) %>%  # selecting geographies of interest and pop
    group_by_at(vars(!!from_geo, !!to_geo)) %>%
    summarize(popdist = sum(ParcelPop, na.rm = T)) %>% # summarizing pop in geography intersects
    group_by_at(vars(!!from_geo)) %>%
    mutate(poppct = popdist/sum(popdist, na.rm = T)) %>%
    mutate(poppct = ifelse(is.nan(poppct), 1, poppct)) %>%
    ungroup()

  if(any(is.na(adf[to_geo])) & !is.character(adf[to_geo])) {
    adf[[to_geo]] <- as.character(adf[[to_geo]])
  }

  adf[adf[[to_geo]] == "" | is.na(adf[[to_geo]]), to_geo] <- "No 'to_geo' mapping"

  adf %>%
    select(-popdist) #%>%
  # spread(!!to_geo, poppct, fill = 0) %>%
  # ungroup()
}

#' Re-allocate aggregrate client counts from one geography to another
#'
#' This function provides a user-friendly interface to re-allocate King County aggregate population (e.g. client) counts from one geographic area
#' definition to another (e.g. zip code to county council district).  Users must provide a dataframe of counts aggregated by a geography column
#' matching one of the geography options available by calling the \code{get_geos()} function. This allocation method is an approximation based on overlapping
#' geographic areas, and the results may produce a client count that is different than the original as a result of rounding. Given the inevitability of
#' rounding errors using this allocation method, \strong{we recommend reporting results of this function only as percentages}.
#'
#' This function relies on parcel-level population counts obtained from the King County GIS Center. KC GIS created an algorithm that assigned
#' Census population counts to individual King County parcels using parcel features like the number of bedrooms. This function works as follows:
#' \enumerate{
#' \item Start with the counts in the provided \emph{from geography} and allocate them at the parcel level, proportional to the population of each parcel.
#' Functionally: Total pop in from geography * percent of population of \emph{from geography} within a single parcel, calculated for each parcel.
#' \item Aggregate the parcel-level counts up to the \emph{to geography}. Functionally: Sum of all parcels within \emph{to geography}, for
#' each \emph{to geography}.
#' }
#'
#' Because the function re-allocates clients proportionally to parcel populations, the assignment creates fractional counts that are then rounded to
#' the nearest whole person. This allocation will thus be more accurate with larger client counts: it is easier to correctly allocate 100 clients between
#' three different geographies than it is to fractionally allocate 5 clients among the same three geographies.  We recommend using the reported percentage
#' rather than numeric counts where possible to minimize impacts from rounding errors.
#'
#' Also note that this allocation rests on the assumption that clients are geographically distributed based only on the expected distribution of population
#' across parcels.  It does not take into account any additional information other than census and parcel data.  This allocation method cannot account for
#' other features (such as residence near a provider location).
#'
#' @section Re-allocation errors:
#' \strong{"No 'to_geo' mapping":} Client counts in parcels associated with the \emph{from geography} that do not have an associated \emph{to geography}.
#' For example, for a re-allocation from zip code to tribal area, if half of the population of a zip code is in parcels also located within a tribal area
#' but the other half is not, half of the client count for that zip code will be allocated to that tribal area and the other half will be allocated to
#' "No 'to_geo'mapping".
#'
#' \strong{"Invalid 'from_geo' values":} Clients counts who are recorded with an invalid \emph{from geography}. For example, zip codes associated with
#' a PO Box are not valid geographic zip codes and clients in that zip cannot be re-allocated to a different geography.
#'
#'
#' @param agg_df A dataframe including a geography column matching one of the options in \code{get_geos}.
#' @param from_geo The geography you want to crosswalk from. Must be a valid value in \code{get_geos} and present in \code{agg_df}.
#' @param to_geo The geography you want to crosswalk to Must be a valid value in \code{get_geos}.
#' @param count_vars A character vector containing the names of the columns containing population counts that the user wants to
#' translate from \code{from_geo} to \code{to_geo}.
#' @return Returns a dataframe which contains a \code{to_geo} column and each column specified in \code{count_vars} that provides the
#' translated counts and corresponding percents (in decimal form) in the new geography.
#' @examples
#' \dontrun{
#' # load test data
#' test <- read.csv("L:/PME/GIS Resources/Parcel Crosswalks/ziptestcounts.csv", as.is = T)
#' # clean up test data (if you run the function without cleaning this, you'll see an example of an error the function could throw)
#' test <- test %>%
#'   mutate_at(vars(Zip.Code, BSK.Clients), as.numeric) %>%
#'     rename(zip5 = Zip.Code)
#'
#' ## convert from zip to council district
#' test_dist <- aggregate_crosswalk(agg_df = test,
#'                                  from_geo = "zip5",
#'                                  to_geo = "county_council_district",
#'                                  count_vars = c("BSK.Clients", "MIDD.Clients", "VSHSL.Count"))
#'                                  }
#' @export
aggregate_crosswalk <- function(agg_df, from_geo, to_geo, count_vars) {

  if(!from_geo %in% get_geos()) stop(paste0('from_geo must be one of: c("', paste(get_geos(), collapse = '", "'), '")'))
  if(!to_geo %in% get_geos()) stop(paste0('to_geo must be one of: c("', paste(get_geos(), collapse = '", "'), '")'))
  if(!from_geo %in% names(agg_df)) stop("Your aggregate dataframe must include a geography column with a name that matches your from_geo")

  geo_cols <- geo_col_map()

  # basic string clean up (trim/capitalization)
  if (is.character(agg_df[[from_geo]])) {
    agg_df[[from_geo]] <- stringr::str_trim(toupper(agg_df[[from_geo]]))
  }

  if(from_geo %in% c('zip5', 'city_council_district', 'tract', 'block')) {
    agg_df[[from_geo]] <- clean_numeric(agg_df[[from_geo]])
  }

  if (from_geo == "city") {
    agg_df[[from_geo]] <- stringr::str_replace(agg_df[[from_geo]], "UNINCORPORATED|KING COUNTY", "UNINCORPORATED KING COUNTY")
  }


  from_col <- geo_cols$cols[geo_cols$geographies == from_geo]
  to_col <- geo_cols$cols[geo_cols$geographies == to_geo]

  adf <- allocate_geography(eval(from_col), eval(to_col))

  for(n in names(adf)) {
    if(n %in% geo_cols$cols) {
      names(adf)[names(adf) == n] <- geo_cols$geographies[geo_cols$cols == n]
    }
  }

  ### attaching geo crosswalks to original counts.
  #read.csv("L:/PME/GIS Resources/Parcel Crosswalks/ziptestcounts.csv", as.is = T)  - must clean up names and char cols
  agg_df <- agg_df %>%
    left_join(adf, by = eval(from_geo), na_matches = "never")
  ## setting proportion unknown to 100% to ensure no counts lost - NA to_geos to be recoded to "Invalid" in next operation
  agg_df$poppct[is.na(agg_df[[to_geo]])] <- 1

  trans_df <- agg_df %>%
    mutate_at(.vars = count_vars, ~.*poppct) %>%
    mutate_at(vars(!!to_geo), ~ifelse(is.na(.), "Invalid 'from_geo' values", .)) %>%
    group_by_at(vars(!!to_geo)) %>%
    summarize_at(.vars = count_vars,  ~sum(., na.rm = T)) %>%
    ungroup() %>%
    mutate_at(.vars = count_vars, list(percent = ~round((.)/sum(.), 3))) %>%
    mutate_at(.vars = count_vars, round, 0) %>% # this is the only rounding in the count aggregation process, and it rounds to the nearest whole person
    rename_at(.vars = count_vars, ~paste0(., "_count"))

  if(length(count_vars) == 1) {trans_df <- rename_at(trans_df, .vars = "percent", ~paste0(count_vars, "_percent"))}

  trans_df
}

