#' Retrieve date range of available meteorological data for one or more stations from
#'  the Applied Climate Information System (ACIS) stations
#'
#' SHubbard, K. G., A. T. DeGaetano, and K. D. Robbins. 2004. A modern Applied Climate
# Information System. Bull. Amer. Meteor. Soc. 85:811-812. doi:10.1175/BAMS-85-6-811.
# See also http://www.rcc-acis.org/docs_webservices.html
#' @param stn vectors of station identification codes for which to retrieve the
#'  period of available ACIS meteorological data. Several identification options
#'  are available, such as WBAN, COOP, FAA, WMO, and GHCN. The available
#'  \code{\link{find_wx_stns}} function returns the FAA code.
#' @export
#' @examples
#' \dontrun{
#' # See what's available around Mattamuskeet NWR
#' # When *not* returning station info
#' matta_wx_stns <- find_wx_stns(-76.175118, 35.451832, n_stns = 5)
#' get_ACIS_meta(matta_wx_stns) # Only 2 of 5 have ACIS data
#'
#' # When returning station info as well
#' matta_wx_stns <- find_wx_stns(-76.175118, 35.451832, n_stns = 5, id_only = FALSE)
#' get_ACIS_meta(matta_wx_stns$id)
#' }

get_ACIS_meta <- function(stn) {

  # Return all common elements
  elems <- c("maxt", "mint", "avgt", "pcpn", "snow", "snwd")

  # Construct URL
  base_url <- "http://data.rcc-acis.org/StnMeta?meta=name,sids,valid_daterange"
  q_url <- paste(base_url,
                 paste0("sids=", paste(stn, collapse = ",")),
                 paste0("elems=", paste(elems, collapse = ",")),
                 sep = "&") %>% utils::URLencode()
  tmp <- httr::GET(q_url)
  httr::stop_for_status(tmp)
  tmp <- httr::content(tmp)[["meta"]]

  if (identical(tmp, list()))
    stop("Sorry, station metadata, and likely weather data, is not available for any of those stations.",
         call. = FALSE)

  # Post-processing
  meta <- lapply(tmp, extract_meta, elems)

  for (i in seq_along(meta)) {
    nm <- meta[[i]]$name
    stns <- meta[[i]]$stn_ids
    wx_dr <- meta[[i]]$wx_dr
    message("ACIS Weather availability summary: ", nm)
    message("Station identifiers:", paste(stns, collapse = "; "))
    print(as.data.frame(wx_dr))
    message("")
  }

  invisible(NULL)

}

extract_meta <- function(meta_list, elems) {

  # Station type codes
  stn_types <- data.frame(code = c(1L:4L, 6L),
                          type = c("wban", "coop", "faa", "wmo", "ghcn"),
                          stringsAsFactors = FALSE)

  # Station name
  nm <- meta_list$name

  # Stations IDs
  stns <- data.frame(sids = unlist(meta_list$sids), stringsAsFactors = FALSE)
  stns <- setNames(data.frame(do.call(rbind, strsplit(stns$sids, split=" ")),
                              stringsAsFactors = FALSE),
                   c("stn", "code")) %>%
    mutate(code = as.integer(code)) %>%
    inner_join(stn_types, by = "code") %>%
    mutate(stn_ids = paste(toupper(type), stn, sep = ": ")) %>%
    pull(stn_ids)

  # Weather variable date ranges
  wx_dr <- meta_list$valid_daterange
  names(wx_dr) <- elems
  wx_dr <- lapply(wx_dr, function(i) paste(i[[1]], i[[2]], sep = " to ")) %>%
    utils::stack() %>%
    select(wx_var = ind, date_range = values) %>%
    group_by(date_range) %>%
    summarize(wx_vars = paste(wx_var, collapse = ", ")) %>%
    select(wx_vars, date_range)

  out <- list(stn_ids = stns, name = nm, wx_dr = wx_dr)
  out
}
