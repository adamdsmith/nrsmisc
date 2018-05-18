#' Update list of US weather stations
#'
#' Meant for developer use only

update_wx_stns <- function() {

  # # Don't use GHCN stations for now (except those in US and major territories)
  # ### GHCN STATIONS
  # ghcn_dat <- "C:/Users/adsmith/Downloads/ghcnd-stations.txt"
  # ghcn <- readr::read_fwf(ghcn_dat,
  #                         readr::fwf_empty(ghcn_dat,
  #                                          col_names = c("GHCN", "lat", "lon", "elev_m", "name")),
  #                         col_types = "cddnc---") %>%
  #   mutate(elev_ft = round(elev_m * 3.28084)) %>%
  #   select(GHCN, name, lat, lon, elev_ft)

  ### CO-OP STATIONS
  coop_dat <- "https://www.ncdc.noaa.gov/homr/file/coop-stations.txt"
  coop <- readr::read_fwf(coop_dat,
                          readr::fwf_empty(coop_dat)) %>%
    mutate(WBAN = NA_character_, FAA = NA_character_) %>%
    select(WBAN, COOP = X2, FAA, GHCN = X4, name = X5, lat = X12, lon = X13, elev_ft = X15) %>%
    # Needn't save GHCN for now
    select(-GHCN)

  ### ASOS STATIONS
  asos_dat <- "https://www.ncdc.noaa.gov/homr/file/asos-stations.txt"
  asos <- readr::read_fwf(asos_dat,
                          readr::fwf_empty(asos_dat, skip = 2),
                          skip = 2) %>%
    select(WBAN = X2, COOP = X3, FAA = X4, name = X5, lat = X11, lon = X12, elev_ft = X13) %>%
    filter(!is.na(WBAN)) %>%
    mutate(ASOS = TRUE)
  # Manually fix one missing station FAA ID (19 May 2018)
  asos[grep("CHERRY POINT MCAS ASOS", asos$name), "FAA"] <- "NKT"

  ### AWOS STATIONS
  awos_dat <- "https://www.ncdc.noaa.gov/homr/file/awos-stations.txt"
  awos <- readr::read_fwf(awos_dat,
                          readr::fwf_empty(awos_dat, skip = 2),
                          skip = 2) %>%
    select(WBAN = X2, COOP = X3, FAA = X4, name = X5, lat = X9, lon = X10, elev_ft = X11) %>%
    filter(!is.na(WBAN),
           !WBAN %in% asos$WBAN) # If ASOS too, keep ASOS

  # Reduce AWOS stations already in COOP
  awos <- awos %>%
    filter(!COOP %in% coop$COOP)

  # Remove COOPs that are also ASOS (prefer to keep ASOS)
  coop <- coop %>%
    filter(!COOP %in% asos$COOP)

  # Consolidate
  all_wx <- bind_rows(coop, awos, asos) %>%
    mutate(ASOS = ifelse(is.na(ASOS), FALSE, TRUE),
           id = ifelse(!is.na(FAA), FAA,
                       ifelse(!is.na(WBAN), WBAN, COOP))) %>%
    select(id, name, lon, lat, ASOS)

  saveRDS(all_wx, "./inst/extdata/wx_stations.rds")

}
