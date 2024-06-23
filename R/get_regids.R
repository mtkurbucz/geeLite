#' @title Collecting ISO 3166-2 Region Codes
#'
#' @description This function collects ISO 3166-2 region codes.
#'
#' @param type [optional] (character) Type of the regions to be printed
#' (options: \code{"state"}, \code{"country"}, \code{"all"}, default:
#' \code{"country"}).
#'
#' @return A data frame object that includes region names, ISO 3166-2 codes, and
#' types.
#'
#' @export
#'
#' @examples
#' # Example: Storing ISO 3166-2 region codes
#' \dontrun{
#'  regids <- get_regids()
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom dplyr select rename filter mutate arrange
#'
get_regids <- function(type = "country") {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  iso <- name <- iso_a2_eh <- iso_3166_2 <- geounit <- NULL

  if (type != "state"){
    regions <- ne_countries(scale = "small") %>%
      st_set_geometry(NULL) %>%
      select(geounit, iso_a2_eh) %>%
      rename(name = geounit, iso = iso_a2_eh) %>%
      filter(!str_detect(iso, "-99")) %>%
      mutate(type = "country")
  }

  if (type != "country"){
    states <- ne_states() %>%
      st_set_geometry(NULL) %>%
      select(name, iso_3166_2) %>%
      rename(iso = iso_3166_2) %>%
      filter(!str_detect(iso, "~")) %>%
      mutate(type = "state")
    if (type == "all") {
      regions <- rbind(regions, states)
    } else {
      regions <- states
    }
  }

  regions <- regions %>% arrange(type, iso)

  return(regions)
}
