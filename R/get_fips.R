#' @title Collecting FIPS Region Codes
#'
#' @description This function collects FIPS region codes.
#'
#' @param type [optional] (character) Type of the regions to be printed
#' (options: \code{"state"}, \code{"country"}, \code{"all"}, default:
#' \code{"country"}).
#'
#' @return A data frame object that includes region names, FIPS codes, and
#' types.
#'
#' @export
#'
#' @examples
#' # Example: Storing FIPS region codes
#' \dontrun{
#'  fips <- get_fips()
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom dplyr select rename filter mutate arrange
#'
get_fips <- function(type = "country") {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  name <- Type <- fips <- FIPS <- fips_10 <- geounit <- NULL

  if (type != "state"){

    regions <- ne_countries(scale = "small") %>%
      st_set_geometry(NULL) %>%
      select(geounit, fips_10) %>%
      rename(
        Name = geounit,
        FIPS = fips_10
      ) %>%
      filter(nchar(FIPS) == 2) %>%
      mutate(Type = "country")

  }

  if (type != "country"){

    states <- ne_states() %>%
      st_set_geometry(NULL) %>%
      select(name, fips) %>%
      rename(Name = name, FIPS = fips) %>%
      filter(nchar(FIPS) == 4) %>%
      mutate(Type = "state")

    if (type == "all") {
      regions <- rbind(regions, states)
    } else {
      regions <- states
    }

  }

  regions <- regions %>%
    arrange(Type, FIPS)

  return(regions)
}
