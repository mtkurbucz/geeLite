# Main Function ----------------------------------------------------------------

#' @title Retrieve ISO 3166-2 Region Codes
#'
#' @description This function returns a data frame containing ISO 3166-2 region
#' codes for specified administrative levels.
#'
#' @param admin_lvl [optional] (numeric) Specifies the administrative level. Use
#' \code{0} for country-level, \code{1} for state-level, or \code{NULL} to
#' include all regions (default: \code{0}).
#'
#' @return A data frame containing region names, ISO 3166-2 codes, and
#' administrative levels.
#'
#' @export
#'
#' @examples
#' # Example: Retrieve ISO 3166-2 region codes
#' \dontrun{
#'   fetch_regions()
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange
#'
fetch_regions <- function(admin_lvl = 0) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  iso <- name <- iso_a2_eh <- iso_3166_2 <- geounit <- NULL

  # Validate 'admin_lvl' parameter
  params <- list(admin_lvl = admin_lvl)
  validate_params(params)

  # Retrieve regions based on admin_lvl
  regions <- NULL
  if (is.null(admin_lvl) || admin_lvl == 0) {
    regions <- fetch_country_regions()
  }
  if (is.null(admin_lvl) || admin_lvl == 1) {
    states <- fetch_state_regions()
    if (is.null(admin_lvl)) {
      regions <- bind_rows(regions, states)
    } else {
      regions <- states
    }
  }

  # Arrange the regions data frame by administrative level and ISO code
  regions <- regions %>% arrange(admin_lvl, iso)

  return(regions)
}

# Internal Functions -----------------------------------------------------------

#' @title Fetch Country-Level Regions
#'
#' @description Retrieves country-level ISO 3166-2 regions.
#'
#' @return A data frame of country-level regions.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr select rename filter mutate
#' @importFrom stringr str_detect
#'
fetch_country_regions <- function() {

  countries <- ne_countries(scale = "small") %>%
    st_set_geometry(NULL) %>%
    select(geounit, iso_a2_eh) %>%
    rename(name = geounit, iso = iso_a2_eh) %>%
    filter(!str_detect(iso, "-99")) %>%
    mutate(admin_lvl = 0)

  return(countries)
}

# ------------------------------------------------------------------------------

#' @title Fetch State-Level Regions
#'
#' @description Retrieves state-level ISO 3166-2 regions.
#'
#' @return A data frame of state-level regions.
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom rnaturalearth ne_states
#' @importFrom dplyr select rename filter mutate
#' @importFrom stringr str_detect
#'
fetch_state_regions <- function() {

  states <- ne_states() %>%
    st_set_geometry(NULL) %>%
    select(name, iso_3166_2) %>%
    rename(iso = iso_3166_2) %>%
    filter(!str_detect(iso, "~")) %>%
    mutate(admin_lvl = 1)

  return(states)
}
