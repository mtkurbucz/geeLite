# Main Function ----------------------------------------------------------------

#' Fetch ISO 3166-2 Region Codes
#'
#' Returns a data frame containing ISO 3166-2 region codes for the specified
#' administrative level.
#' @param admin_lvl [optional] (integer) Specifies the administrative level to
#'   retrieve. Use \code{0} for country-level, \code{1} for state-level, or
#'   \code{NULL} to include all regions (default: \code{0}).
#' @return A data frame containing region names, ISO 3166-2 codes, and the
#'   corresponding administrative levels.
#' @export
#' @examples
#' # Example: Fetch ISO 3166-2 region codes
#' \dontrun{
#'   fetch_regions()
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange bind_rows
#'
fetch_regions <- function(admin_lvl = 0) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  iso <- NULL

  # Validate the 'admin_lvl' parameter
  params <- list(admin_lvl = admin_lvl)
  validate_params(params)

  # Initialize the regions data frame based on the specified 'admin_lvl'
  regions <- NULL

  # Fetch country-level regions if 'admin_lvl' is 0 or NULL
  if (is.null(admin_lvl) || admin_lvl == 0) {
    regions <- fetch_country_regions()
  }

  # Fetch state-level regions if 'admin_lvl' is 1 or NULL; combine if necessary
  if (is.null(admin_lvl) || admin_lvl == 1) {
    states <- fetch_state_regions()
    if (is.null(admin_lvl)) {
      regions <- bind_rows(regions, states)
    } else {
      regions <- states # Only state-level regions if 'admin_lvl' is 1
    }
  }

  # Only state-level regions if 'admin_lvl' is 1
  regions <- regions %>% arrange(admin_lvl, iso)

  return(regions)
}

# Internal Functions -----------------------------------------------------------

#' Fetch Country-Level Regions
#'
#' Retrieves ISO 3166-2 codes for country-level regions.
#' @return A data frame containing country-level region names and ISO 3166-2
#'   codes.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom stringr str_detect
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr select rename filter mutate
#'
fetch_country_regions <- function() {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  geounit <- iso <- iso_a2_eh <- NULL

  # Fetch country data, clean it, and set the 'admin_lvl' to 0 for country-level
  countries <- ne_countries(scale = "small") %>%
    st_set_geometry(NULL) %>%
    select(geounit, iso_a2_eh) %>%
    rename(name = geounit, iso = iso_a2_eh) %>%
    filter(!str_detect(iso, "-99")) %>%
    mutate(admin_lvl = 0)

  return(countries)
}

# ------------------------------------------------------------------------------

#' Fetch State-Level Regions
#'
#' Retrieves ISO 3166-2 codes for state-level regions.
#' @return A data frame containing state-level region names and ISO 3166-2
#'   codes.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom rnaturalearth ne_states
#' @importFrom dplyr select rename filter mutate
#' @importFrom stringr str_detect
#'
fetch_state_regions <- function() {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  iso <- iso_3166_2 <- name <- NULL

  # Fetch state data, clean it, and set the 'admin_lvl' to 1 for state-level
  states <- ne_states() %>%
    st_set_geometry(NULL) %>%
    select(name, iso_3166_2) %>%
    rename(iso = iso_3166_2) %>%
    filter(!str_detect(iso, "~")) %>%
    mutate(admin_lvl = 1)

  return(states)
}
