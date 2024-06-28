#' @title Retrieve ISO 3166-2 Region Codes
#'
#' @description This function returns a data frame containing ISO 3166-2 region codes for specified administrative levels.
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
#'   regids <- get_regids()
#'   print(regids)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom sf st_set_geometry
#' @importFrom rnaturalearth ne_states ne_countries
#' @importFrom dplyr select rename filter mutate arrange
#' @importFrom stringr str_detect
#'
get_regids <- function(admin_lvl = 0) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  iso <- name <- iso_a2_eh <- iso_3166_2 <- geounit <- NULL

  # Validate 'admin_lvl' parameter
  if (!is.null(admin_lvl) && !admin_lvl %in% c(0, 1)) {
    stop("Invalid 'admin_lvl' parameter. Valid entries are 0, 1, or NULL.")
  }

  regions <- NULL

  # Retrieve country-level regions if admin_lvl is 0 or NULL
  if (is.null(admin_lvl) || admin_lvl == 0) {
    regions <- ne_countries(scale = "small") %>%
      st_set_geometry(NULL) %>%
      select(geounit, iso_a2_eh) %>%
      rename(name = geounit, iso = iso_a2_eh) %>%
      filter(!str_detect(iso, "-99")) %>%
      mutate(admin_lvl = 0)
  }

  # Retrieve state-level regions if admin_lvl is 1 or NULL
  if (is.null(admin_lvl) || admin_lvl == 1) {
    states <- ne_states() %>%
      st_set_geometry(NULL) %>%
      select(name, iso_3166_2) %>%
      rename(iso = iso_3166_2) %>%
      filter(!str_detect(iso, "~")) %>%
      mutate(admin_lvl = 1)
    if (is.null(admin_lvl)) {
      regions <- rbind(regions, states)
    } else {
      regions <- states
    }
  }

  # Arrange the regions data frame by administrative level and ISO code
  regions <- regions %>% arrange(admin_lvl, iso)

  return(regions)
}
