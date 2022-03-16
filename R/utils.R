
#' Title getBaseURL()
#'
#' @return A base URL to be used throughout the application. If the BASE_URL
#' environment variable is not set,
#' this function will return https://www.datim.org,
#' otherwise, the value of the environment variable.
#' @export
#'
getBaseURL <- function() {
  if (Sys.getenv("BASE_URL") !=  "")  {
    return(Sys.getenv("BASE_URL"))
  } else {
    futile.logger::flog.warn("No BASE_URL environment variable found. Using www.datim.org")

    return("https://www.datim.org/")

  }
}

getVersionInfo <- function() {


  currDCF <- read.dcf("DESCRIPTION")
  currVersion <- currDCF[1, "Version"]

  #paste0("Version: ", gert::git_branch(), "@", substr(gert::git_log(max=1)$commit, 0, 10)) %>%
  paste0("Version: ", currVersion) %>%
    paste('<div style="font-size:small;text-align: center;"><p>', .) %>%
    paste(., "</p></div>")
}

datapack_config <- function() {

    dp_regions <- datapackr::valid_PSNUs %>% 
      dplyr::filter(stringr::str_detect(ou,"Region")) %>% 
      dplyr::select(ou,country_uid) %>% 
      dplyr::distinct() %>%
      tidyr::nest(country_uids = country_uid) %>% 
      dplyr::rename(datapack_name = ou)
    
    countries_in_regions <- datapackr::valid_PSNUs %>% 
      dplyr::filter(stringr::str_detect(ou,"Region")) %>% 
      dplyr::select(country_name,country_uid) %>% 
      dplyr::distinct() %>%
      tidyr::nest(country_uids = country_uid) %>% 
      dplyr::rename(datapack_name = country_name)
    
    countries_no_regions <- datapackr::valid_PSNUs %>% 
      dplyr::filter(!stringr::str_detect(ou,"Region")) %>% 
      dplyr::select(country_name,country_uid) %>% 
      dplyr::distinct() %>%
      tidyr::nest(country_uids = country_uid) %>% 
      dplyr::rename(datapack_name = country_name)
    
    
    dplyr::bind_rows(dp_regions,countries_in_regions,countries_no_regions) %>% 
      dplyr::arrange(datapack_name)
    
}

getOrgtunitNamefromUID <- function(uid, d2_session) {

    glue(d2_session$base_url, "api/organisationUnits/{uid}?fields=name") %>%
      httr::GET(., handle = d2_session$handle) %>%
      httr::content(., "text") %>%
      jsonlite::fromJSON(.) %>%
      purrr::pluck("name")
}

PSNUxIM_pivot <- function(d) {

  pivot <- d  %>%
    purrr::pluck("memo") %>%
    purrr::pluck("datim") %>% 
    purrr::pluck("by_psnu") %>%
    dplyr::select("Agency", "Partner", "Mechanism",
                  "Organisation unit" = "ou",
                  "Country" = "country_name",
                  "SNU1" = "snu1",
                  "PSNU" = "psnu",
                  "Indicator",
                  "Age",
                  "value")

  rpivotTable(data =  pivot, rows = c("Indicator", "Age"),
              vals = "value", aggregatorName = "Integer Sum", rendererName = "Table"
              , width = "70%", height = "700px")
}
