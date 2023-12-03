
#' Get aoc_input for a given day
#' 
#' @source : credits for this fun to David Robinson https://github.com/dgrtwo who posted it
#' on R4DS channel
#' 
#'
#' @param day 
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
advent_input <- function(day = lubridate::day(Sys.Date()),
                         year = 2023) {
  session <- Sys.getenv("ADVENT_SESSION")
  if (session == "") {
    stop("Must set ADVENT_SESSION in .Renviron")
  }
  
  url <- paste0("https://adventofcode.com/", year, "/day/", day, "/input")
  
  req <- httr::GET(url,
                   httr::set_cookies(session = session))
  httr::stop_for_status(req)
  
  txt <- httr::content(req)
  
  tibble::tibble(x = head(readr::parse_guess(stringr::str_split(txt, "\n")[[1]]), -1))
}
