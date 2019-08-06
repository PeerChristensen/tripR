#' Main function at user level
#'
#' @param base_url url provided by user
#' @param page_lim number of pages to parse
#' @param company  company name
#' @param verbose  whether to print current url
#'
#' @export
#'
#' @import rvest
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import purrr
#'
#' @examples
#' get_reviews("https://www.tripadvisor.dk/Hotel_Review-g189532-d232132-Reviews-Hjerting_Badehotel-Esbjerg_South_Jutland_Jutland.html",2,"hbh")


### MAIN FUNCTION: PREPARE REVIEW DATASET ###

get_reviews <- function(base_url, page_lim = NULL, company = NULL, verbose = TRUE) {

  ### NESTED FUNCTION ###

  build_dfs <- function(url) {

    Sys.sleep(3)
    # whether to print the current url
    if (verbose == TRUE) {
      message(url)
    }

    # get HTML
    #html <- url %>% xml2::read_html()

    html <- NULL
    attempt <- 1
    while(is.null(html) && attempt <= 4 ) {

      attempt <- attempt + 1

      try(
        html <- url %>% xml2::read_html()
      )
    }

    # get name
    if (str_detect(base_url,"Attraction")) {

      name     <- html %>% get_name_attr()
      #location <- html %>% get_location_attr()
      title    <- html %>% get_title_attr()
      date     <- html %>% get_date_attr()
      rating   <- html %>% get_rating_attr()
      review   <- html %>% get_review_attr()

    }

    else {
      name   <- html %>% get_name()
      #location...
      title  <- html %>% get_title()
      date   <- html %>% get_date()
      rating <- html %>% get_rating()
      review <- html %>% get_review()
    }

    review <- as.character(review)

    # gather variables in tibble
    if (length(date) != length(review)) {
      date = NA
    }
    if (length(rating) != length(review)) {
      rating = NA
    }

    tibble(name,date,rating,title,review)
  }

  ### START OF MAIN FUNCTION ###

  # if page limit is set, do..
  if (!is.null(page_lim)) {
    urls <- get_pages(base_url)
    urls <- urls[1:page_lim]
  }

  else if (is.null(page_lim)) {
    urls <- get_pages(base_url)
  }

  # if company name is set, do..
  if (is.null(company)) {
    data <- urls %>% map(build_dfs)
    data <- do.call("rbind",data)
  }

  # if company name is not set, do..
  else if (!is.null(company)) {
    data <- urls %>% map(build_dfs)
    data <- do.call("rbind",data) %>%
      mutate(company = company)   %>%
      select(company, everything())

  }

}
