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

    # whether to print the current url
    if (verbose == TRUE) {
      message(url)
    }
    # get HTML
    html <- url %>% xml2::read_html()

    # get id
    id <- html %>% get_id()

    # get name
    name <- html %>% get_name()

    # get title
    title <- html %>% get_title()

    # get date
    date <- html %>% get_date()

    # get rating
    rating <- html %>% get_rating()

    # get review
    review <- html %>% get_review()
    review <- as.character(review)

    # gather variables in tibble
    if (length(id) != length(review)) {
      id = NA
    }
    if (length(date) != length(review)) {
      date = NA
    }
    if (length(rating) != length(review)) {
      rating = NA
    }

    tibble(id,name,date,rating,title,review)
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

