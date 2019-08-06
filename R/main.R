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
    html <- retry(xml2::read_html(url))

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
    if (length(id) != length(review)) {
      id = NA
    }
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

# function for retries
library(futile.logger)
library(utils)

retry <- function(expr, isError=function(x) "try-error" %in% class(x), maxErrors=5, sleep=0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]", capture.output(str(retval)))
      flog.fatal(msg)
      stop(msg)
    } else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]", attempts, maxErrors,
                    capture.output(str(retval)))
      flog.error(msg)
      warning(msg)
    }
    if (sleep > 0) Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(retval)
}
