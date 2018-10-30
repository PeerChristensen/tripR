#' Functions for scraping data
#'
#'
#' @param base_url url provided by user
#'
#' @export
#' @import glue
#' @import rvest
#' @import dplyr
#' @import readr
#' @import lubridate
#' @import stringr

### FUNCTION: GET N PAGES ###

n_pages <- function(base_url){

  base_url                        %>%
    xml2::read_html()             %>%
    html_nodes("a")               %>%
    html_attr("data-page-number") %>%
    na.omit()                     %>%
    as.numeric()                  %>%
    max()
}

### FUNCTION: GET ALL PAGES ###

get_pages <- function(base_url){

  last_page      = n_pages(base_url)
  page_intervals = seq_along(1:(last_page-1)) * 5
  split_url      = base_url %>% str_split("Reviews") %>% unlist()
  combined_urls  = NULL
  for (i in page_intervals){
    combined_urls[i/5]  = glue(split_url[1],"Reviews-or",i,split_url[2])
  }
  urls           = c(base_url,combined_urls)
}

### FUNCTION: GET ID ###

get_id <- function(html) {

  id <- html                          %>%
    html_nodes("[class='info_text']") %>%
    html_node("div")                  %>%
    html_text()
}

### FUNCTION: GET DATE ###

get_date <- function(html) {

  date <- html                         %>%
    html_nodes("[class='ratingDate']") %>%
    html_attr("title")                 %>%
    dmy()
}

### FUNCTION: GET RATING ###

get_rating <- function(html) {

  rating <- html                   %>%
    html_nodes("div span")         %>%
    html_attr("class")             %>%
    str_subset("ui_bubble_rating") %>%
    .[5:9]                         %>%
    parse_number() / 10
}

### FUNCTION: GET REVIEW ###

get_review <- function(html) {

  review <- html     %>%
    html_nodes("[class='partial_entry']") %>%
    html_text()      %>%
    str_remove("\n")
}
