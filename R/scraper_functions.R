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
    attributes()                  %>%
    unlist()                      %>%
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

  id <- html                   %>%
    html_nodes("div")          %>%
    html_attr("data-reviewid") %>%
    na.omit()

 # attributes(id) = NULL
}

### FUNCTION: GET NAME ###

get_name <- function(html) {

  name <- html %>%
    html_nodes("[class='ui_header_link social-member-event-MemberEventOnObjectBlock__member--35-jC']") %>%
    html_text()
}

### FUNCTION: GET DATE ###

get_date <- function(html) {

  date <- html         %>%
    html_nodes("span") %>%
    html_text()        %>%
    str_subset("skrev en anmeldelse|wrote") %>%
    str_split("d\\.") %>%
    map_chr(2) %>%
    str_trim() #%>%
    #lubridate::parse_date_time(date,orders=c("mdy")) doesn't work
    #Error in strptime(.enclose(x), .enclose(fmt), tz) : invalid 'tz' value
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

### FUNCTION: GET TITLE ###

get_title <- function(html) {

  title <- html %>%
    html_nodes("[class='hotels-review-list-parts-ReviewTitle__reviewTitleText--3QrTy']") %>%
    html_text()
}

### FUNCTION: GET REVIEW ###

get_review <- function(html) {

  review <- html     %>%
    html_nodes("q") %>%
    html_text()
}

#####################################################
##### ATTRACTION REVIEWS ############################


### FUNCTION: GET DATE ###

get_date_attr <- function(html) {

  date <- html %>%
    html_nodes("[class='ratingDate']") %>%
    html_attrs() %>%
    map(2) %>%
    unlist()
}

### FUNCTION: GET NAME ###

get_name_attr <- function(html) {

  name <- html %>%
    html_nodes("[class='info_text']") %>%
    html_text()
}

### FUNCTION: GET LOCATION ###

get_location_attr <- function(html) {

  location <- html %>%
    html_nodes("div.userLoc") %>%
    html_text()
}

### FUNCTION: GET RATING ###

get_rating_attr <- function(html) {

  rating <- html                   %>%
    html_nodes("div span")         %>%
    html_attr("class")             %>%
    str_subset("ui_bubble_rating") %>%
    .[6:15]                        %>%
    parse_number() / 10

}

### FUNCTION: GET TITLE ###

get_title_attr <- function(html) {

  title <- html %>%
    html_nodes("[class='noQuotes']") %>%
    html_text()
}

### FUNCTION: GET REVIEW ###

get_review_attr <- function(html) {

  review <- html %>%
    html_nodes("[class='entry']") %>% html_text()
}

# # danske
# url <- "https://www.tripadvisor.dk/Attraction_Review-g189541-d197738-Reviews-Tivoli_Gardens-Copenhagen_Zealand.html"
#
# html <- url %>% xml2::read_html()
#
# # dato
# html %>% html_nodes("[class='ratingDate']") %>%
#   html_attrs() %>%
#   map(2) %>%
#   unlist()
#
# # navn
# html %>% html_nodes("[class='info_text']") %>% html_text()
#
# # sted
# html %>% html_nodes("div.userLoc") %>% html_text()
#
# # rating
# html                               %>%
#     html_nodes("div span")         %>%
#     html_attr("class")             %>%
#     str_subset("ui_bubble_rating") %>%
#     .[6:15]                        %>%
#     parse_number() / 10
#
# # titel
# html %>% html_nodes("[class='noQuotes']") %>% html_text()
#
# # review
# html %>% html_nodes("[class='entry']") %>% html_text()
#
#
#
#
#
# # engelske reviews
# url2 <- "https://www.tripadvisor.co.uk/Attraction_Review-g189541-d197738-Reviews-Tivoli_Gardens-Copenhagen_Zealand.html"
#
# html2 <- url2 %>% xml2::read_html()
#
# html2 %>% html_nodes("[class='info_text']") %>% html_text()
#
# # italienske reviews
# url3 <- "https://www.tripadvisor.it/Attraction_Review-g189541-d197738-Reviews-Tivoli_Gardens-Copenhagen_Zealand.html"
#
# html3 <- url3 %>% xml2::read_html()
#
# html3 %>% html_nodes("[class='info_text']") %>% html_text()

# links <- html %>% html_nodes("a") %>% html_attr("href") %>%
#   str_subset("ShowUserReviews")



# #første link der følges..
# url2 <- "https://www.tripadvisor.dk/ShowUserReviews-g189541-d197738-r693066475-Tivoli_Gardens-Copenhagen_Zealand.html"
# html2 <- url2 %>% xml2::read_html()
#
# html2 %>% html_nodes("div.userLoc")
#
# <div class="info_text" onclick="widgetEvCall('handlers.usernameClick', event, this);">
#
#
#
# data = html2 %>%
#   html_nodes("[type='application/ld+json']") %>%
#   html_text() %>%
#   .[[1]] %>%
#   fromJSON()
#
# title <- data$name
# date <- data$datePublished
# review <- data$reviewBody
# rating <- as.numeric(data$reviewRating$ratingValue)

