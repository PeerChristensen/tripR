# tripR
A package for scraping ratings and reviews from Tripadvisor

By default, tripR reads all pages containing reviews of a particular business.

Set page_lim (optional), if you only want to scrape a specific number of pages.

Set company_name (optional), if you want to include a name in the data frame.

Use get_reviews() to fetch reviews of a particular business.

example:
```{r}
data <- get_reviews("landing_page_url", page_lim = NULL, company_name = NULL)
```
