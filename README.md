# tripR
A package for scraping ratings and reviews from Tripadvisor

Use get_reviews() to fetch reviews of a particular business.

example:
```{r}
data <- get_reviews("landing_page_url", page_lim = NULL, company_name = NULL)
```
