# API Function 
nytAPI_function <- function(term1, begin_date, end_date, api) {
  # Query the term in NYT between date listed 
  searchQ = URLencode(term1) 
  url = paste0('https://api.nytimes.com/svc/search/v2/articlesearch.json?q=', searchQ,
               '&begin_date=', begin_date, '&end_date=', end_date, '&api-key=', api)
  
  # Create an initial search to find out how many total hits, or articles that meet our query parameters
  initialsearch = jsonlite::fromJSON(url, flatten = TRUE) 
  hits = initialsearch$response$meta$hits
  
  # If there are no hits, return an empty data frame
  if (hits == 0) return(data.frame()) 
  
  # Otherwise, find out how many times we'll need to loop thru fcn, each time returning 10 pages
  maxPages = min(round(hits / 10) - 1, 10) 
  
  # Make results a dataframe 
  df = data.frame(id=as.numeric(), created_time=character(),
                  snippet=character(), headline=character())
  
  for (i in 0:maxPages) {
    nytSearch = tryCatch(
      jsonlite::fromJSON(paste0(url, "&page=", i), flatten = TRUE),
      error = function(e) NULL
    )
    if (is.null(nytSearch)) break
    temp = data.frame(id = 1:nrow(nytSearch$response$docs),
                      created_time = nytSearch$response$docs$pub_date,
                      snippet = nytSearch$response$docs$snippet,
                      headline = nytSearch$response$docs$headline.main)
    df = rbind(df, temp)
    Sys.sleep(12)
  }
  return(df)
}
