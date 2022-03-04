
#' @export
get_reviews <- function(a) {
  
  first_page <- publons("GET", "academic/review/", 
                        query = list(academic = a))  %>% 
    jsonlite::fromJSON() 
  
  1:ceiling(first_page$count/10) %>% 
    purrr::map_dfr(~{
      publons("GET", "academic/review/", 
              query = list(academic = a, 
                           page = .x))  %>% 
        jsonlite::fromJSON() %>% 
        .$results %>% 
        tidyr::unnest(cols = c(ids, verification, publisher), names_sep = "_") %>% 
        tidyr::unnest(cols = c(ids_academic, publisher_ids), names_sep = "_")
    }) %>% 
    dplyr::mutate(reviewer_id = a) 
  
  
}



#' @export
get_journals <- function(id){
  
  review_html <- rvest::read_html(glue::glue("https://publons.com/review/author/{id}/")) %>% 
    rvest::html_nodes(".reviews") %>% 
    rvest::html_nodes("ul") %>%
    rvest::html_nodes("li") %>%
    # html_nodes("a") %>% 
    # as.character() %>% 
    .[!stringr::str_detect(as.character(.), "author")] %>%
    .[stringr::str_detect(as.character(.), "Performed for")] 
  
  j <- review_html %>% 
    rvest::html_text() %>% 
    stringr::str_trim() %>% 
    stringr::str_squish()
  
  j_link <- review_html %>%
    rvest::html_nodes("a") %>% 
    rvest::html_attr("href")
  
  
  return(tibble::tibble(ids_academic_id = id, journal_name = j, journal_link = j_link))
  
}



get_page <- function(a, page) {
  jsonlite::fromJSON(glue::glue("https://publons.com/api/profile/journal/reviewed/{a}/?page={page}")) %>% 
    .$results
}

# get_page <- purrr::possibly(get_page, otherwise = NULL, quiet = F)


#' @export
get_reviewed_journals <- function(a) {
  page1 <- jsonlite::fromJSON(glue::glue("https://publons.com/api/profile/journal/reviewed/{a}"))
  
  # a <- 1722571
  if(page1$count == 0){
    return(NULL)
  } else if(page1$count <= 10){
    fin <- page1$results %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(reviewer_id = as.character(a))
  } else if(page1$count > 10) {
    fin <- 2:ceiling(page1$count/10) %>%
      purrr::map_dfr(~{
        get_page(a, .x)
      }) %>% 
      dplyr::bind_rows(page1$results) %>% 
      janitor::clean_names() %>% 
      dplyr::mutate(reviewer_id = as.character(a))    
  } 
  
  return(fin) 
}

