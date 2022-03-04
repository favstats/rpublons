#' @rdname rpublons
#' @name rpublons-package
#' @title rpublons
#' @aliases rpublons-package rpublons
#' @docType package
#' @description Client for Publons.com
#' @details Extract review information information from Publons \url{https://publons.com}. This is currently a low-level client that handles authorization but no high-level functionality.
#' @author Thomas J. Leeper <thosjleeper@gmail.com>
#' @keywords package 
#' @examples
#' \dontrun{
#'   # retrieve and store API token
#'   publons_auth("myusername", "mypassword")
#'   
#'   # get list of academics
#'   publons("GET", "academic/")
#'   
#'   # get academics by institution
#'   publons("GET", "academic/", query = list(institution = "Harvard University"))
#'   
#'   # get a specific academic by ORCiD
#'   publons("GET", paste0("academic/", "0000-0003-4097-6326"))
#'   
#'   # get reviews for a specific academic by ORCiD
#'   publons("GET", "academic/review/", 
#'           query = list(academic = "0000-0003-4097-6326"))
#'   
#'   # get a specific review
#'   publons("GET", paste0("academic/review/", "YK8zafgo"))
#'   
#' }
#' @references \href{https://publons.com/api/v2/}{Publons API v2 Documentation}
NULL

#' @rdname rpublons
#' @param verb A character string containing an HTTP request verb.
#' @param endpoint A character string containing an API endpoint.
#' @param body Optionally, a request body.
#' @param query Optionally, a list containing HTTP query arguments.
#' @param base_url A character string specifying the base URL for the API.
#' @param token A character string containing a Publons API token. By default, taken from environment variables.
#' @param username A character string, containing a Publons username, passed to \code{publons_auth} if \code{token} is missing. This is used to generate an API token, if needed.
#' @param password A character string, containing a Publons password, passed to \code{publons_auth} if \code{token} is missing. This is used to generate an API token, if needed.
#' @import httr
#' @export
publons <- 
function(verb = c("GET", "POST", "HEAD", "OPTIONS"),
         endpoint, 
         body = NULL,
         query = NULL,
         ...,
         base_url = "https://publons.com/api/v2/", 
         token = Sys.getenv("PUBLONS_TOKEN"),
         username = Sys.getenv("PUBLONS_USERNAME"),
         password = Sys.getenv("PUBLONS_PASSWORD")) {
    
    # authorization
    if (token == "") {
      if (Sys.getenv("PUBLONS_TOKEN") == "") {
        stop("'token' is missing.\nUse 'publons_auth' to set an API token before continuing.")
      }
    }
  
    if(endpoint == "researcher"){
      u <- glue::glue("https://publons.com/{endpoint}/api/")
    } else{
      u <- paste0(base_url, endpoint, "/")
    }
  
    auth_header <- httr::add_headers(Authorization = paste0("Token ", token))
    FUN <- switch(match.arg(verb), GET = httr::GET, POST = httr::POST, HEAD = httr::HEAD, OPTIONS = httr::OPTIONS)
    
    if(base_url != "https://publons.com/api/"){
      if (length(body)) {
        if (length(query)) {
          r <- FUN(u, query = query, auth_header, ..., body = body, encode = "form")
        } else {
          r <- FUN(u, auth_header, ..., body = body, encode = "form")
        }
      } else {
        if (length(query)) {
          r <- FUN(u, auth_header, ..., query = query)
        } else {
          r <- FUN(u, auth_header, ...)
        }
      }      
    } else {
      if (length(body)) {
        if (length(query)) {
          r <- FUN(u, query = query, ..., body = body, encode = "form")
        } else {
          r <- FUN(u, ..., body = body, encode = "form")
        }
      } else {
        if (length(query)) {
          r <- FUN(u, ..., query = query)
        } else {
          r <- FUN(u, ...)
        }
      }        
    }
    

    stop_for_status(r)
    return(content(r, "text", encoding = "UTF-8"))
}

#' @rdname rpublons
#' @export
publons_auth <- function() {
  
    # if(Sys.getenv("PUBLONS_USERNAME") == "" & missing(username)){
    #   username <- readline(prompt = "Please enter your Publons user name")
    # } 
    # 
    # if(Sys.getenv("PUBLONS_PASSWORD") == "" & missing(password)){
    #   password <- readline(prompt = "Please enter your Publons password")
    # } 
  
    # r <- httr::POST("https://publons.com/api/v2/token/", body = list(username = username, password = password), encode = "form")
    # stop_for_status(r)
    # token <- content(r, encoding = "UTF-8")$token
  
    cat("Please find and copy your token from here: https://publons.com/api/v2/\n\nNote: you have to be logged into your publons account.\n\n")
  
    token <- readline(prompt = "Please enter your token: ")
  
    set_renv("PUBLONS_TOKEN" = token)
    
    print("Token has been set!")
    
    # return(token)
}
