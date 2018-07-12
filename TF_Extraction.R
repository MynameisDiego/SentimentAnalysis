library(dplyr)
library(httr)
apiUrl <- 'https://westus.api.cognitive.microsoft.com/emotion/v1.0/recognizeinvideo?outputStyle=perFrame'
key <- 'InsertKey'
urlVideo = 'https://www.dropbox.com/s/dnas7wenqk1oo35/The%20Longest%20Way%201.0%20-%20walk%20through%20China%20and%20grow%20a%20beard%21%20-%20a%20photo%20every%20day%20timelapse.mp4?dl=1'
mybody <- list(url = urlVideo)

faceEMO = POST(
  url = apiUrl,
  content_type('application/json'),
  add_headers(.headers = c('Ocp-Apim-Subscription-Key' = key)),
  body = mybody,
  encode = 'json')
operationLocation <- httr::headers(faceEMO)[["operation-location"]]
while (TRUE) {
    ret <- httr::GET( operationLocation,httr::add_headers(.headers = c('Ocp-Apim-Subscription-Key'= key)))
    con <- httr::content(ret)
    if (is.null(con$status)) {
        warning("Connection Error, wretry after 1 minute")
        Sys.sleep(60)
    } else if (con$status == "Running" | con$status == "Uploading") {
        cat(paste0("status ", con$status, "\n"))
        cat(paste0("progress ", con$progress, "\n"))
        Sys.sleep(60)
    } else {
        cat(paste0("status ", con$status, "\n"))
        break
    }}

data <- (con$processingResult %>% jsonlite::fromJSON())$fragments
data$events <- purrr::map(data$events, function(events) {
    events %>% purrr::map(function(event) {
        jsonlite::flatten(event)
    }) %>% bind_rows()})

    