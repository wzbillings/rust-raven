library(jsonlite)

json_file <- here::here("beeradvocate-scraper","beers.json")
test <- fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))

test$style <- factor(test$style)
