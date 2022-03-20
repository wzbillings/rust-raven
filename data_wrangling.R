###
# Data Cleaning and Wrangling for BA data paper
# @author Zane Billings
# @date 2020-04-29
###

library(dplyr, include.only = "%>%")
# Read in raw data
reviews <- arrow::read_parquet(
  here::here("data/raw/beer_reviews.parquet"),
  col_types = "cfddddcfddcdc"
)

# First data set is all reviews.
reviews <- reviews %>%
  dplyr::select(-review_profilename) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(review_time = lubridate::as_datetime(review_time),
         review_date = lubridate::date(review_time),
         beer_name = as.factor(beer_name),
         short_name = as.factor(stringr::str_trunc(
           as.character(beer_name),
           width = 20,side = "right"))) %>%
  dplyr::ungroup()

reviews$category <- forcats::fct_recode(reviews$beer_style,
                       "German Wheat Beer" = "Hefeweizen",
                       "Strong British Ale" = "English Strong Ale",
                       "Dark British Beer" = "Foreign / Export Stout",
                       "Pale Bitter European Beer" = "German Pilsener",
                       "Strong American Ale" = "American Double / Imperial IPA",
                       "Speciality" = "Herbed / Spiced Beer",
                       "Standard American Beer" = "Light Lager",
                       "Dark British Beer" = "Oatmeal Stout",
                       "Standard American Beer" = "American Pale Lager",
                       "Amber Malty European Lager" = "Rauchbier",
                       "Pale American Ale" = "American Pale Ale (APA)",
                       "American Porter and Stout" = "American Porter",
                       "Trappist Ale" = "Belgian Strong Dark Ale",
                       "IPA" = "American IPA",
                       "American Porter and Stout" = "American Stout",
                       "Strong European Beer" = "Russian Imperial Stout",
                       "Amber and Brown American Beer" = "American Amber / Red Ale",
                       "Strong American Ale" = "American Strong Ale",
                       "Amber Malty European Lager" = "Märzen / Oktoberfest",
                       "Standard American Beer" = "American Adjunct Lager",
                       "Pale American Ale" = "American Blonde Ale",
                       "Pale Malty European Lager" = "Euro Pale Lager",
                       "Brown British Beer" = "English Brown Ale",
                       "Strong British Ale" = "Scotch Ale / Wee Heavy",
                       "Speciality" = "Fruit / Vegetable Beer",
                       "American Porter and Stout" = "American Double / Imperial Stout",
                       "Belgian Ale" = "Belgian Pale Ale",
                       "British Bitter" = "English Bitter",
                       "Brown British Beer" = "English Porter",
                       "Irish Beer" = "Irish Dry Stout",
                       "Strong American Ale" = "American Barleywine",
                       "Strong Belgian Ale" = "Belgian Strong Pale Ale",
                       "Strong European Beer" = "Doppelbock",
                       "Pale Malty European Lager" = "Maibock / Helles Bock",
                       "Speciality" = "Pumpkin Ale",
                       "Pale Bitter European Beer" = "Dortmunder / Export Lager",
                       "Strong European Beer" = "Euro Strong Lager",
                       "Dark European Lager" = "Euro Dark Lager",
                       "Speciality" = "Low Alcohol Beer",
                       "German Wheat Beer" = "Weizenbock",
                       "British Bitter" = "Extra Special / Strong Bitter (ESB)",
                       "Amber Malty European Lager" = "Bock",
                       "Pale Commonwealth Beer" = "English India Pale Ale (IPA)",
                       "Amber Bitter European Beer" = "Altbier",
                       "Pale Bitter European Beer" = "Kölsch",
                       "Dark European Lager" = "Munich Dunkel Lager",
                       "Speciality" = "Rye Beer",
                       "Standard American Beer" = "American Pale Wheat Ale",
                       "Dark British Beer" = "Milk / Sweet Stout",
                       "Dark European Lager" = "Schwarzbier",
                       "Amber Bitter European Beer" = "Vienna Lager",
                       "Amber and Brown American Beer" = "American Amber / Red Lager",
                       "Scottish Ale" = "Scottish Ale",
                       "Belgian Ale" = "Witbier",
                       "American Porter and Stout" = "American Black Ale",
                       "Strong Belgian Ale" = "Saison / Farmhouse Ale",
                       "Strong British Ale" = "English Barleywine",
                       "Brown British Beer" = "English Dark Mild Ale",
                       "Amber and Brown American Beer" = "California Common / Steam Beer",
                       "Czech Lager" = "Czech Pilsener",
                       "Pale Commonwealth Beer" = "English Pale Ale",
                       "IPA" = "Belgian IPA",
                       "Trappist Ale" = "Tripel",
                       "European Sour Ale" = "Flanders Oud Bruin",
                       "Amber and Brown American Beer" = "American Brown Ale",
                       "Speciality" = "Winter Warmer",
                       "Speciality" = "Smoked Beer",
                       "Trappist Ale" = "Dubbel",
                       "European Sour Ale" = "Flanders Red Ale",
                       "German Wheat Beer" = "Dunkelweizen",
                       "Historical" = "Roggenbier",
                       "Amber Bitter European Beer" = "Keller Bier / Zwickel Bier",
                       "Belgian Ale" = "Belgian Dark Ale",
                       "Belgian Ale" = "Bière de Garde",
                       "Speciality" = "Japanese Rice Lager",
                       "Speciality" = "Black & Tan",
                       "Irish Beer" = "Irish Red Ale",
                       "Speciality" = "Chile Beer",
                       "Brown British Beer" = "English Stout",
                       "Standard American Beer" = "Cream Ale",
                       "American Wild Ale" = "American Wild Ale",
                       "Strong American Ale" = "American Double / Imperial Pilsner",
                       "Historical" = "Scottish Gruit / Ancient Herbed Ale",
                       "Strong American Ale" = "Wheatwine",
                       "American Porter and Stout" = "American Dark Wheat Ale",
                       "Speciality" = "American Malt Liquor",
                       "Strong European Beer" = "Baltic Porter",
                       "Pale Malty European Lager" = "Munich Helles Lager",
                       "German Wheat Beer" = "Kristalweizen",
                       "Pale Commonwealth Beer" = "English Pale Mild Ale",
                       "European Sour Ale" = "Lambic - Fruit",
                       "Trappist Ale" = "Quadrupel (Quad)",
                       "Strong British Ale" = "Old Ale",
                       "Speciality" = "Braggot",
                       "European Sour Ale" = "Lambic - Unblended",
                       "Strong European Beer" = "Eisbock",
                       "European Sour Ale" = "Berliner Weissbier",
                       "Speciality" = "Kvass",
                       "Belgian Ale" = "Faro",
                       "European Sour Ale" = "Gueuze",
                       "Historical" = "Gose",
                       "Speciality" = "Happoshu",
                       "Speciality" = "Sahti",
                       "Belgian Ale" = "Bière de Champagne / Bière Brut"
)

saveRDS(reviews, file = "cleaned/reviews.rds")

# Second dataset contains average for all beers.
beers <- reviews %>%
  dplyr::group_by(beer_name, beer_style, brewery_name, category) %>%
  dplyr::summarize(rAvg = mean(review_overall),
            Avg_aroma = mean(review_aroma),
            Avg_appearance = mean(review_appearance),
            Avg_palate = mean(review_palate),
            Avg_taste = mean(review_taste),
            ABV = mean(beer_abv),
            num_reviews = n(),
            rSD = sd(review_overall),
            rSE = rSD/num_reviews) %>%
  dplyr::filter(num_reviews > 2) %>%
  dplyr::ungroup()

m <- 10
c <- mean(beers$rAvg)
v <- beers$num_reviews
r <- beers$rAvg
beers$score <- (v / (v + m)) * r + (m / (v + m)) * c
beers$ss <- (beers$score - mean(beers$score)) / sd(beers$score)
beers$short_name <- as.factor(
  stringr::str_trunc(
    as.character(beers$beer_name), 
    width = 20, 
    side = "right"))

saveRDS(beers, file = "cleaned/beers.rds")

# Also, deal with the data from the web scraper, just in case.
json_file <- here::here("beeradvocate-scraper","beers.json")
df <- jsonlite::fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))
df <- df %>%
  dplyr::mutate(
    timestamp = as.POSIXct(timestamp),
    num_reviews = as.numeric(timestamp),
    abv = as.numeric(abv),
    style_id = as.factor(style_id),
    pDev = as.numeric(pDev),
    rAvg = as.numeric(rAvg)
  )

saveRDS(df, "cleaned/scraped_data.rds")