library(dplyr)

# load data from flat files exported from AFSC database
haul <- read.csv(here::here("data", "raw", "haul.csv"))

# get haul midpoints
haul <- cbind(
  haul,
  geosphere::midPoint(
    cbind(haul$START_LONGITUDE, haul$START_LATITUDE),
    cbind(haul$END_LONGITUDE, haul$END_LATITUDE)
  )
) 

haul$DATE <- as.Date(haul$START_TIME, format = "%d-%b-%y")
haul$MONTH <- lubridate::month(haul$DATE)
haul$DAY <- lubridate::day(haul$DATE)
haul$YEAR <- lubridate::year(haul$DATE)

# filter by region, year, and whether the haul was satisfactory for inclusion in abundance index comp
haul <- haul %>%
  filter(
    REGION == "GOA",
    YEAR > 1989,
    YEAR < 2020,
    ABUNDANCE_HAUL == "Y"
  )

saveRDS(
  object = haul,
  file = here::here("data", "processed", "AK_GOA_BTS_hauls.rds")
)
