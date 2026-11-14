#### Load packages
library(zippeR)
library(here)
library(readr)
library(tidycensus)
library(tidyverse)
`%+%` <- paste0



#### Load data
pimh_clean <- readRDS(here("Data", "PIMH - 2012-2019", "pimh_clean.rds"))
pih_clean <- readRDS(here("Data", "PIH - 2019-Current", "pih_clean.rds"))


## Urban/rural area (USDA)
zip_to_urban_rural <- read_csv(here("Data", "Additional Data", "RUCA-codes-2020-zipcode.csv")) %>%
  select(
    zip = ZIPCode,
    urban_rural = PrimaryRUCA
  )

## Liberal/conservative (Harvard Dataverse)
zip_to_liberal_conservative_2016 <- read_csv(here("Data", "Additional Data", "zcta-2016-RLCR.csv")) %>%
  mutate(
    year = 2016,
    dem_votes = rowSums(across(starts_with("G16PRED")), na.rm = T),
    total_votes = rowSums(across(starts_with("G16PRE")), na.rm = T),
    dem_vote_share = dem_votes / total_votes
  ) %>%
  select(year, zcta = zcta_GEOID, dem_vote_share)

zip_to_liberal_conservative_2020 <- read_csv(here("Data", "Additional Data", "zcta-2020-RLCR.csv")) %>%
  mutate(
    year = 2020,
    dem_votes = rowSums(across(starts_with("G20PRED")), na.rm = T),
    total_votes = rowSums(across(starts_with("G20PRE")), na.rm = T),
    dem_vote_share = dem_votes / total_votes
  ) %>%
  select(year, zcta = zcta_GEOID, dem_vote_share)

zip_to_liberal_conservative_2024 <- read_csv(here("Data", "Additional Data", "election-results-zcta-2024-landcover-method.csv")) %>%
  mutate(
    year = 2024,
    dem_vote_share = votes_dem / votes_total
  ) %>%
  select(year, zcta = zcta_GEOID, dem_vote_share)

zip_to_liberal_conservative <- bind_rows(
  zip_to_liberal_conservative_2016,
  zip_to_liberal_conservative_2020,
  zip_to_liberal_conservative_2024
)


## Median family income (ACS)
zcta_to_median_family_income <- map(
  .x = lst(2016, 2017, 2018, 2019, 2020, 2021, 2022), # Last year with a five-year window (i.e., 2020-2024)
  .f = ~ get_acs(
    geography = "zcta",
    variables = "B19113_001",
    year = .x + 2, # i.e., the year 2016 gets the 2014-*2018* estimate
    survey = "acs5"
  ) %>%
    select(zcta = GEOID, median_family_income = estimate)
) %>%
  bind_rows(.id = "year") %>%
  mutate(
    year = as.numeric(year),
    median_family_income = median_family_income / 10000 # In $10,000s (plays nice with mice)
  )

# Adjust for inflation
cpi_u <- c( # BLS CPI-U annual averages
  `2018` = 251.107,
  `2019` = 255.657,
  `2020` = 258.811,
  `2021` = 270.970,
  `2022` = 292.655,
  `2023` = 304.702,
  `2024` = 313.689
)

zcta_to_median_family_income_adj <- zcta_to_median_family_income %>%
  mutate(median_family_income = median_family_income * cpi_u[["2024"]] / cpi_u[as.character(year + 2)])


## Access to treatment (County Health Rankings)
county_to_provider_rate <- map(
  .x = lst( # Each release is data for the previous year
    "2016" = here("Data", "Additional Data", "analytic_data2017.csv"),
    "2017" = here("Data", "Additional Data", "analytic_data2018_0.csv"),
    "2018" = here("Data", "Additional Data", "analytic_data2019.csv"),
    "2019" = here("Data", "Additional Data", "analytic_data2020_0.csv"),
    "2020" = here("Data", "Additional Data", "analytic_data2021.csv"),
    "2021" = here("Data", "Additional Data", "analytic_data2022.csv"),
    "2022" = here("Data", "Additional Data", "analytic_data2023_0.csv"),
    "2023" = here("Data", "Additional Data", "analytic_data2024.csv"),
    "2024" = here("Data", "Additional Data", "analytic_data2025_v3.csv"),
    "2025" = here("Data", "Additional Data", "analytic_supplement_20260325.csv")
  ),
  .f = ~ read_csv(
    .x,
    skip = if(grepl("supplement", .x)) 0 else 1, # Supplemental release has one header row, not two
    col_types = cols_only(
      statecode = col_character(),
      countycode = col_character(),
      v062_numerator = col_double(),
      v062_denominator = col_double()
    )
  ) %>%
    filter(countycode != "000") %>%
    mutate(
      county = paste0(statecode, countycode),
      provider_rate = (v062_numerator / v062_denominator) * 100000 # Per 100,000 (plays nice with mice)
    ) %>%
    select(county, provider_rate)
) %>%
  bind_rows(.id = "year") %>%
  mutate(year = as.numeric(year)) %>%
  # Use most recent available data when a release is missing a county (i.e., CT with 2025 data)
  arrange(county, year) %>%
  fill(provider_rate, .by = county, .direction = "down")


## ZIP to ZCTA crosswalk
zip_to_zcta <- map(
  .x = lst(2016, 2017, 2018, 2019, 2020, 2021, 2022), # Last year available: 2022
  .f = ~ zippeR::zi_load_crosswalk(year = .x) %>%
    select(zip = ZIP, zcta = ZCTA)
) %>%
  bind_rows(.id = "year") %>%
  mutate(year = as.numeric(year))


## ZIP to county crosswalk (HUD)
zip_to_county <- read_csv(here("Data", "Additional Data", "ZIP-COUNTY_062026.csv")) %>%
  select(zip, county = geoid, res_ratio) %>%
  slice_max(res_ratio, n = 1, by = zip, with_ties = FALSE)

zip_to_county_2010 <- read_csv(here("Data", "Additional Data", "ZIP-COUNTY_122022.csv")) %>% # Earlier HUD file with CT counties
  select(zip, county_2010 = geoid, res_ratio_2010 = res_ratio) %>%
  slice_max(res_ratio_2010, n = 1, by = zip, with_ties = FALSE)


#### Merge data
add_geo <- function(data) {
  data %>%
    mutate(
      crosswalk_year = pmin(year, 2022), # zippeR crosswalks run through 2022
      acs_year = pmin(year, 2022), # ACS data runs through 2022 (2020-2024 5-year estimate)
      chr_year = pmin(year, 2025), # CHR data runs through 2025 (2025 supplemental data release)
      election_year = case_when( # Choose closest election year
        date < as_date("2018-11-01") ~ 2016,
        date < as_date("2022-11-01") ~ 2020,
        TRUE ~ 2024
      )
    ) %>%
    # Join to ZCTA
    left_join(zip_to_zcta, by = c("crosswalk_year" = "year", "zip"), relationship = "many-to-one") %>%
    # Join to median family income (inflation-adjusted)
    left_join(zcta_to_median_family_income_adj, by = c("acs_year" = "year", "zcta"), relationship = "many-to-one") %>%
    # Join to liberal-conservative vote share
    left_join(zip_to_liberal_conservative, by = c("election_year" = "year", "zcta"), relationship = "many-to-one") %>%
    # Join to urban-rural area
    left_join(zip_to_urban_rural, by = "zip", relationship = "many-to-one") %>%
    # Join to county; takes extra steps because CT abandoned counties in favor of "planning zones"
    left_join(zip_to_county, by = "zip", relationship = "many-to-one") %>%
    left_join(
      zip_to_county_2010,
      by = "zip", relationship = "many-to-one"
    ) %>%
    mutate(
      use_2010 = chr_year <= 2023 & !is.na(county_2010), # CHR data through 2023 use pre-2022 county codes
      county = if_else(use_2010, county_2010, county),
      res_ratio = if_else(use_2010, res_ratio_2010, res_ratio)
    ) %>%
    select(-c(county_2010, res_ratio_2010, use_2010)) %>%
    # Join to provider rate
    left_join(county_to_provider_rate, by = c("chr_year" = "year", "county"), relationship = "many-to-one") %>%
    # Remove unnecessary variables
    select(-c(crosswalk_year, acs_year, chr_year, election_year))
}

pimh_geo <- add_geo(pimh_clean)
pih_geo  <- add_geo(pih_clean)



#### Save data
saveRDS(pimh_geo, here("Data", "PIMH - 2012-2019", "pimh_with_geo.rds"))
saveRDS(pih_geo, here("Data", "PIH - 2019-Current", "pih_with_geo.rds"))
