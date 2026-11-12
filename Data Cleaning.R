#### Load packages
library(tidyverse)
library(here)
library(scales)
library(lubridate)
`%+%` <- paste0


#### PIMH: 2012-2019
### Load data
pimh_raw <- read.csv(here("Data", "PIMH - 2012-2019", "261109 PIMH Depression IAT_withRT.csv"))


### Explore data
## US resident status
pimh_raw %>%
  count(dem_residence %in% c(1, 187), !is.na(dem_residence)) # 1 and 187 are both USA
# Looks good. Will need to filter.

## Mental health history
pimh_raw %>%
  count(mhhistory_depressive_current)
# This is all 1's (saw and endorsed) and NAs (did not see OR did not endorse).
# We need to distinguish between those who saw and did not endorse, vs did not see.
pimh_raw %>%
  mutate(
    saw_first_mh_item = !is.na(mhdem00),
    endorsed_any_mhhistory = if_any(starts_with("mhhistory"), ~ !is.na(.x)),
  ) %>%
  count(saw_first_mh_item, endorsed_any_mhhistory, mhhistory_depressive_current)
# It looks like !is.na(mhdem00) is our gatekeeper - although there is some
# lingering messiness (~200 rows where mhhistory_depressive_current is endorsed
# but mhdem00 is NA; these will be dropped because we can't tell what they mean)

# Q: When did the new item begin to be used? (We will drop previous responses)
pimh_raw %>%
  drop_na(datetime) %>%
  mutate(date = as_date(ymd_hms(datetime))) %>%
  filter(
    year(date) == "2016",
    month(date) == "7",
    day(date) %in% 11:20
  ) %>%
  group_by(date) %>%
  summarize(any_mhhistory = max(mhhistory_depressive_current, na.rm = T))
# A: The first entry appears on 7/15/2016. So to be safe, we'll start data on 7/16.

## Depression symptom severity (DASS-21)
pimh_raw %>%
  count(any_dep = if_any(starts_with("depression"), ~ !is.na(.x)))
# Will need to drop NAs.

# Q: How much overlap is there across missing values
pimh_raw %>%
  count(
    missing_date = is.na(datetime),
    missing_res = is.na(dem_residence),
    missing_mhhx = is.na(mhdem00),
    missing_dep = if_any(starts_with("depression"), ~ is.na(.x))
  )
# A: About the pattern you'd expect. This nets 36k nonmissing responses


### Clean columns
pimh_mutated <- pimh_raw %>%
  mutate(
    dataset = "PIMH",
    date = as_date(ymd_hms(datetime)),
    year = year(date),
    decimal_year = decimal_date(date),
    dep_self_label_now = mhhistory_depressive_current %in% 1,
    dep_self_label_past = mhhistory_depressive_past %in% 1,
    dep_self_label_ever = dep_self_label_now | dep_self_label_past,
    dep_dx = mhhistory_depressive_diagnosed %in% 1,
    race = case_when(
      dem_raceomb == "1" & !dem_ethnicityomb %in% 1 ~ "American Indian or Alaska Native non-Hispanic",
      dem_raceomb %in% c("2", "3", "2:3", "2:03") & !dem_ethnicityomb %in% 1 ~ "Asian non-Hispanic",
      dem_raceomb == "5" & !dem_ethnicityomb %in% 1 ~ "Black non-Hispanic",
      dem_raceomb == "4" & !dem_ethnicityomb %in% 1 ~ "Native Hawaiian or Other Pacific Islander non-Hispanic",
      dem_raceomb == "6" & !dem_ethnicityomb %in% 1 ~ "White non-Hispanic",
      dem_ethnicityomb == 1 ~ "Hispanic",
      dem_raceomb == "7" | grepl(":", dem_raceomb) | dem_ethnicityomb == 3 ~ "Multiracial, Other or Unknown",
      !is.na(dem_raceomb) & !is.na(dem_ethnicityomb) ~ "Multiracial, Other or Unknown",
      T ~ NA_character_
    ),
    gender = recode_values(
      dem_sex,
      1 ~ "Woman",
      2 ~ "Man"
    ),
    education = recode_values(
      dem_education,
      1:4 ~ "High School or Less",
      5 ~ "Some College",
      6:8 ~ "Undergraduate Degree",
      9:14 ~ "Graduate Degree",
      c(-900, NA) ~ NA
    ),
    political_id = dem_socialpoliticalid %>%
      replace_values(-900 ~ NA) %>%
      factor(
        levels = 1:7,
        labels = c(
          "Strongly Liberal",
          "Moderately Liberal",
          "Slightly Liberal",
          "In the Middle",
          "Slightly Conservative",
          "Moderately Conservative",
          "Strongly Conservative"),
        ordered = T
      ),
    zip = if_else(grepl("^[0-9]{5}$", dem_zipcode), dem_zipcode, NA_character_),
    response_time = rowSums(across(ends_with("rt") & !ends_with("trt")), na.rm = TRUE) / 60000
  ) %>%
  # Take sum DASS-21 depression subscale score. All items here have ranges of 1:4, so subtract 7
  rowwise() %>%
  mutate(depression_sum = sum(c_across(all_of("depression" %+% 1:7))) - 7) %>%
  ungroup() %>%
  select(
    dataset, date, year, decimal_year,
    starts_with("dep_self_label"), dep_dx,
    all_of("depression" %+% 1:7), depression_sum,
    age = consentage, race, gender, education, political_id, zip, country = dem_residence,
    response_time, saw_dep_self_label = mhdem00
  )


### Filter rows
pimh_filtered <- pimh_mutated %>%
  tidylog::drop_na(date) %>% # Non-missing date
  tidylog::drop_na(all_of("depression" %+% 1:7)) %>% # Non-missing depression items
  tidylog::drop_na(saw_dep_self_label) %>% # Saw self-labeling item
  tidylog::filter(date >= mdy("7/16/2016")) %>% # First date with self-labeling data
  tidylog::filter(date <= mdy("8/20/2019")) %>% # Last date with real data
  tidylog::filter(is.na(age) | age >= 18) %>% # Adults
  tidylog::filter(country %in% c(1, 187)) %>% # US residents; 9,729 otherwise valid responses are non-US or missing (7,847 have a non-missing other country)
  tidylog::filter(age <= 80 | is.na(age)) %>% # (Likely) valid age data
  tidylog::filter(response_time >= .5) # Minimum response time
# 22,469 rows remain


### Save data
saveRDS(pimh_filtered, here("Data", "PIMH - 2012-2019", "pimh_clean.rds"))



#### PIH: 2019-Present
### Load data
pih_raw <- read.csv(here("Data", "PIH - 2019-Current", "261109 PIH Depression IAT_withRT.csv"))


### Explore data
## US resident status
pih_raw %>%
  count(countryres == 1) # In PIH, only 1 is USA
# Looks good. Will need to filter to countryres == 1.

## Mental health history
# Q: Do rows with all 0's represent respondents without mental health history,
# or respondents who didn't see the page? (The latter should be recoded as NA)
pih_raw %>%
  count(currentmhhx) %>%
  arrange(desc(n)) %>%
  head()
# A: Here, "none of the above" is a response option that is captured (16).
# We'll want to drop blank rows ("") and -999 values. Otherwise, any row with "11"
# codes as T, and any without codes as F
pih_raw %>%
  mutate(
    dep_self_label_current = case_when(
      currentmhhx %in% c("", " ", "-999") ~ NA,
      grepl("11", currentmhhx) ~ T,
      T ~ F
    )
  ) %>%
  count(dep_self_label_current, currentmhhx) %>%
  arrange(desc(n)) %>%
  head()

## Depression symptom severity (DASS-21)
pih_raw %>%
  count(any_dep_is_na = if_any(starts_with("depression"), ~ !is.na(.x)))
# Will need to drop NAs.

# Q: How much overlap is there across missing values
pih_raw %>%
  count(
    missing_date = is.na(date),
    missing_res = is.na(countryres),
    missing_mhhx = currentmhhx %in% c("", " ", "-999"),
    missing_dep = if_any(starts_with("depression"), ~ is.na(.x))
  )
# A: About the pattern you'd expect. This nets 46k nonmissing responses


### Clean columns
pih_mutated <- pih_raw %>%
  mutate(
    dataset = "PIH",
    date = as_date(ymd_hms(date)),
    year = year(date),
    decimal_year = decimal_date(date),
    dep_self_label_now = if_else(currentmhhx == "", NA, grepl("11", currentmhhx)), # Includes a negligible number of respondents who selected "depression" and "none of the above"
    dep_self_label_past = if_else(pastmhhx == "", NA, grepl("11", pastmhhx)),
    dep_self_label_ever = dep_self_label_now | dep_self_label_past,
    age = round(age1),
    race = case_when(
      raceombmulti == "[1]" & !ethnicityomb %in% 1 ~ "American Indian or Alaska Native non-Hispanic",
      raceombmulti %in% c("[2]", "[3]", "[2,3]") & !ethnicityomb %in% 1 ~ "Asian non-Hispanic",
      raceombmulti == "[5]" & !ethnicityomb %in% 1 ~ "Black non-Hispanic",
      raceombmulti == "[4]" & !ethnicityomb %in% 1 ~ "Native Hawaiian or Other Pacific Islander non-Hispanic",
      raceombmulti == "[6]" & !ethnicityomb %in% 1 ~ "White non-Hispanic",
      ethnicityomb == 1 ~ "Hispanic",
      raceombmulti == "[7]" | ethnicityomb == 3 ~ "Multiracial, Other or Unknown",
      !is.na(raceombmulti) & !is.na(ethnicityomb) ~ "Multiracial, Other or Unknown",
      T ~ NA_character_
    ),
    gender = case_when(
      gender == 1 & birth_sex %in% c(1, 3, NA) ~ "Cisgender Man",
      gender == 2 & birth_sex %in% c(2, 3, NA) ~ "Cisgender Woman",
      gender != birth_sex | gender == 3 ~ "TGD",
      T ~ NA
    ),
    education = recode_values(
      edu,
      1:4 ~ "High School or Less",
      5 ~ "Some College",
      6:8 ~ "Undergraduate Degree",
      9:14 ~ "Graduate Degree",
      NA ~ NA
    ),
    political_id = factor(
      politicalid,
      levels = 1:7,
      labels = c(
        "Strongly Liberal",
        "Moderately Liberal",
        "Slightly Liberal",
        "In the Middle",
        "Slightly Conservative",
        "Moderately Conservative",
        "Strongly Conservative"),
      ordered = T
    ),
    referral = broughtwebsite %>%
      replace_values(c("", "null") ~ NA),
    zip = if_else(grepl("^[0-9]{5}$", zipcode), zipcode, NA_character_),
    response_time = rowSums(across(ends_with("rt") & !c(
      "birthmonthrt", "birthyearrt", "genderrt", "countryresrt", "fieldofstudyrt",
      "prioriatrt", "ethnicityombrt", "mjcurrentrt", "iatevaluations001rt",
      "iatevaluations002rt", "iatevaluations003rt", "iatevaluationsrt"
    )), na.rm = TRUE) / 60000
  ) %>%
  # Take sum DASS-21 depression subscale score. All items here have ranges of 1:4, so subtract 7
  rowwise() %>%
  mutate(depression_sum = sum(c_across(all_of("depression" %+% 1:7))) - 7) %>%
  ungroup() %>%
  select(
    user_id, session_id, dataset, date, year, decimal_year,
    starts_with("dep_self_label"),
    all_of("depression" %+% 1:7), depression_sum,
    age, race, gender, education, political_id, referral, zip, country = countryres,
    response_time
  )


### Filter rows
## Filter responses
pih_filtered <- pih_mutated %>%
  tidylog::drop_na(all_of("depression" %+% 1:7)) %>% # Non-missing depression items
  tidylog::drop_na(dep_self_label_now) %>% # Non-missing self-labeling item
  tidylog::filter(country == 1) %>% # US residents
  tidylog::filter(is.na(age) | age >= 18) %>% # Adults
  tidylog::filter(date >= mdy("8/21/2019")) %>% # After PIH launch
  tidylog::filter(age <= 80 | is.na(age)) %>% # (Likely) valid age data
  tidylog::filter(response_time >= 1) # Minimum response time
# 37,138 rows remain

## Deduplicate
# Where possible, filter repeat responses from the same ID. Only 3,435 (3%) of raw
# PIH responses come from registered accounts. Of these, 708 (21%) responses are
# from accounts with more than one response. Of valid (i.e., otherwise included)
# responses from these accounts, keep only the first one.
duplicate_responses <- pih_filtered %>%
  arrange(user_id, date) %>%
  filter(
    user_id != -1,
    duplicated(user_id)
  ) %>%
  pull(session_id)

pih_deduplicated <- pih_filtered %>%
  tidylog::filter(!session_id %in% duplicate_responses)
# 37,109 rows remain


### Save data
saveRDS(pih_deduplicated, here("Data", "PIH - 2019-Current", "pih_clean.rds"))



#### Sanity checks
## Combine datasets for sanity checks (not done in analysis)
combined_data <- bind_rows(
  pimh_filtered,
  pih_deduplicated
) %>%
  # Recode certain demographic variables to examine change over time
  mutate(
    race_recoded = replace_values(
      race,
      "Asian non-Hispanic" ~ "Asian non-Hispanic",
      "Black non-Hispanic" ~ "Black non-Hispanic",
      "White non-Hispanic" ~ "White non-Hispanic",
      "Hispanic" ~ "Hispanic",
      c(
        "American Indian or Alaska Native non-Hispanic",
        "Native Hawaiian or Other Pacific Islander non-Hispanic",
        "Multiracial, Other or Unknown"
      ) ~ "Multiracial, Other or Unknown",
      NA ~ NA
    ),
    gender_recoded = replace_values(
      gender,
      c("Cisgender Man", "Man") ~ "Man",
      c("Cisgender Woman", "Woman") ~ "Woman",
      c("TGD", NA) ~ NA
    ),
    referral_recoded = case_when(
      referral == "Assignment for school" ~ "Assignment for school",
      !is.na(referral) ~ "Other",
      is.na(referral) ~ NA
    )
  )


## Symptom severity distributions compared to DASS-21 norms: see DASS vs Norms.R
source(here("DASS vs Norms.R"))
dass21_dep_compare(combined_data, depression_sum)
dass21_dep_compare(combined_data, depression_sum, group = dataset)
dass21_dep_compare(combined_data, depression_sum, group = year)


## Missingness
combined_data %>%
  group_by(dataset) %>%
  summarise(across(everything(), ~ percent(mean(is.na(.x)), .1)), .groups = "drop") %>%
  pivot_longer(-dataset, names_to = "variable", values_to = "prop_missing") %>%
  pivot_wider(
    names_from   = dataset,
    values_from  = prop_missing,
    names_prefix = "missing_"
  ) %>%
  print(n = nrow(.))


## Self-labeling counts
combined_data %>%
  mutate(year = year(date)) %>%
  count(dataset, year, dep_self_label_now) %>%
  group_by(dataset, year) %>%
  mutate(
    total_n = sum(n),
    pct     = percent(n / total_n)
  ) %>%
  ungroup() %>%
  select(-n) %>%
  pivot_wider(
    names_from  = dep_self_label_now,
    values_from = pct,
    names_glue  = "pct_{ifelse(dep_self_label_now, 'self_label', 'no_self_label')}"
  ) %>%
  arrange(year, desc(dataset))


## Density of self-labeling by symptom severity by dataset
combined_data %>%
  ggplot(aes(x = depression_sum, group = dep_self_label_now, fill = dep_self_label_now)) +
  geom_density(alpha = .25) +
  facet_wrap(~ dataset)
# Looks fine


## Self-labeling vs diagnosis
combined_data %>%
  filter(dataset == "PIMH") %>%
  group_by(dataset, year) %>%
  summarize(
    n = n(),
    self_label_now = mean(dep_self_label_now),
    self_label_ever = mean(dep_self_label_ever),
    dx = mean(dep_dx)
  )
# This looks great. The concern would be that people only report self-labeling in
# the presence of a formal diagnosis; this is clearly not the case here. This also
# provides more context for how depressed the sample is (20% report a diagnosis!)


## Change in potential weighting variables across years
pct_in <- function(x, levels) {
  x <- x[!is.na(x)]
  percent(mean(x %in% levels), .1)
}

change_by_year <- combined_data %>%
  mutate(year = year(date)) %>%
  group_by(dataset, year) %>%
  summarise(
    n = n(),

    # Race
    pct_white    = pct_in(race, "White non-Hispanic"),
    pct_hispanic = pct_in(race, "Hispanic"),
    pct_black    = pct_in(race, "Black non-Hispanic"),
    pct_asian    = pct_in(race, "Asian non-Hispanic"),

    # Gender (pre-2019 and 2019+ labels pooled)
    pct_man   = pct_in(gender, c("Man", "Cisgender Man")),
    pct_woman = pct_in(gender, c("Woman", "Cisgender Woman")),
    pct_tgd = pct_in(gender, "TGD"),

    # Means
    mean_age            = round(mean(age, na.rm = TRUE), 1),
    mean_political_id   = round(mean(as.numeric(political_id), na.rm = TRUE), 2),

    # Education: college+ vs. no college degree
    pct_hs_or_less = pct_in(education, c("High School or Less")),
    pct_some_college = pct_in(education, c("Some College")),
    pct_undergrad = pct_in(education, c("Undergraduate Degree")),
    pct_graduate   = pct_in(education, c("Graduate Degree")),

    # Referral
    pct_referral_school = pct_in(referral, "Assignment for school"),

    .groups = "drop"
  ) %>%
  arrange(year, desc(dataset))

# Age
change_by_year %>% select(dataset, year, n, mean_age)

aov(
  data = combined_data,
  formula = date ~ age
) %>% summary()

# Gender
change_by_year %>% select(dataset, year, n, pct_man, pct_woman, pct_tgd)

aov(
  data = combined_data,
  formula = date ~ gender_recoded
) %>% summary()

# Race
change_by_year %>% select(dataset, year, n, pct_white:pct_asian)

aov(
  data = combined_data,
  formula = date ~ race_recoded
) %>% summary()

# Education
change_by_year %>% select(dataset, year, n, pct_hs_or_less:pct_graduate)

aov(
  data = combined_data,
  formula = date ~ education
) %>% summary()

# Referral
change_by_year %>% select(dataset, year, n, pct_referral_school)

aov(
  data = combined_data,
  formula = date ~ referral_recoded
) %>% summary()

# All together
aov(
  data = combined_data,
  formula = as.numeric(date) ~ age + race_recoded + gender_recoded + education
) %>% summary()

# With directionality
lm(
  data = combined_data,
  formula = as.numeric(date) ~ age + race_recoded + gender_recoded + education
) %>% summary()


## Change in self-labeling item wording
by_month <- combined_data %>%
  mutate(
    dep_self_label_now = as.numeric(dep_self_label_now),
    month = format(date, "%Y-%m")
  ) %>%
  group_by(dataset, month) %>%
  summarize(
    month_midpoint = mean(date),
    p = mean(dep_self_label_now),
    n = n(),
    .groups = "drop"
  )

by_month %>%
  ggplot(aes(x = month_midpoint, y = p, size = n, group = dataset, color = dataset)) +
  geom_point(alpha = .5) +
  scale_size_area(max_size = 4, guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Depression self-labeling by response date",
    x = NULL, y = "% currently self-labeling as depressed",
    color = "Dataset"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
# There seems to be some discontinuity here; important argument against pooling
