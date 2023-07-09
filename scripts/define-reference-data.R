# define-reference-data.R

###### FIPS ######

# since no change in NC county FIPS codes over this time period, I can use the latest
# includes fipscty 999 "Statewide" that corresponds to employment not associated with a county
d_fips_county_ref_all <- read_csv("./data/ref/georef17.txt",
                                  col_types = "ccc",
                                  show_col_types = FALSE) |>
  rename(fipstate = st,
         fipscty = cty,
         county_name = ctyname) |>
  mutate_if(is.character, ~ purrr::map_chr(.x, iconv, "UTF-8", "UTF-8", sub='')) %>% # just in case
  separate(county_name, into = c("county_name", "state_name"), sep = ", ") |>
  mutate(state_name = if_else(state_name == "Massaschusetts", "Massachusetts", state_name)) # yes, there is an error in the source data!

d_fips_county_ref <- d_fips_county_ref_all |>
  filter(fipstate == "37") |>
  mutate(county_name = str_remove(county_name, " County$"),
         county_name = if_else(county_name == "Statewide", "Statewide, NC", county_name)) |>
  select(-state_name)

d_fips_state_ref <- read_csv("./data/ref/georef17.txt",
                             col_types = "ccc",
                             show_col_types = FALSE) |>
  distinct(st, .keep_all = TRUE) |>
  mutate(state_name = str_extract(ctyname, "(?<=, ).*$")) |>
  select(fipstate = st,
         state_name)

# Since sector total codes and descriptions have not changed in this time period, I'll use the latest
d_naics_ref_2021 <- read_csv("./data/ref/naics2017.txt",
                             col_types = "cc",
                             show_col_types = FALSE) |>
  clean_names()|>
  mutate_if(is.character, ~ purrr::map_chr(.x, iconv, "UTF-8", "UTF-8", sub='')) %>% # just in case
  rename(naics_descr = description) |>
  mutate(naics_descr = str_to_sentence(naics_descr),
         naics_descr = str_replace_all(naics_descr, "&", "and"))

d_naics_abbr_ref <- tribble(
  ~ naics_descr,                                                                 ~naics_abbr_num,  ~naics_abbr,
  "Agriculture, Forestry, Fishing and Hunting",                                  3,               "AgForFish",
  "Utilities",                                                                   17,              "Util",
  "Construction",                                                                5,               "Constr",
  "Manufacturing",                                                               10,              "Manuf",
  "Wholesale Trade",                                                             18,              "Whlsale",
  "Retail Trade",                                                                15,              "Retail",
  "Transportation and Warehousing",                                              16,              "Transpt",
  "Information",                                                                 9,               "Info",
  "Finance and Insurance",                                                       7,               "FinIns",
  "Real Estate and Rental and Leasing",                                          14,              "RealEst",
  "Professional, Scientific, and Technical Services",                            13,              "ProfSciTec",
  "Management of Companies and Enterprises",                                     12,              "Mgmt",
  "Administrative and Support and Waste Management and Remediation Services",    2,               "AdminWaste",
  "Educational Services",                                                        6,               "Educ",
  "Health Care and Social Assistance",                                           8,               "HealthSoc",
  "Arts, Entertainment, and Recreation",                                         4,               "ArtEntRec",
  "Accommodation and Food Services",                                             1,               "AccFood",
  "Other Services (except Public Administration)",                               19,              "OtherServ",
  "Industries not classified",                                                   20,              "OtherInd",
  "Mining, Quarrying, and Oil and Gas Extraction",                               11,              "MineOilGas",
  "Total for all sectors",                                                       0,               "TotalAllSec"
) |>
  mutate(naics_descr = str_to_sentence(naics_descr))


###### inflation adjustment ######

d_infation_ref <- read_xlsx("./data/ref/us-inflation.xlsx",
                            skip = 13)

inflation_adj <- d_infation_ref$cpi[d_infation_ref$year == 2021] / d_infation_ref$cpi[d_infation_ref$year == 2001]
d_infation_adj <- d_infation_ref |>
  filter(year >= 2001,
         year <= 2021) |>
  mutate(adj = cpi[year == 2021] / cpi)


###### employee category reference ######

est_size_class_levels = c("n1_4", "n5_9", "n10_19", "n20_49", "n50_99", "n100_249", "n250_499", "n500_999", 
                          "n1000")

emp_size_class_levels = c("e1_4", "e5_9", "e10_19", "e20_49", "e50_99", "e100_249", "e250_499", "e500_999", 
                          "e1000")

n_employees_ref <- tibble(
  est_size_class = est_size_class_levels,
  emp_size_class = emp_size_class_levels,
  emp_class_count = c(2, 7, 15, 35, 75, 175, 375, 750, 2000),
  factor_order = 1:9
)
