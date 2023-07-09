# define-state-and-county-data

data_fname <- "./data/processed/cbp_county_sector_totals_2001_2021.rds"

if(!file.exists(data_fname)) {
  data_files <- tibble(
    file_path = fs::dir_ls("./data/raw/",
                           regexp = ".{3}[0-2][0-9]co.txt")
  )
  
  get_cdp_data_from_file <- function(file_path, year) {
    read_csv(file = file_path, 
             id = "path",
             col_names = TRUE,
             show_col_types = FALSE) |>
      clean_names() |>
      filter(str_detect(naics, "([0-9]{2})?----"))
  }
  
  dta <- bind_rows(
    bind_rows(
      dta_part1 <- map_dfr(data_files[1:6, ], get_cdp_data_from_file),
      dta_part2 <- map_dfr(data_files[7:14, ], get_cdp_data_from_file),
      dta_part3 <- map_dfr(data_files[15, ], get_cdp_data_from_file),
      dta_part4 <- map_dfr(data_files[16, ], get_cdp_data_from_file)
    ),
    bind_rows(
      dta_part5 <- map_dfr(data_files[17, ], get_cdp_data_from_file),
      dta_part6 <- map_dfr(data_files[18:19, ], get_cdp_data_from_file),
      dta_part7 <- map_dfr(data_files[20:21, ], get_cdp_data_from_file)
    ) |>
      rename(n1_4 = n_5) |>
      # convert to numeric, addressing "N" values
      mutate(across(n1_4:n1000_4, function(x) ifelse(x == "N", "0", x))) |>
      mutate(across(n1_4:n1000_4, as.numeric)),
  ) |>
    mutate(year = as.numeric(paste0("20", str_extract(path, "\\d{2}")))) |>
    select(-ends_with(c("flag", "_nf")),
           -c(censtate:cencty)
    ) |>
    relocate(year, .before = path) |>
    relocate(path, .after = est)
  
  write_rds(dta, data_fname,
            compress = "bz2")
} else {
  dta <- read_rds(data_fname)
}

d_cbp_data_county <- dta |>
  mutate(avg_wage = if_else(emp > 0, (ap * 1000) / emp, NA_real_)) |> # nominal avg wage for sector one year
  select(-c(n1000_1:n1000_4))


###### HealthSoc detail by county

data_fname <- "./data/processed/cbp_county_healthsoc_2001_2021.rds"

if(!file.exists(data_fname)) {
  data_files <- tibble(
    file_path = fs::dir_ls("./data/raw/",
                           regexp = ".{3}[0-2][0-9]co.txt")
  )
  
  get_cdp_data_from_file <- function(file_path, year) {
    read_csv(file = file_path, 
             id = "path",
             col_names = TRUE,
             show_col_types = FALSE) |>
      clean_names() |>
      filter(str_detect(naics, "^62"))
  }
  
  dta <- bind_rows(
    bind_rows(
      dta_part1 <- map_dfr(data_files[1:6, ], get_cdp_data_from_file),
      dta_part2 <- map_dfr(data_files[7:14, ], get_cdp_data_from_file),
      dta_part3 <- map_dfr(data_files[15, ], get_cdp_data_from_file),
      dta_part4 <- map_dfr(data_files[16, ], get_cdp_data_from_file)
    ),
    bind_rows(
      dta_part5 <- map_dfr(data_files[17, ], get_cdp_data_from_file),
      dta_part6 <- map_dfr(data_files[18:19, ], get_cdp_data_from_file),
      dta_part7 <- map_dfr(data_files[20:21, ], get_cdp_data_from_file)
    ) |>
      rename(n1_4 = n_5) |>
      # convert to numeric, addressing "N" values
      mutate(across(n1_4:n1000_4, function(x) ifelse(x == "N", "0", x))) |>
      mutate(across(n1_4:n1000_4, as.numeric)),
  ) |>
    mutate(year = as.numeric(paste0("20", str_extract(path, "\\d{2}")))) |>
    select(-ends_with(c("flag", "_nf")),
           -c(censtate:cencty)
    ) |>
    relocate(year, .before = path) |>
    relocate(path, .after = est)
  
  write_rds(dta, data_fname,
            compress = "bz2")
} else {
  dta <- read_rds(data_fname)
}

d_cbp_data_county_healthsoc <- dta |>
  filter(str_detect(naics, "^62")) |>
  mutate(avg_wage = if_else(emp > 0, (ap * 1000) / emp, NA_real_)) |> # nominal avg wage for sector one year
  select(-c(n1000_1:n1000_4))


###### State-level detail

data_fname <- "./data/processed/cbp_state_sector_totals_2001_2021.rds"

if(!file.exists(data_fname)) {
  data_files <- tibble(
    file_path = fs::dir_ls("./data/raw-state/",
                           regexp = ".{3}[0-2][0-9]st.txt")
  )
  
  get_cdp_data_from_file <- function(file_path, year) {
    read_csv(file = file_path, 
             id = "path",
             col_names = TRUE,
             show_col_types = FALSE) |>
      clean_names() |>
      filter(str_detect(naics, "([0-9]{2})?----"))
  }
  
  dta <- bind_rows(
    bind_rows(
      dta_part1 <- map_dfr(data_files[1:3, ], get_cdp_data_from_file),
      dta_part1a <- map_dfr(data_files[4:6, ], get_cdp_data_from_file),
      dta_part2 <- map_dfr(data_files[7:9, ], get_cdp_data_from_file),
      dta_part2a <- map_dfr(data_files[10:14, ], get_cdp_data_from_file),
      dta_part3 <- map_dfr(data_files[15, ], get_cdp_data_from_file),
      dta_part4 <-  map_dfr(data_files[16, ], get_cdp_data_from_file)
    ) |>
      select(-starts_with(c("q", "f_", "f1", "f2", "f5")),
             -ends_with(c("flag", "_nf", "nf"))),
    bind_rows(
      dta_part5 <- map_dfr(data_files[17, ], get_cdp_data_from_file),
      dta_part6 <- map_dfr(data_files[18:19, ], get_cdp_data_from_file),
      dta_part7 <- map_dfr(data_files[20:21, ], get_cdp_data_from_file)
    ) |>
      select(-starts_with(c("q", "f_", "f1", "f2", "f5")),
             -ends_with(c("flag", "_nf", "nf"))) |>
      rename(n1_4 = n_5,
             e1_4 = e_5,
             a1_4 = a_5
      ) |>
      # convert to numeric, addressing "N" values
      mutate(across(n1_4:n1000, function(x) ifelse(x == "N", "0", x))) |>
      mutate(across(n1_4:n1000, as.numeric))
  ) |>
    filter(is.na(lfo) | lfo == "-") |> # we only want totals
    mutate(year = as.numeric(paste0("20", str_extract(path, "\\d{2}")))) |>
    relocate(year, .before = path) |>
    select(-censtate)
  
  write_rds(dta, data_fname,
            compress = "bz2")
} else {
  dta <- read_rds(data_fname)
}

d_cbp_data_state <- dta |>
  mutate(avg_wage = (ap * 1000) / emp) |> # nominal avg wage for sector one year
  select(-c(lfo))
