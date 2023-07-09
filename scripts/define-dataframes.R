# define-dataframes.R

# est == establishments
# emp == employees


###### NC state ######

d_est_total_state_nc <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics == "------") |>
  select(year, fipstate, naics, est, starts_with("n"))

d_est_sector_total_state_nc <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics != "------") |>
  select(year, fipstate, naics, est, starts_with("n")) |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  mutate(pct_est = est / sum(est),
         .by = "year")

d_emp_total_state_nc <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics == "------") |>
  select(year, fipstate, naics, emp, starts_with("e"), avg_wage) 

d_emp_sector_total_state_nc <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics != "------") |>
  select(year, fipstate, naics, emp, starts_with("e"), avg_wage) |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  mutate(pct_emp = emp / sum(emp),
         .by = "year")

d_est_state_nc_total_size_classes <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics == "------") |>
  select(year, fipstate, est, starts_with("n")) |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  pivot_longer(n1_4:n1000,
               names_to = "est_size_class",
               values_to = "value") |>
  mutate(est_size_class = as_factor(est_size_class), # keep existing order
         est_size_class = as.ordered(est_size_class),
         pct_est = value / sum(value),
         .by = "year") |>
  filter(value > 0)

d_emp_state_nc_total_size_classes <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics == "------") |>
  select(year, fipstate, naics, emp, starts_with("e")) |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  pivot_longer(e1_4:e1000,
               names_to = "emp_size_class",
               values_to = "value") |>
  mutate(emp_size_class = as_factor(emp_size_class), # keep existing order
         emp_size_class = as.ordered(emp_size_class),
         pct_emp = value / sum(value),
         .by = "year") |>
  filter(value > 0)

d_est_state_nc_size_classes <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics != "------") |>
  select(year, fipstate, est, starts_with("n")) |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  pivot_longer(n1_4:n1000,
               names_to = "est_size_class",
               values_to = "value") |>
  mutate(est_size_class = as_factor(est_size_class), # keep existing order
         est_size_class = as.ordered(est_size_class),
         pct_est = value / sum(value),
         .by = "year") |>
  filter(value > 0)

d_emp_state_nc_size_classes <- d_cbp_data_state |>
  filter(fipstate == "37",
         naics != "------") |>
  select(year, fipstate, naics, emp, starts_with("e")) |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  pivot_longer(e1_4:e1000,
               names_to = "emp_size_class",
               values_to = "value") |>
  mutate(emp_size_class = as_factor(emp_size_class), # keep existing order
         emp_size_class = as.ordered(emp_size_class),
         pct_emp = value / sum(value),
         .by = "year") |>
  filter(value > 0)

top_naics <- d_emp_sector_total_state_nc |>
  filter(year == 2021) |>
  slice_max(order_by = emp, n = top_naics_cutoff) |>
  mutate(naics_abbr = fct_reorder(naics_abbr, emp),
         naics_sort_order = row_number()) |>
  select(naics, naics_abbr, naics_sort_order)

state_nc_workers_2021 <- d_emp_sector_total_state_nc |>
  filter(year == 2021)


###### NC county ######

d_emp_total_county <- d_cbp_data_county |>
  filter(fipstate == "37",
         naics == "------") |>
  select(year, fipscty, naics, emp) |>
  inner_join(d_fips_county_ref,
             by = c("fipscty")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  mutate(pct_emp = emp / sum(emp),
         .by = c("year")) |>
  filter(emp > 0)

d_emp_sector_total_county <- d_cbp_data_county |>
  filter(fipstate == "37",
         naics != "------") |>
  select(year, fipscty, naics, emp) |>
  inner_join(d_fips_county_ref,
             by = c("fipscty")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  mutate(pct_emp = emp / sum(emp),
         .by = c("year")) |>
  filter(emp > 0)

d_emp_county_nc_size_classes <- d_cbp_data_county |>
  filter(fipstate == "37",
         naics != "------") |>
  select(year, fipstate, fipscty, naics, emp, starts_with("n")) |>
  inner_join(d_fips_county_ref,
             by = c("fipstate", "fipscty")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  pivot_longer(n1_4:n1000,
               names_to = "est_size_class",
               values_to = "value") |>
  # county data doesn't include employees per size class, so we'll estimate
  inner_join(n_employees_ref,
             by = "est_size_class") |>
  rename(est_value = value) |>
  mutate(value = est_value * emp_class_count) |>
  mutate(emp = if_else(sum(emp) == 0, sum(value), mean(emp)),
         .by = c("year", "fipscty", "naics")) |>
  mutate(pct_emp = value / sum(value),
         .by = c("year", "fipscty")) |>
  filter(value > 0)

top_counties_emp <- d_emp_sector_total_county |>
  filter(year == 2021) |>
  summarize(emp_sum = sum(emp),
            .by = county_name) |>
  slice_max(order_by = emp_sum, n = top_county_cutoff)

d_nc_2021_top5_sectors_county_nc <- d_emp_sector_total_county |>
  filter(year == 2021) |>
  inner_join(top_counties_emp,
             by = "county_name") |>
  slice_max(order_by = emp,
            n = 5,
            by = "fipscty") |>
  mutate(n_cat = n(),
         emp_rank = n_cat + 1 - rank(emp,
                                     ties.method = "random"),.by = fipscty)

d_emp_county_healthsoc <- d_cbp_data_county_healthsoc |>
  filter(fipstate == "37") |>
  select(year, fipstate, fipscty, naics, emp, starts_with("n"), avg_wage) |>
  left_join(d_naics_ref_2021,
            by = "naics")


###### USA ######

d_est_emp_total_usa <- d_cbp_data_state |>
  inner_join(d_fips_state_ref,
             by = c("fipstate")) |>
  inner_join(d_naics_ref_2021,
             by = "naics") |>
  inner_join(d_naics_abbr_ref,
             by = "naics_descr") |>
  summarize(# establishments
    est = sum(est, na.rm = TRUE),
    n1_4 = sum(n1_4, na.rm = TRUE),
    n5_9 = sum(n5_9, na.rm = TRUE),
    n10_19 = sum(n10_19, na.rm = TRUE),
    n20_49 = sum(n20_49, na.rm = TRUE),
    n50_99 = sum(n50_99, na.rm = TRUE),
    n100_249 = sum(n100_249, na.rm = TRUE),
    n250_499 = sum(n250_499, na.rm = TRUE),
    n500_999 = sum(n500_999, na.rm = TRUE),
    n1000 = sum(n1000, na.rm = TRUE),
    # employees
    emp = sum(emp, na.rm = TRUE),
    e1_4 = sum(e1_4, na.rm = TRUE),
    e5_9 = sum(e5_9, na.rm = TRUE),
    e10_19 = sum(e10_19, na.rm = TRUE),
    e20_49 = sum(e20_49, na.rm = TRUE),
    e50_99 = sum(e50_99, na.rm = TRUE),
    e100_249 = sum(e100_249, na.rm = TRUE),
    e250_499 = sum(e250_499, na.rm = TRUE),
    e500_999 = sum(e500_999, na.rm = TRUE),
    e1000 = sum(e1000, na.rm = TRUE),
    .by = c("year", "naics", "naics_descr", "naics_abbr")
  )

d_est_total_usa <- d_est_emp_total_usa |> #
  filter(naics == "------") |>
  select(year, naics, naics_abbr, est, starts_with("n"))

d_est_sector_total_usa <- d_est_emp_total_usa |>
  filter(naics != "------") |>
  select(year, naics, naics_abbr, est, starts_with("n")) |>
  mutate(pct_est = est / sum(est),
         .by = "year")

d_emp_total_usa <- d_est_emp_total_usa |> #
  filter(naics == "------") |>
  select(year, naics, naics_abbr, emp, starts_with("e"))

d_emp_sector_total_usa <- d_est_emp_total_usa |>
  filter(naics != "------") |>
  select(year, naics, naics_abbr, emp, starts_with("e")) |>
  mutate(pct_emp = emp / sum(emp),
         .by = "year")

d_est_usa_total_size_classes <- d_est_emp_total_usa |>
  filter(naics == "------") |>
  pivot_longer(n1_4:n1000,
               names_to = "est_size_class",
               values_to = "value") |>
  mutate(est_size_class = as_factor(est_size_class), # keep existing order
         est_size_class = as.ordered(est_size_class),
         pct_est = value / sum(value),
         .by = "year") |>
  filter(value > 0)

d_emp_usa_total_size_classes <- d_est_emp_total_usa |>
  filter(naics == "------") |>
  pivot_longer(e1_4:e1000,
               names_to = "emp_size_class",
               values_to = "value") |>
  mutate(emp_size_class = as_factor(emp_size_class), # keep existing order
         emp_size_class = as.ordered(emp_size_class),
         pct_emp = value / sum(value),
         .by = "year") |>
  filter(value > 0)
