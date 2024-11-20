set.seed(1729) # for reproducibility

nrow = 5000

logbooks <- data.frame(id = 1:nrow) |>
  dplyr::mutate(species = sample(c("Glitterfin snapper","Starlight trout","Spiralstripe mackerel","Ivoryscale trout","Sparklefin snapper","Quicksilver wrasse"), nrow, replace = TRUE),
                operator = sample(c("Operator A", "Operator B", "Operator C", "Operator D"), nrow, replace = TRUE),
                grid = sample(1:10, nrow, replace = TRUE),
                site = sample(1:10, nrow, replace = TRUE),
                ncrew = sample(1:8, nrow, replace = TRUE),
                method = sample(c("Handline", "Longline"), nrow, replace = TRUE),
                date = as.Date(runif(nrow, min = as.numeric(as.Date("2000-01-01")), max = as.numeric(as.Date("2025-01-01"))), origin = "1970-01-01"),
                weight = runif(nrow, min = 1, max = 100),
                latitude = runif(nrow, min = 20, max = 30),
                longitude = runif(nrow, min = 120, max = 140)) |>
  dplyr::mutate(weight = ifelse(id < 10, weight*5, weight)) |>
  dplyr::mutate(number = round(weight/5),
                boatmark = substr(operator,10,11),
                maximum_fishing_day_count = 1,
                effort_qty = 1,
                method_type = "Line",
                region_coarse = "Region A",
                stock_area = "Area A")

usethis::use_data(logbooks, overwrite = TRUE, compress = "xz")


nrow = 1000

biological_data <- data.frame(id = 1:nrow) |>
  dplyr::mutate(year = sample(2015:2025, nrow, replace = TRUE),
                month = sample(1:12, nrow, replace = TRUE),
                age = runif(nrow, min = 1, max = 20),
                sex = sample(c("Female","Male","Unknown"), nrow, replace = TRUE),
                region = sample(c("Region A","Region B","Region C","Region D"), nrow, replace = TRUE),
                method = sample(c("Line","Net"), nrow, replace = TRUE),
                sector = sample(c("Commercial","Recreational"), nrow, replace = TRUE)) |>
  dplyr::mutate(length = jitter(100*(1-exp(-0.2*(age-0))),1000))

usethis::use_data(biological_data, overwrite = TRUE, compress = "xz")
