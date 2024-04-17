# Working with larger-than-memory data
library(tidyverse)
library(duckdb)
library(dbplyr)
library(biglmm)

# Task 1: Plotting ----
# a descriptive plot of mean income per year and per sex

## Option 1: chunked data ----
# create some chunked data
for (yr in 2000L:2020L) {
  cat("Creating year:", yr, "\r")
  dat <- 
    read_rds("processed_data/income_df.rds") |> 
    mutate(
      year = yr, 
      income_log = log(pmax(1, rnorm(n(), exp(income_log + 0.01*(yr-2000)), sd = 500))), 
      GBAGESLACHT = factor(GBAGESLACHT)
    )
  filename <- paste0("processed_data/panel_data/dat_", yr, ".rds")
  write_rds(dat, filename)
}

fns <- list.files("processed_data/panel_data", full.names = TRUE)

tab <- 
  read_rds(fns[1]) |> 
  summarize(
    income = expm1(mean(income_log)), 
    .by = GBAGESLACHT
  ) |> 
  mutate(year = 2000)

for (i in 2:length(fns)) {
  tab <- 
    bind_rows(
      tab,
      read_rds(fns[i]) |> 
      summarize(income = expm1(mean(income_log)), .by = GBAGESLACHT) |> 
      mutate(year = 1999 + i)
    )
}

tab |> 
  ggplot(aes(x = year, y = income, colour = GBAGESLACHT)) +
  geom_point() +
  geom_line() +
  scale_colour_viridis_d() +
  theme_minimal()


## Option 2: use a database ----
unlink("processed_data/panel.duckdb") # delete existing database
drv <- duckdb("processed_data/panel.duckdb")
dbc <- dbConnect(drv)
for (fn in fns) {
  cat("Loading file:", fn, "\r")
  dbWriteTable(dbc, "income", read_rds(fn), append = TRUE)
}
# we create a tbl object: a lazy data frame
income_tbl <- tbl(dbc, "income")

# we can perform queries on this virtual table
count(income_tbl)

tab_sql <- 
  income_tbl |> 
  filter(GBAGESLACHT != "Onbekend") |> 
  summarise(
    income = exp(mean(income_log)),
    lower = exp(mean(income_log) - 2 * sd(income_log) / sqrt(n())),
    upper = exp(mean(income_log) + 2 * sd(income_log) / sqrt(n())),
    .by = c(GBAGESLACHT, year)
  )

# lazy evaluation on the database
tab_sql |> show_query()
tab_sql

# create plot
tab_sql |> 
  ggplot(aes(x = year, y = income, ymin = lower, ymax = upper, colour = GBAGESLACHT)) +
  geom_pointrange(position = position_dodge(width = 0.1)) +
  geom_line() +
  theme_minimal()

dbDisconnect(dbc)
duckdb_shutdown(drv)


# Task 2: Regression ----
# Are there differences in income between men and women & do these change over time?
coef_names <- c("(Intercept)", "Year", "Women - Men", "Unknown - Men", "Year : (Women - Men)", "Year : (Unknown - Men)")

## Option 1: Chunked regression ----
res <- biglm(
  formula = income_log ~ I(year-2000L) * factor(GBAGESLACHT, levels = c("Mannen", "Vrouwen", "Onbekend")), 
  data = read_rds(fns[1])
)

# update with data from the other chunks
for (fn in fns[-1]) res <- update(res, moredata = read_rds(fn))

# create a summaryl
s1 <- summary(res)
rownames(s1$mat) <- coef_names
s1

