# Solution 1: duckdb to the rescue!
library(tidyverse)
library(haven)
library(duckdb)
library(dbplyr)

# first, read the whole table into a duckdb
# database. Do this in chunks to ensure low 
# RAM usage.
spolis_loc <- "fake_cbs_data/Spolis/SPOLISBUS2022V2.sav"

drv <- duckdb("processed_data/spolis.duckdb")
dbc <- dbConnect(drv)

cur_pos <- 0L
chunk_size <- 1e6
cur_df <- read_spss(
  file = spolis_loc, n_max = chunk_size, skip = cur_pos, 
  col_select = c(RINPERSOON, RINPERSOONS, SCONTRACTSOORT, SBASISLOON, SBASISUREN)
)
dbWriteTable(dbc, "income", cur_df, append = TRUE)
while (nrow(cur_df) != 0) {
  cur_pos <- cur_pos + nrow(cur_df)
  cat("Row:", cur_pos, "\r")
  cur_df <- read_spss(
    file = spolis_loc, n_max = chunk_size, skip = cur_pos,
    col_select = c(RINPERSOON, RINPERSOONS, SCONTRACTSOORT, SBASISLOON, SBASISUREN)
  )
  dbWriteTable(dbc, "income", cur_df, append = TRUE)
}



# connect to the table we just created
income_tbl <- tbl(dbc, "income")

income_tbl |> 
  summarize(
    mean  = mean(SBASISLOON / pmax(1, SBASISUREN)), 
    stdev = sd(SBASISLOON / pmax(1, SBASISUREN)),
    n     = n(),
    .by   = SCONTRACTSOORT
  ) |> 
  mutate(
    stderr = stdev / sqrt(n),
    lower  = mean - 1.96*stderr,
    upper  = mean + 1.96*stderr
  ) |> 
  ggplot(aes(
    x = as_factor(SCONTRACTSOORT),
    y = mean,
    ymax = upper,
    ymin = lower
  )) +
  geom_pointrange() +
  labs(
    x = "Contract type",
    y = "Average wage",
    title = "Average wage per unit time for different contract types."
  ) +
  theme_linedraw()

dbDisconnect(dbc)
duckdb_shutdown(drv)
