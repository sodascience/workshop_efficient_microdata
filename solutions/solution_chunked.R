# Solution 2: manual chunks & online statistics to compute
# mean and variance in a streaming way

# function to get a data chunk, with only required columns
get_chunk <- function(start_pos = 0L, chunksize = 1e6) {
  read_spss(
    spolis_loc,
    n_max = chunksize,
    skip = start_pos,
    col_select = c(SBASISLOON, SBASISUREN, SCONTRACTSOORT)
  ) 
}

# function to compute n, the sum, and the sum of squares
compute_stats <- function(df) {
  df |>
    mutate(hourlywage = SBASISLOON / pmax(SBASISUREN, 1)) |>
    summarize(
      sum = sum(hourlywage),
      ssq = sum(hourlywage^2),
      n = n(),
      .by = SCONTRACTSOORT
    )
}

# loop over chunks, add to result every time
cur_pos <- 0L
chunk <- get_chunk(cur_pos)
result <- compute_stats(chunk)
while (nrow(chunk) != 0) {
  cur_pos <- cur_pos + nrow(chunk)
  cat("Row:", cur_pos, "\r")
  chunk <- get_chunk(cur_pos)
  result <- bind_rows(result, compute_stats(chunk))
}
write_rds(result, "processed_data/chunked_result.rds")

# we need to do one extra aggregation step
output <- 
  result |> 
  summarize(
    sum = sum(sum), 
    ssq = sum(ssq),
    n = sum(n), 
    .by = SCONTRACTSOORT
  ) |> 
  mutate(
    mean = sum / n,
    var  = ssq / n - (sum / n)^2,
    sd   = sqrt(var),
    sem  = sd / sqrt(n),
    lwr  = mean - 1.96*sem,
    upr  = mean + 1.96*sem
  )

# create plot!
output |> 
  ggplot(aes(x = as_factor(SCONTRACTSOORT, levels = "labels"), y = mean, ymax = upr, ymin = lwr)) +
  geom_pointrange() +
  labs(
    x = "Contract type",
    y = "Average wage",
    title = "Average wage per unit time for different contract types."
  ) +
  theme_linedraw()

