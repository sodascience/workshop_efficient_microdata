# Exercise time!

# The assignment.
# Create a plot with on the x-axis the three different contract types,
# and on the y-axis the average wage per unit time (wage per hour). 
# Add 95% confidence intervals (+- 1.96 * standard error of the mean)
library(tidyverse)
library(haven)
spolis_loc <- "fake_cbs_data/Spolis/SPOLISBUS2022V2.sav"

# the plot for 100k rows below. With more samples
# we can bring the s.e. down to see if there is a
# significant difference between these items
df_example <- read_spss(spolis_loc, n_max = 1e5) 

df_example |>  
  summarize(
    mean   = mean(SBASISLOON / pmax(1, SBASISUREN)), 
    stderr = sd(SBASISLOON / pmax(1, SBASISUREN)) / sqrt(n()),
    lower  = mean - 1.96*stderr,
    upper  = mean + 1.96*stderr,
    .by = SCONTRACTSOORT
  ) |> 
  ggplot(aes(
    x = as_factor(SCONTRACTSOORT, levels = "labels"),
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


# Use your skills to do this for the whole data without loading it all
# in at once! What is your conclusion?




