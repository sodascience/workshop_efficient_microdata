# saving a dataset in different formats
library(tidyverse)
library(palmerpenguins)

df <- penguins_raw

# saving as csv
write_csv(df, "processed_data/penguins.csv")
file.size("processed_data/penguins.csv") / 1000

# serializing to compressed .rds
write_rds(df, "processed_data/penguins.rds", compress = "xz")
file.size("processed_data/penguins.rds") / 1000

# reading from .rds ensures exact replication
df_fromdisk <- read_rds("processed_data/penguins.rds")
identical(df, df_fromdisk)
