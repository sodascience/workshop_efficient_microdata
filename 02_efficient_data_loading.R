# Reading and processing data efficiently
# Goal of this file: combine personal income 
# (from INPA) with sex (from GBA) in a single
# dataset
library(tidyverse)
library(haven)

# Put file locations at top of your file
# NB: version number and location of files may change!
persoon_file <- "fake_cbs_data/GBAPERSOONTAB/GBAPERSOON2018TABV2.sav"
inpa_file    <- "fake_cbs_data/INPATAB/INPA2018TABV2.sav"

# reading the data
persoon <- read_spss(file = persoon_file)

# for testing, use n_max; much faster!
persoon <- read_spss(persoon_file, n_max = 1000)

# persoontab (14 MB) full and then select
persoon <- 
  read_spss(persoon_file) |> 
  select(c(RINPERSOON, RINPERSOONS, GBAGESLACHT)) |> 
  mutate(GBAGESLACHT = as_factor(GBAGESLACHT))

# same as col_select argument, but this is much faster & efficient!
persoon <- 
  read_spss(persoon_file, col_select = c(RINPERSOON, RINPERSOONS, GBAGESLACHT)) |> 
  mutate(GBAGESLACHT = as_factor(GBAGESLACHT))

# inpatab (5.3 MB)
inpa <- 
  read_spss(inpa_file, col_select = c(RINPERSOON, RINPERSOONS, INPPERSBRUT)) |> 
  mutate(income_log = log1p(INPPERSBRUT), .keep = "unused")

# combine: joins
income_df <- left_join(
  x = persoon,
  y = inpa, 
  by = join_by(RINPERSOON, RINPERSOONS)
)

# throw away unused datasets
rm(inpa, persoon)

# writing the data (1.3MB)
write_rds(income_df, "processed_data/income_df.rds", compress = "xz")

# done!