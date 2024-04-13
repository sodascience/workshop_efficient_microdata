# Exercise time!
library(tidyverse)
library(haven)
spfns <- list.files("fake_cbs_data/Spolis/", full.names = TRUE)


bigtab <- read_spss(spfns[1])
for (fn in spfns[-1]) {
  cat("reading", fn, "\r")
  bigtab <- bind_rows(bigtab, read_spss(fn))
}

write_rds(bigtab, "bigtab.rds")


N <- nrow(bigtab)
bigtab$IKVID <- as.character(round(runif(N, min = 100000000000, max = 999999999999)))
bigtab$SDATUMAANVANGIKO <- as.Date("2013-01-01")
bigtab$SDATUMEINDEIKO <- as.Date("2022-08-23")

write_sav(data = bigtab, path = "fake_cbs_data/Spolis/SPOLISBUS2022V2.sav")
