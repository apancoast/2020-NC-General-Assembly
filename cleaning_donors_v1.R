library(tidyverse)

# Exploring Donors ----
donors <- read.csv("donors.csv")

#let's get that table real purrrty
donors %>%
  rename(tot_to_candidate = donations_total,
         tot_from_donor = Total.Donations..Click.to.sort.Ascending.,
         donor_name = Donor..Click.to.sort.Ascending.,
         donor_type = Type..Click.to.sort.Descending.
           )
