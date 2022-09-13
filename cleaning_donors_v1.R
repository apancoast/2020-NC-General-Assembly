library(tidyverse)

# Exploring Donors ----
#let's get that table real purrrty
nc_ga_2020_elected <- read.csv("D:/RStudio/state_congress/CSVs/nc_ga_2020_elected.csv")
donors <- read.csv("D:/RStudio/state_congress/CSVs/donors.csv") %>%
  select( -matches("X")) %>%
  rename(tot_to_candidate = donations_total,
         tot_from_donor = Total.Donations..Click.to.sort.Ascending.,
         donor_name = Donor..Click.to.sort.Ascending.,
         donor_type = Type..Click.to.sort.Descending.
           ) %>%
  mutate(across(everything(), gsub, pattern = "[\\$,]", replacement = "")) %>%
  left_join(nc_ga_2020_elected) %>%
  select( -matches("X")) %>%
  mutate(tot_to_candidate = as.numeric(tot_to_candidate),
         tot_from_donor = as.numeric(tot_from_donor)
  )

#gotta clean entity donors
entity_donors <- donors %>%
  filter(donor_type == "ENTITY") %>%
  select(district,member,tot_from_donor,donor_name) %>%
  arrange(donor_name)

entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 1667

entity_donors <-
  entity_donors %>%
  mutate(donor_name=
            str_replace_all(
              donor_name,
              "10th District Repulican Party|10th Cong Dist Gop|10th Cong Dist Republican Party|10th District Republican Party|10th District Repulican Party",
              "10th Congressional District Republican Party"),
          donor_name=
            str_replace_all(
              donor_name,
              "3m Company Politcal Action Committee|3m PAC|3m Politcal Action Committee",
              "3M Co PAC"),
          donor_name=
            str_replace_all(
              donor_name,
              "3rd District Republican Party|3rd Cong District Republican",
              "3rd Congressional District Republican Party")
         )


#gotta do a lil fix for rows 176:193
aanc <-
  entity_donors %>%
    slice(176:193) %>%
    mutate(donor_name="Apartment Association of North Carolina PAC")
##I don't know how to merge those back in though


View(entity_donors)

test <- left_join(entity_donors, aanc, by="district")







