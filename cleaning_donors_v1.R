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
  mutate(id = 1:6122) %>%
  select(district,member,tot_from_donor,donor_name, id) %>%
  arrange(donor_name)

entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 1667

#entity_donors <-
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
              "3rd Congressional District Republican Party"),
          donor_name=
            str_replace_all(
              donor_name,
               "Aanc PAC",
               "Apartment Association of North Carolina"),
          donor_name=
             str_replace_all(
               donor_name,
               "Acec NC PAC|Acec of NC PAC|Acec of North Carolina PAC",
               "American Council of Engineering Companies"),
         donor_name=
           str_replace_all(
             donor_name,
             " \\(Acec/NC\\) PAC| \\(Acec/PAC\\)",
             ""),
         donor_name=
           str_replace_all(
             donor_name,
             "Acpac Automobile Club",
             "Auto Care Association's PAC"),
         donor_name=
           str_replace_all(
             donor_name,
             "Actbluedonate|Act Blue",
             "ActBlue"),
         donor_name=
           str_replace_all(
             donor_name,
             "Adp",
             "ADP"),
         donor_name=
           str_replace_all(
             donor_name,
             "Alamance County Republican Party|Alamance County of Republican Party",
             "Alamance County of Republican Party Building Fund"),
         donor_name=
           str_replace_all(
             donor_name,
             "Alamance County Republican Women's Club|Alamance Republican Women of NC",
             "Alamance Republican Women"),
         donor_name=
           str_replace_all(
             donor_name,
             " Exec ",
             " Executive "),
         donor_name=
           str_replace_all(
             donor_name,
             "Alexander Republican Party",
             "Alexander County Republican Party"),
         donor_name=
           str_replace_all(
             donor_name,
             "Alleghany County Repbulican Part",
             "Alleghany County Repbulican Party"),
         donor_name=
           str_replace_all(
             donor_name,
             "American Anestesiology of NC PAC|American Anethesiology of NC PAC",
             "American Anesthesiology of NC PAC"),
         donor_name=
           str_replace_all(
             donor_name,
             "American Federation of State County and Municipal Employees AFSCME|American Fed of State Cty & Municipal Emp Afscme-People",
             "American Federation of State County and Municipal Employees (AFSCME-PEOPLE)"),
         donor_name=
           str_replace_all(
             donor_name,
             "Apta North Carolina PAC \\(Formerly NC Physical Therapy PAC\\)|Apta NC PAC",
             "American Physical Therapy Association NC PAC")
         )

  slice(128:130)

# Row Updates ----
#gotta do a lil fix for rows 176:193
aanc <-
  entity_donors %>%
    slice(176:193) %>%
    mutate(donor_name="Apartment Association of North Carolina")

entity_donors <- entity_donors %>%
  rows_update(aanc, by = c("id"))

ahold <-
  entity_donors %>%
  slice(34:43) %>%
  mutate(donor_name="Ahold Delhaize USA, Inc PAC")

entity_donors <- entity_donors %>%
  rows_update(ahold, by = c("id"))

allstate <-
  entity_donors %>%
  slice(61:71) %>%
  mutate(donor_name="Allstate Insurance Comp PAC")

entity_donors <- entity_donors %>%
  rows_update(allstate, by = c("id"))

amazon <-
  entity_donors %>%
  slice(75:78) %>%
  mutate(donor_name="Amazon")

entity_donors <- entity_donors %>%
  rows_update(amazon, by = c("id"))

aapac <-
  entity_donors %>%
  slice(79:97) %>%
  mutate(donor_name="American Airlines Inc PAC")

entity_donors <- entity_donors %>%
  rows_update(aapac, by = c("id"))

akc <-
  entity_donors %>%
  slice(117:124) %>%
  mutate(donor_name="American Kennel Club PAC")

entity_donors <- entity_donors %>%
  rows_update(akc, by = c("id"))

apta <-
  entity_donors %>%
  slice(4099:4104) %>%
  mutate(donor_name="American Physical Therapy Association NC PA")

entity_donors <- entity_donors %>%
  rows_update(apta, by = c("id"))


