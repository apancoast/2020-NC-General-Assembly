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
  arrange(donor_name) %>%
  mutate(id = 1:6122)

entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 1667
row_replace <- function(rows, name) {
  slice <- entity_donors %>%
    slice({{rows}}) %>%
    mutate(donor_name={{name}})

  entity_donors %>%
    rows_update(slice, by = c("id"))
}

entity_donors <- row_replace(1:6, "10th Congressional District Republican Party")
entity_donors <- row_replace(8:12, "3M Co PAC")



# Row Updates ----
#gotta do a lil fix for rows 176:193
third <- entity_donors %>%
  slice(13:14) %>%
  mutate(donor_name="3rd Congressional District Republican Party")

entity_donors <- entity_donors %>%
  rows_update(third, by = c("id"))

aanc <-
  entity_donors %>%
  slice(15, 176:193) %>%
  mutate(donor_name="Apartment Association of North Carolina")

entity_donors <- entity_donors %>%
  rows_update(aanc, by = c("id"))


donor_name=
  str_replace_all(
    donor_name,
    "Acec NC PAC|Acec of NC PAC|Acec of North Carolina PAC",
    "American Council of Engineering Companies")

donor_name=
  str_replace_all(
    donor_name,
    " \\(Acec/NC\\) PAC| \\(Acec/PAC\\)",
    "")









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
  slice(199:201,4099:4104) %>%
  mutate(donor_name="American Physical Therapy Association NC PAC")

entity_donors <- entity_donors %>%
  rows_update(apta, by = c("id"))

apcia <-
  entity_donors %>%
  slice(129:141, 2111) %>%
  mutate(donor_name="American Property Casualty Insurance Association PAC")

entity_donors <- entity_donors %>%
  rows_update(apcia, by = c("id"))

anthem <-
  entity_donors %>%
  slice(160:175) %>%
  mutate(donor_name="Anthem Inc PAC")

entity_donors <- entity_donors %>%
  rows_update(anthem, by = c("id"))

astellas <-
  entity_donors %>%
  slice(209:229) %>%
  mutate(donor_name="Astellas US LLC PAC")

entity_donors <- entity_donors %>%
  rows_update(astellas, by = c("id"))

att <-
  entity_donors %>%
  slice(230:278,310) %>%
  mutate(donor_name="AT&T NC PAC")

entity_donors <- entity_donors %>%
  rows_update(att, by = c("id"))

iatse <-
  entity_donors %>%
  slice(315:317) %>%
  mutate(donor_name="IATSE 491 Back to One PAC")

entity_donors <- entity_donors %>%
  rows_update(iatse, by = c("id"))

BoA <-
  entity_donors %>%
  slice(318:325) %>%
  mutate(donor_name="Bank of America Corp State and Federal PAC")

entity_donors <- entity_donors %>%
  rows_update(BoA, by = c("id"))

bayada <-
  entity_donors %>%
  slice(328:341) %>%
  mutate(donor_name="Bayada Home Health Care Inc NC PAC")

entity_donors <- entity_donors %>%
  rows_update(bayada, by = c("id"))

entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 1617
entity_donors %>%
  distinct(donor_name) %>%
  View()


# Row by row ----
entity_donors <-
  entity_donors %>%
  mutate(donor_name=
           str_replace_all(
             donor_name,
             "Acpac Automobile Club|Automobile Club PAC",
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
             "Alleghany County Republican Party"),
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
             "Anesthesiologist of the Triad PAC|Anesthessologist of the Triad PA",
             "Anesthesiologists of the Triad PAC"),
         donor_name=
           str_replace_all(
             donor_name,
             "Anson County NC Republican Party",
             "Anson County Republican Party"),
         donor_name=
           str_replace_all(
             donor_name,
             "Aristotle",
             "Aristotle PAC"),
         donor_name=
           str_replace_all(
             donor_name,
             "Asd",
             "ASD"),
         donor_name=
           str_replace_all(
             donor_name,
             "Gop",
             "GOP"),
         donor_name=
           str_replace_all(
             donor_name,
             "Atmc",
             "ATMC"),
         donor_name=
           str_replace_all(
             donor_name,
             "NC Auto Dealers Assn PAC|Autopac",
             "NC Auto Dealers Accociation PAC"),
         donor_name=
           str_replace_all(
             donor_name,
             " (Iatse 491)",
             ""),
      )




#don't forget row 310 w AT&T

entity_donors %>%
  distinct(donor_name) %>%
  count()
#1605

autopac
