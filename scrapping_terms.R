library(tidyverse)
library(rvest)

#### STATE HOUSE REPS ####

##Scrapping NC state house rep terms
link <- "https://ballotpedia.org/North_Carolina_House_of_Representatives"
page <- read_html(link)

district <- page %>% html_nodes("#officeholder-table td:nth-child(1) a") %>% html_text()
term_start <- page %>% html_nodes("#officeholder-table td:nth-child(4)") %>% html_text()

ga_house_terms <- tibble(district, term_start)
write.csv(ga_house_terms, "~/RStudio/state_congress/CSVs/ga_house_terms.csv")

#exported from the State's website: https://www.ncleg.gov/Members/MemberTable/H
house <- read.csv("~/RStudio/state_congress/CSVs/house_NCGA.csv") %>%
  rename(party = Party,
         district = District,
         member = Member,
         counties = Counties.Represented) %>%
  mutate(district = paste("North Carolina House of Representatives District", district))

#join
house <- full_join(house, ga_house_terms,by="district")

#### STATE SENATORS ####
#exported from the State's website: https://www.ncleg.gov/Members/MemberTable/S
senate <- read.csv("~/RStudio/state_congress/CSVs/senate_NCGA.csv") %>%
  rename(party = Party,
         district = District,
         member = Member,
         counties = Counties.Represented) %>%
  mutate(district = paste("North Carolina State Senate District", district))

#scrapping for term from https://ballotpedia.org/North_Carolina_State_Senate
link <- "https://ballotpedia.org/North_Carolina_State_Senate"
page <- read_html(link)

district <- page %>% html_nodes("#officeholder-table td:nth-child(1) a") %>% html_text()
term_start <- page %>% html_nodes("#officeholder-table td:nth-child(4)") %>% html_text()

ga_senate_terms <- tibble(district, term_start)

write.csv(ga_senate_terms, "~/RStudio/state_congress/CSVs/ga_senate_terms.csv")

#join
senate <- full_join(senate,ga_senate_terms,by="district")

#join senate and house
nc_ga_2020_elected <- bind_rows(senate, house) %>%
  mutate(counties = gsub("\\s+", "", counties),
         member = gsub("\\s+", " ", member))

nc_ga_2020_elected %>% write.csv("~/RStudio/state_congress/CSVs/nc_ga_2020_elected.csv")
