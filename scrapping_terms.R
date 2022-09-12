library(tidyverse)
library(rvest)

#### STATE HOUSE REPS ####

##Scrapping NC state house rep terms
link <- "https://ballotpedia.org/North_Carolina_House_of_Representatives"
page <- read_html(link)

house_district <- page %>% html_nodes("#officeholder-table td:nth-child(1) a") %>% html_text()
rep <- page %>% html_nodes("#officeholder-table td:nth-child(2)") %>% html_text()
term_start <- page %>% html_nodes("#officeholder-table td:nth-child(4)") %>% html_text()

ga_house <- data.frame(house_district, rep, term_start, stringsAsFactors = FALSE)
write.csv(ga_house, "ga_house.csv")

#exported from the State's website: https://www.ncleg.gov/Members/MemberTable/H
house <- read.csv("house_NCGA.csv") %>%
  rename(party = Party,
         district = District,
         member = Member,
         counties = Counties.Represented) %>%
  mutate(district = paste("North Carolina House of Representatives District", district))

#join
house <- full_join(house_reps,house_terms,by="house_district")

#save
write.csv(merged, "nc_state_house_v2.csv")

#### STATE SENATORS ####

#exported from https://www.ncleg.gov/Members/MemberTable/S
senators <- fread("D:/RStudio/pride_month/Senate - North Carolina General Assembly.csv")

#scrapping for term from https://ballotpedia.org/North_Carolina_State_Senate
link = "https://ballotpedia.org/North_Carolina_State_Senate"
page = read_html(link)

senate_district = page %>% html_nodes("#officeholder-table td:nth-child(1) a") %>% html_text()
term_start = page %>% html_nodes("#officeholder-table td:nth-child(4)") %>% html_text()

nc_senate_terms = data.frame(senate_district, term_start, stringsAsFactors = FALSE)

write.csv(nc_senate_terms, "nc_senate_terms.csv")

#transform
senate_terms <-
  nc_senate_terms %>%
  mutate(across(everything(), gsub, pattern = "North Carolina State Senate District ", replacement = ""))

nc_senators <- sentors %>%
  mutate(senate_district = as.character(factor(senate_district)))

#join
nc_senators.2 <- full_join(nc_senators,senate_terms,by="senate_district")

#save
write.csv(nc_senators.2, "nc_state_senate_v1.csv")


#### CAPE FEAR REGION ####

#Let's look at all of NC's legislative body
nc_leg <- bind_rows(nc_senators.2, merged)
