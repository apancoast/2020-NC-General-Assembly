library(tidyverse)
library(rvest)

### GET PAGES OF CANDIDATES ###
candidate_pages <- tibble(page_num = 1:8)
candidate_pages <- candidate_pages %>%
  mutate(page = paste0("https://www.transparencyusa.org/nc/candidates?cycle=2020-election-cycle&by=candidateOfficeHeld&order=desc", page_num))

### GET CANDIDATE'S PAGES ###
get_candidates <- function(page) {
  page <- read_html(page)

  page %>%
      html_nodes(".table-hover a") %>%
      html_attr("href") %>%
      as_tibble() %>%
      transmute(links = paste0("https://www.transparencyusa.org", value, "/donors?by=donorTypeCode", sep=""))
}

#Scrape candidate links
candidate_pages <- candidate_pages %>%
  mutate(links = map(page, get_candidates))

candidate_pages <- candidate_pages %>%
  unnest(links)

get_district = function(links) {
  links <- read_html(links)

  links %>%
    html_nodes(".profile-subtitle span") %>%
    html_text() %>%
    as_tibble() %>%
    rename("district" = "value")
}

#scrap districts
candidates <- candidate_pages %>%
  mutate(district = map(links, get_district))

candidate_pages <- candidates %>%
  unnest(district)

#170 Districts (120 reps + 50 senators) so let's confirm we got all of them
check <- candidate_pages %>%
  filter(grepl('District', district))

#### NEW and not working ####

get_tot_donations = function(links) {
  links <- read_html(links)

  links %>%
    html_nodes(".hide-tablet+ .number") %>%
    html_text() %>%
    as_tibble()

}

#scrap total donations per candidate
donations <- candidate_pages %>%
  mutate(tot_donations = map(links, get_tot_donations))

candidate_pages <- donations %>%
  unnest(tot_donations)

#### END NEW ####

get_tables = function(links) {
  links <- read_html(links)

  links %>%
    html_table()
}

candidate_pages <- candidate_pages %>%
  mutate(tables = map(links, get_tables))

donors <- candidate_pages %>%
  unnest(tables)

#Final and cleaning the table

donors.table <- donors %>%
  unnest(tables) %>%
  rename("donation_total" = "Total Donations (Click to sort Ascending)",
         "donor_name" = "Donor (Click to sort Ascending)",
         "donor_type" = "Type (Click to sort Descending)") %>%
  mutate(district = str_trim(district)) %>%
  select(links, district, donation_total, donor_name, donor_type)

write.csv(donors.table, "donor_table.csv")







