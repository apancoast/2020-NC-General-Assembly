library(tidyverse)
library(rvest)
library(progress)

### GET PAGES OF CANDIDATES ###
candidate_pages <- tibble(page_num = 1:7)
candidate_pages <- candidate_pages %>%
  mutate(page = paste0("https://www.transparencyusa.org/nc/candidates?by=candidateOfficeHeld&order=desc&page=", page_num))

### GET CANDIDATE'S PAGES ###
get_candidates <- function(page) {
  page <- read_html(page)

  page %>%
    html_nodes(".table-hover a") %>%
    html_attr("href") %>%
    as_tibble() %>%
    transmute(links = paste0("https://www.transparencyusa.org", value, "/donors?cycle=2020-election-cycle&by=donorTypeCode", sep=""))
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
    as_tibble()
}

#scrap districts
candidates <- candidate_pages %>%
  mutate(district = map(links, get_district))

candidate_pages <- candidates %>%
  unnest(district) %>%
  rename(district = value)

#170 Districts (120 reps + 50 senators) so let's confirm we got all of them
check <- candidate_pages %>%
  select(links, district) %>%
  filter(grepl('District', district)) %>%
  mutate(district = str_trim(district, side = "both"))
#Only returning 162. Eight missing

#After investigating it looks like some members just aren't on the list on Transparency USA.
#Some aren't on TUSA altogether.

#Find missing reps ####
#I'll need to manually enter missing members. I could do a join that leaves only unmatched
house <-
  tibble(
  loc = "North Carolina House of Representatives District",
  numbers = 1:120
  )

house <- house %>%
  unite(district, loc:numbers, sep = " ", remove = TRUE)

senate <-
  tibble(
  loc = "North Carolina State Senate District",
  numbers = 1:50
  )

senate <- senate %>%
  unite(district, loc:numbers, sep=" ", remove = TRUE)

check_against <-
  bind_rows(house, senate)

check_against <-
  anti_join(check_against, check)

check_against <-
  check_against %>%
    mutate(links = c("https://www.transparencyusa.org/nc/candidate/shelly-willingham/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/m-jack-nichols/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/evelyn-terry/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/john-bradford-iii/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/kelly-alexander-jr/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/robert-hanig/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/ernestine-bazemore/donors?cycle=2020-election-cycle&by=donorTypeCode",
                     "https://www.transparencyusa.org/nc/candidate/donald-g-davis/donors?cycle=2020-election-cycle&by=donorTypeCode"
                     )
           )

check <- bind_rows(check, check_against)

# Get Value of All Donations ####

get_all_donations = function(links) {
  links <- read_html(links)

  links %>%
    html_nodes(".user-display-stat:nth-child(1) .user-display-stat-counter") %>%
    html_text() %>%
    as_tibble()
}

#scrap total donations per candidate
donations <- check %>%
  mutate(tot_donations = map(links, get_tot_donations))

congress <- donations %>%
  unnest(tot_donations)

congress <- congress %>%
  mutate(links = paste0(links, "&page="))

#Going to save this df here so I don't always have to wait for the webscrapping when I reopen project
write_csv(congress, "congress.csv")

output <- tibble()
for (i in 1:1) {
  message(paste0("Page ",i))
  links <- read_html(paste0(test$links,i))
  tables <- html_table(links)
 # tables <- tables %>% as_tibble() %>% mutate(candidate= html_nodes(".profile-title") %>%  html_text())
                     # Then we are getting every htmml attributes values into columns and rows
                     # it's a copy/past from stackoverflow, it's works don't ask me how.
  tables <- bind_rows(lapply(tables, function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  tables <- tables %>% mutate(candidates = as.character(links %>% html_nodes(".profile-title") %>% html_text()))

#  output <- bind_rows(output, tables)
}





#I need to learn how to loop the below function in order to get all donations no matter how many pages there are

#maybe I don't need a loop. maybe I just want to use as many numbers as I need like in the beginning of the code
#using this function to find out how many page nums I need
how_many = function(links) {
  links <- read_html(links)

  links %>%
    html_nodes(".page-link") %>%
    html_text()
}

test <- congress %>%
  mutate(tables = map(links, how_many))
##88 is the highest I see, let's go with that
##I want the function to run the link and paste the following until x <= 88


# Get donation tables ####

get_tables = function(links) {
  links <- read_html(links)

  links %>%
    html_table()
}

candidate_pages <- congress %>%
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







