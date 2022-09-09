library(tidyverse)
library(rvest)
library(data.table)

#### STATE HOUSE REPS ####

##Scrapping NC state house rep terms
link = "https://ballotpedia.org/North_Carolina_House_of_Representatives"
page = read_html(link)

house_district = page %>% html_nodes("#officeholder-table td:nth-child(1) a") %>% html_text()
rep = page %>% html_nodes("#officeholder-table td:nth-child(2)") %>% html_text()
term_start = page %>% html_nodes("#officeholder-table td:nth-child(4)") %>% html_text()

nc_state_house = data.frame(house_district, rep, term_start, stringsAsFactors = FALSE)
write.csv(nc_state_house, "nc_state_house.csv")

#exported from the State's website
state_house <- fread("D:/RStudio/pride_month/nc_state_house_reps.csv")

#transform and select
house_terms <-
  nc_state_house %>% 
  mutate(across(everything(), gsub, pattern = "North Carolina House of Representatives District ", replacement = "")) %>% 
  select(house_district,term_start)

#join
merged <- full_join(house_reps,house_terms,by="house_district")

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

#Now I want just Cape Fear. I'm going to use a handy library
library(stringr)
cfr_reps <- 
  nc_leg %>% 
    filter(str_detect(counties_represented, "Brunswick|Columbus|Pender|New Hanover"))

#looks like our longest standing rep has been there since '09, but more started in '17
##I'll take that info to dig up a view bill votes to look at for the next section
##Also going to make reopening this data easier with:

write.csv(nc_leg, "nc_state_legislators_v1.csv")

#### THE BILLS WITH VOTES ####
#Coming back I need to reopen:
nc_senators <- fread("D:/RStudio/pride_month/nc_state_senate_v1.csv")
nc_house <- fread("D:/RStudio/pride_month/nc_state_house_v2.csv")
nc_leg <- bind_rows(nc_senators, nc_house)

##2017 HB142 that became SL2017-4, meant to "repeal" the 2016 HB2
link = "https://www.ncleg.gov/Legislation/Votes/RollCallVoteTranscript/2017/H/144"
page = read_html(link)

bill = "HB142_2017"
ayes = page %>% html_nodes(".mt-3:nth-child(2) .ncga-row-no-gutters .col-12+ .col-12") %>% html_text()
noes = page %>% html_nodes(".mt-3:nth-child(3) .ncga-row-no-gutters .col-12+ .col-12") %>% html_text()
excused_absence = page %>% html_nodes(".mt-3~ .mt-3+ .mt-3 .ncga-row-no-gutters .col-12+ .col-12") %>% html_text()
sponsors = "Stevens, Jordan, Floyd"

hb142_2017 = data.frame(bill, sponsors, ayes, noes, excused_absence, stringsAsFactors = FALSE)
write.csv(hb142_2017, "hb142_2017.csv")

##NC's Don't Say Gay ver https://www.ncleg.gov/BillLookup/2021/H755 in house rn
link = "https://www.ncleg.gov/Legislation/Votes/RollCallVoteTranscript/2021/S/563"
page = read_html(link)

bill = "HB755_2021"
ayes = page %>% html_nodes(".mt-3:nth-child(2) .ncga-row-no-gutters .col-12+ .col-12") %>% html_text()
noes = page %>% html_nodes(".mt-3:nth-child(3) .ncga-row-no-gutters .col-12+ .col-12") %>% html_text()
excused_absence = page %>% html_nodes(".mt-3~ .mt-3+ .mt-3 .ncga-row-no-gutters .col-12+ .col-12") %>% html_text()
sponsors = "Blackwell; Torbett; Hardister; Elmore; Clampitt; Goodwin; Moss; Penny; Potts; Riddell; Strickland; Wheatley"

hb755_2021 = data.frame(bill, sponsors, ayes, noes, excused_absence, stringsAsFactors = FALSE)
write.csv(hb755_2021, "hb755_2021.csv")

#### THE BILLS ONLY SPONSORS ####

##"Youth Health protection act"Senate Bill 514 would also compel state employees to immediately notify parents #in writing if their child displays “gender nonconformity” or expresses a desire to be treated in a way that is #incompatible with the gender they were assigned at birth." https://www.nbcnews.com/feature/nbc-out/n-c-bill-would-ban-treatment-trans-people-under-21-n1263146

#No votes, but could list sponsors: https://www.ncleg.gov/BillLookUp/2021/S514

#House Bill 449 bars the panic defense, while HB 450 expands nondiscrimination protections for all LGBTQ people in North Carolina. House Bill 451 would completely do away with the bathroom bill passed in 2016, and HB 452 would prohibit conversion therapy. https://apnews.com/article/legislature-bills-north-carolina-gender-identity-4584f64772d1bd0f168b93821fa0880b
