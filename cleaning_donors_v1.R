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
replace <- function(rows, name) {
  slice <- entity_donors %>%
    slice({{rows}}) %>%
    mutate(donor_name={{name}})

   entity_donors %>%
    rows_update(slice, by = c("id"))

}

entity_donors <- entity_donors %>%
  mutate(donor_name=
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
           "Asd",
           "ASD")
  )



# Row Updates ----
entity_donors <- replace(1:6, "10th Congressional District Republican Party")
entity_donors <- replace(8:12, "3M Co PAC")
entity_donors <- replace(13:14, "3rd Congressional District Republican Party")
entity_donors <- replace(c(15,176:193), "Apartment Association of North Carolina")
entity_donors <- replace(c(16:27,105:107), "American Council of Engineering Companies")
entity_donors <- replace(c(28:29,312:313), "Auto Care Association's PAC")
entity_donors <- replace(30:31, "ActBlue")
entity_donors <- replace(32, "ADP as Depository for Taxes")
entity_donors <- replace(34:43, "Ahold Delhaize USA, Inc PAC")
entity_donors <- replace(45:46, "Alamance County of Republican Party Building Fund")
entity_donors <- replace(47:48, "Alamance Republican Women")
entity_donors <- replace(49, "Alexander County Democratic Executive Committee")
entity_donors <- replace(50, "Alexander County Republican Party")
entity_donors <- replace(52:53, "Alleghany County Republican Party")
entity_donors <- replace(61:71, "Allstate Insurance Comp PAC")
entity_donors <- replace(75:78, "Amazon")
entity_donors <- replace(79:97, "American Airlines Inc PAC")
entity_donors <- replace(98:104, "American Anesthesiology of NC PAC")
entity_donors <- replace(108:116, "American Federation of State County and Municipal Employees (AFSCME-PEOPLE)")
entity_donors <- replace(117:124, "American Kennel Club PAC")
entity_donors <- replace(c(199:201,4099:4104), "American Physical Therapy Association NC PAC")
entity_donors <- replace(c(129:141, 2111), "American Property Casualty Insurance Association PAC")
entity_donors <- replace(144:157, "Anesthesiologists of the Triad PAC")
entity_donors <- replace(158:159, "Anson County Republican Party")
entity_donors <- replace(160:175, "Anthem Inc PAC")
entity_donors <- replace(203, "Aristotle PAC")
entity_donors <- replace(209:229, "Astellas US LLC PAC")
entity_donors <- replace(c(230:278,310), "AT&T NC PAC")
entity_donors <- replace(c(314, 3117:3211, 4635:4639), "NC Auto Dealers Accociation PAC")
entity_donors <- replace(315:317, "IATSE 491 Back to One PAC")
entity_donors <- replace(318:325, "Bank of America Corp State and Federal PAC")
entity_donors <- replace(328:341, "Bayada Home Health Care Inc NC PAC")
entity_donors <- replace(342:354, "Bayer US LLC PAC")
entity_donors <- replace(355:360, "BB&T Corp")
entity_donors <- replace(c(361:363,382:445,5566:5567), "Blue Cross & Blue Shield of NC Employees PAC") #Carla D. Cunningham listed BCBS as Blue PAC. I did some hunting to confirm it had the same mailing address as BCBS's PAC, and was not ActBlue, Team Blue, etc.
entity_donors <- replace(364:365, "Beasley Media Group")
entity_donors <- replace(367:369, "Committee to Re-elect Becky Carney")
entity_donors <- replace(c(379:380,1576:1577), "Elect BJ Barnes")
entity_donors <- replace(447:449, "Elect Bob Steinburg")
entity_donors <- replace(450, "Bobbie Richardson for NC House")
entity_donors <- replace(455:457, "Committee to Elect Brenden Jones")
entity_donors <- replace(458, "Brent Jackson for NC Senate")
entity_donors <- replace(459, "Brian Farkas for NC House")
entity_donors <- replace(461:467, "Brian Turner for NC House")
entity_donors <- replace(c(468:470, 5218), "Brightspring Legacy Fund")
entity_donors <- replace(471:487, "Bristol-Myers Squibb Company Employee PAC")
entity_donors <- replace(c(460,491), "Bryan Rauers for County Commissioner")
entity_donors <- replace(c(492:494,2709:2710,3317:3321,3646:3780,4737:4744,5595), "NC Home Builders Association PAC")
entity_donors <- replace(c(495, 2163:2172), "Butterfield for Congress")
entity_donors <- replace(501, "Camden County Republican Party")
entity_donors <- replace(503, "Camping World")
entity_donors <- replace(c(1678:1681,1686:1737,5405,5523:5524), "Employee's PAC")

entity_donors <- replace(c(1472:1502, 1506, 1569), "East Carolina Anesthesia PAC")

entity_donors <- replace(c(1944:1981, 5591), "GlaxoSmithKline LLC PAC")
entity_donors <- replace(c(2116:2135,2136:2137), "International Paper PAC")
entity_donors <- replace(c(2711,4063:4093,4773:4774), "NC Outdoor Advertising Association PAC")
entity_donors <- replace(c(2741,2746:2747,2813,4563:4597), "National Federation of Independent Business NC PAC")

entity_donors <- replace(2859:2871, "NC Assisted Living Association PAC")
entity_donors <- replace(c(2811,2904:3027,3077,3094:3096,3506,4620,4622,5199:5209,5334:5352), "NC Association of Electric Copperatives Rural Electric Action Program")

entity_donors <- replace(4989:5014, "PepsiCo, Inc Concerned Citizens Fund")
entity_donors <- replace(5219:5253, "Resident Lenders of NC PAC")
entity_donors <- replace(5433:5442, "Smithfield Foods Inc PAC")
entity_donors <- replace(5534:5542, "Syngenta Corp Employee PAC")
entity_donors <- replace(6003:6051, "Wells Fargo and Company NC Employees Good Government Fund")
entity_donors <- replace(6059:6076, "Weyerhaeuser Company PAC")


entity_donors <- replace(c( ), " ")


entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 1393

entity_donors %>%
  distinct(donor_name) %>%
  View()









