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
entity_donors <- replace(c(34:43,1249:1251), "Ahold Delhaize USA, Inc PAC")
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
entity_donors <- replace(c(142,944), "Committee to Elect Amos Quick")
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
entity_donors <- replace(504:508, "Canal Partners Media LLC")
entity_donors <- replace(c(512:517,534:564), "Carolina Asphalt Pavement Association PAC")
entity_donors <- replace(c(531:533,595:630), "Carolinas Association of General Contractors Inc NC PAC")
entity_donors <- replace(c(565:571,631), "Carolina Auto Recyclers PAC")
entity_donors <- replace(c(572:573,632:633,1057:1095,1154,1161:1180), "Credit Union PAC of NC")
entity_donors <- replace(c(574:576,839:841,1383), "Carolina Drive Chapter #1 State Account International Brotherhood of Teamsters")
entity_donors <- replace(c(582:590,594,658:692), "Carolina Link Telephone Cooperative PAC")
entity_donors <- replace(c(593,635:657), "Carolinas Ready Mixed Concrete Association NC PAC")
entity_donors <- replace(693, "Republican Women of Cary & Southwestern Wake")
entity_donors <- replace(c(695,5568:5580), "The Caterpillar Inc Employee PAC")
entity_donors <- replace(c(696:729,5991), "Centene Corporation PAC")
entity_donors <- replace(c(730:737,5581,5583), "Center for International Education Inc PAC")
entity_donors <- replace(738:748, "Central Carolina Radiologists for the Improvement of Medicine")
entity_donors <- replace(c(749:776, 5582), "Lumen Technologies Inc PAC")
entity_donors <- replace(777, "Chad Brown for Secretary of State")
entity_donors <- replace(778, "Committee to Elect Chalmers L. McDougald")
entity_donors <- replace(780:836, "Charter Communications NC PAC")
entity_donors <- replace(838, "Chatham County Democratic Party")
entity_donors <- replace(842, "Chris Humphrey Committee")
entity_donors <- replace(c(843:849,953), "Committee to Elect Christine Kushner")
entity_donors <- replace(850:885, "Cigna Corporation Employee PAC")
entity_donors <- replace(892, "Citizens for Destin Hall")
entity_donors <- replace(893, "City of Lumberton ABC Board")
entity_donors <- replace(864:898, "Committee to Elect Wayne Sasser")
entity_donors <- replace(902, "Coats Area Chamber of Commerce")
entity_donors <- replace(c(903:930,932), "Coca-Cola Consolidated Inc PAC")
entity_donors <- replace(931, "Committee to Elect Cody Henson")
entity_donors <- replace(c(941, 988:998), "Committee to Elect Republican Women")
entity_donors <- replace(942, "Committee to Elect Karl Gillespie")
entity_donors <- replace(948:949, "Committee to Elect Britt Moore")
entity_donors <- replace(c(957,1927), "Committee to Elect Garland Pierce")
entity_donors <- replace(c(958,1984:1987), "Committee to Elect Graig Meyer")
entity_donors <- replace(959:962, "Committee to Elect Greg Phipps Charlotte City Council District 4")
entity_donors <- replace(963:968, "Committee to Elect Holly Jones")
entity_donors <- replace(974:975, "Committee to Elect Larry Potts for NC House")
entity_donors <- replace(c(978:979,2664), "Committee to Elect Michele Presnell")
entity_donors <- replace(c(980,2668,5588), "Committee to Elect Mike Woodard")
entity_donors <- replace(c(1472:1502, 1506, 1569), "East Carolina Anesthesia PAC")
entity_donors <- replace(c(1904,2462), "Friends of Matt Hughes")
entity_donors <- replace(c(1944:1981, 5591), "GlaxoSmithKline LLC PAC")
entity_donors <- replace(c(2116:2135,2136:2137), "International Paper PAC")
entity_donors <- replace(2268, "Friends of Kevin Corbin")
entity_donors <- replace(2288, "Larry W Potts - NC House of Representatives")
entity_donors <- replace(2448, "Committee to Elect Mark Brody")
entity_donors <- replace(981:982, "Committee to Elect Nancy Hoffman")
entity_donors <- replace(c(984:986,4969), "Committee to Elect Pat B Hurley")
entity_donors <- replace(c(1006, 4982:4983), "Paul Lowe for NC Senate")
entity_donors <- replace(1017:1039, "Communication Workers of America - COPE PCC")
entity_donors <- replace(c(1041,1245:1246), "Conrad Committee for NC House")
entity_donors <- replace(1045:1049, "Corning Incorporated Employees PAC")
entity_donors <- replace(1052:1055, "Craven County Republican Men's Club")
entity_donors <- replace(1098:1153, "CSX Good Government Fund")
entity_donors <- replace(c(1155,1158:1159), "Cumberland County Homeowners & Taxpayers Association")
entity_donors <- replace(1185:1214, "CVS Health PAC")
entity_donors <- replace(1215:1221, "Cynthia for NC")
entity_donors <- replace(c(2143,1234), "Jackson for NC")
entity_donors <- replace(1235:1237, "Dave Craven for NC")
entity_donors <- replace(1239:1242, "Friends of David Ladley Swanson")
entity_donors <- replace(c(1244,2455), "Lucas for House Committee")
entity_donors <- replace(c(1255,1265:1277,1471), "Democratic Women of Mecklenburg County")
entity_donors <- replace(2606:2607, "Mecklenburg County Medical Society")
entity_donors <- replace(c(1256, 1295), "Democracy Engine Inc PAC")
entity_donors <- replace(1257, "Democratic Women of Sampson County")
entity_donors <- replace(c(1261:1264,1470), "Democratic Women of Guilford County")
entity_donors <- replace(1278:1279, "North Carolina Democractic Women")
entity_donors <- replace(1282:1284, "Democratic Women of Pitt County")
entity_donors <- replace(c(1285:1292,5871), "Democratic Women of Wake County")
entity_donors <- replace(c(1296,5590), "The Denis P Bilodeau Campaign Committee")
entity_donors <- replace(c(1298:1303, 5308), "Riddell for NC House 64")
entity_donors <- replace(1367, "Don Mial Committee to Elect")
entity_donors <- replace(1368, "Donna Lake for NC")
entity_donors <- replace(1369:1370, "Donna White for NC House")
entity_donors <- replace(1373, "Committee to Elect Donny Lambeth")
entity_donors <- replace(c(1376:1378,5183), "Rabon for Senate")
entity_donors <- replace(1382, "Drive - Democrat, Republican, Independent Voter Education")
entity_donors <- replace(1385:1468, "Duke Energy Corp PAC")
entity_donors <- replace(c(1469,1682:1685), "Employees of Dupont PAC - Dupont De Nemours Inc")
entity_donors <- replace(1503:1505, "East Carolina Republican Women")
entity_donors <- replace(1574, "East Carolina Emergency Physicians PAC")
entity_donors <- replace(c(1582:1594, 1596, 2322), "Eli Lilly and Company PAC")
entity_donors <- replace(c(1595, 2179), "Elmore for NC House")
entity_donors <- replace(c(1597,1677:1681, 1686:1738,5388:5405,5523:5524),
                         "State Employees Association of NC PAC")
entity_donors <- replace(c(1599:1600,1618:1663), "Emergeortho PA PAC")
entity_donors <- replace(c(1601:1617,3406:3411), "NC College of Emergency Physicians PAC")
entity_donors <- replace(1664:1676, "Emily's List")
entity_donors <- replace(1755:1790, "Enterprise Holdings Inc PAC")
entity_donors <- replace(1791, "Committee to Elect Erica McAdoo")
entity_donors <- replace(1792:1801, "Erie Indemnity Company PAC")
entity_donors <- replace(c(1814,2819:2823, 4433,4615,4618), "NC Academy of Family Physicians")
entity_donors <- replace(c(1817:1818, 1820:1829), "FedEx Corporation PAC")
entity_donors <- replace(c(1819,2463,2469:2601, 2605), "McGuireWoods Federal PAC Fund")
entity_donors <- replace(c(1831:1832, 1834:1836), "First Citizens Bank")
entity_donors <- replace(1837, "First Horizon Bank") #Reported as interest earned, not donation
entity_donors <- replace(c(1838, 2669), "Fitch for Senate Committee") #Reports from candidates don't match, so our government is ruined
entity_donors <- replace(1839:1845, "Flippable Federal PAC")
entity_donors <- replace(1846:1849, "McKissick for NC State")
entity_donors <- replace(1850, "First National Bank Corporation PAC")
entity_donors <- replace(1857, "Committee to Elect Frank Iler")
entity_donors <- replace(c(1866:1867,1873:1903), "Friends of Forestry PAC")
entity_donors <- replace(c(1908,2010:2025), "Friends to Elect Dr. Greg Murphy to Congress")
entity_donors <- replace(1909:1922, "Future Now Fund PAC")
entity_donors <- replace(1924:1926, "Gale Adcock for NC House")
entity_donors <- replace(1928:1929, "Gem Country Republican Women's Club")
entity_donors <- replace(1930:1942, "General Motors Company PAC")
entity_donors <- replace(1983, "Graham County Republican Party")
entity_donors <- replace(1991:1993, "Greater Greensboro Repubilican Women's Club")
entity_donors <- replace(1994:2008, "Greensboro Orthopaedics PAC")#This PAC closed in 2019 and I don't see it in the candidates receipts, so I am confused.
entity_donors <- replace(2026:2028, "Greg Newman for District Attorney")
entity_donors <- replace(2029:2030, "Committee to Elect Grey Mills")
entity_donors <- replace(2035, "Committee to Elect Harper Peterson")
entity_donors <- replace(2037, "Harry Brown for NC Senate")
entity_donors <- replace(2039:2059, "Health Network Solutions PAC")
entity_donors <- replace(c(2061:2062,2064), "Henderson County Republican Men's Club")
entity_donors <- replace(2063, "Henderson County Republican Women's Club")
entity_donors <- replace(2072, "Committee to Re-Elect Howard J. Hunter III Representative")
entity_donors <- replace(c(2073:2074, 5286:5299), "Hudson for Congress")
entity_donors <- replace(2075, "Hugh Blackwell for NC House")
entity_donors <- replace(c(2078:2109, 2140), "Independent Insurance Agents of NC")
entity_donors <- replace(c(2112:2113, 2138), "Integrated Strategy Group")






entity_donors <- replace(2191:2229, "JM Family Enterprince Inc PAC")
entity_donors <- replace(c(2711,4063:4093,4773:4774), "NC Outdoor Advertising Association PAC")
entity_donors <- replace(c(2741,2746:2747,2813,4563:4597), "National Federation of Independent Business NC PAC")
entity_donors <- replace(2859:2871, "NC Assisted Living Association PAC")
entity_donors <- replace(c(2811,2904:3027,3077,3094:3096,3506,4620,4622,5199:5209,5334:5352),
                         "NC Association of Electric Cooperatives Rural Electric Action Program")
entity_donors <- replace(4989:5014, "PepsiCo, Inc Concerned Citizens Fund")
entity_donors <- replace(5219:5253, "Resident Lenders of NC PAC")
entity_donors <- replace(5364:5366, "Sarah for NC")
entity_donors <- replace(5433:5442, "Smithfield Foods Inc PAC")
entity_donors <- replace(5534:5542, "Syngenta Corp Employee PAC")
entity_donors <- replace(6003:6051, "Wells Fargo and Company NC Employees Good Government Fund")
entity_donors <- replace(6059:6076, "Weyerhaeuser Company PAC")


entity_donors <- replace(c(), "")


entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 1128

entity_donors %>%
  group_by(donor_name) %>%
  mutate(count = n()) %>%
  select(donor_name, count, id) %>%
  distinct(donor_name, .keep_all = TRUE) %>%
  #filter(str_detect(donor_name, "^E")) %>%
  View()

( / )*100



#Absolutely cannot find the #110 Carolina PAC at PO BOX 368 YADKINVILLE	NC	27055
#or club for prosperity pac #901
#Best guess for Fd & Ma of NC Inc PAC at 1816 is Funeral Directors and Morticians Association of North Carolina, Inc but address is "wiseman mortuary"


