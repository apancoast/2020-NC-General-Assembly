library(tidyverse)
library(sf)

# Exploring Donors ----
#let's get that table real purrrty
nc_ga_2020_elected <- read.csv("D:/RStudio/state_congress/CSVs/nc_ga_2020_elected.csv")
#laptop C:/Users/Ashley/Documents/RStudio/state_congress/CSVs/nc_ga_2020_elected.csv
#desktop D:/RStudio/state_congress/CSVs/nc_ga_2020_elected.csv
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
## 1:1000 ----
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
entity_donors <- replace(c(974:975,2288), "Committee to Elect Larry Potts for NC House")
entity_donors <- replace(c(978:979,2664), "Committee to Elect Michele Presnell")
entity_donors <- replace(c(980,2668,5588), "Committee to Elect Mike Woodard")

## 1001:2000 ----
entity_donors <- replace(c(1472:1502, 1506, 1569), "East Carolina Anesthesia PAC")
entity_donors <- replace(c(1816), "Funeral Directors and Morticians Association of North Carolina")#Best guess for Fd & Ma of NC Inc PAC at 1816 is Funeral Directors and Morticians Association of North Carolina, Inc but address is "wiseman mortuary." However, I think it's reasonable for a mortuary address to be the registered address for a mortician's PAC
entity_donors <- replace(c(1904,2462), "Friends of Matt Hughes")
entity_donors <- replace(c(1944:1981, 5591), "GlaxoSmithKline LLC PAC")
entity_donors <- replace(c(2116:2135,2136:2137), "International Paper PAC")
entity_donors <- replace(c(973, 2268), "Friends of Kevin Corbin")
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
entity_donors <- replace(c(2143,1234,2144), "Jackson for NC")
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
entity_donors <- replace(c(1296,5590), "The Denis P. Bilodeau Campaign Committee")
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
entity_donors <- replace(c(1597,1677:1681, 1686:1738,5388:5405,5521,5523:5524),
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

## 2009:3000 ----
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
entity_donors <- replace(2146:2147, "Citizens to Elect James D. Gailliard")
entity_donors <- replace(2149:2159, "Jason Saine Committee")
entity_donors <- replace(2160:2161, "Chaudhuri for New NC")
entity_donors <- replace(c(2162, 5851:5852), "Vote Jay Wagner")
entity_donors <- replace(2173:2178, "Jeff Jackson for NC Senate")
entity_donors <- replace(2180:2181, "Committee to Elect McNeely for NC House")
entity_donors <- replace(2182, "Jen Mangrum for NC")
entity_donors <- replace(c(2183,5614), "Jerry W. Tillman for NC Senate")
entity_donors <- replace(2184:2185, "Jim Burgin for Senate Committee")
entity_donors <- replace(2186, "Committee to Elect Jim Duke")
entity_donors <- replace(2187:2189, "Jim Perry Committee")
entity_donors <- replace(2191:2229, "JM Family Enterprises Inc PAC")
entity_donors <- replace(2231:2233, "John A. Fraley for NC House")
entity_donors <- replace(2234:2238, "John Bell Committee")
entity_donors <- replace(c(2248:2250, 2252:2253), "Johnston County Republican Women")
entity_donors <- replace(c(2251, 2254), "Johnston County Republican Party")
entity_donors <- replace(2255:2256, "Dollar for House")
entity_donors <- replace(2257:2266, "Julia C. Howard for House")
entity_donors <- replace(2267, "Elect Justin Davis")
entity_donors <- replace(2270:2285, "Koch Industries Inc PAC")
entity_donors <- replace(2289:2294, "Larry C. Strickland Campaign Committtee")
entity_donors <- replace(2297, "Lee County Republican Party")
entity_donors <- replace(c(2298,2300), "LGBTQ Democrats of NC")
entity_donors <- replace(c(2299,2301), "LGBTQ Democrats of Wake County")
entity_donors <- replace(2302:2309, "Liberty Mutual Insurance Company PAC")
entity_donors <- replace(2310:2321, "Lillian's List PAC")
entity_donors <- replace(c(2327, 4515:4528), "Nelson Mullins Riley & Scarborough LLP PAC")
entity_donors <- replace(2328:2333, "LKQ Corporation Employee Good Government Fund")
entity_donors <- replace(c(2336:2342, 2343:2360, 2365:2377), "Lowe's Comp Inc. PAC")
entity_donors <- replace(2361:2364, "Lower Cape Fear Republican Women")
entity_donors <- replace(2378:2379, "Lumberton Emergency Medical Associates")
entity_donors <- replace(2383:2407, "Mag Mutual NC PAC")
entity_donors <- replace(2409, "Making a Difference in Service to Our Nation PAC")
entity_donors <- replace(c(2411, 3854:3855, 4745:4759), "NC Manufactured & Modular Homebuilders Association PAC")
entity_donors <- replace(2413:2440, "Marathon Petroleum Corp Employee PAC")
entity_donors <- replace(2441:2447, "Marcia Morey Campaign")
entity_donors <- replace(2449, "Friends of Mark Robinson")
entity_donors <- replace(c(2450:2454, 2650:2659), "Metrolina Area Radiologists for Quality in Medicine PAC")
entity_donors <- replace(2456:2461, "Harrison for House")
entity_donors <- replace(2464:2467, "McCready for Congress")
entity_donors <- replace(c(2468, 5217), "McDowell County Republican Party")
entity_donors <- replace(c(2602:2603, 4971:4979), "McHenry for Congress")
entity_donors <- replace(2604, "McInnis for State Senate")
entity_donors <- replace(2608:2649, "Merck & Co., Inc Employees PAC")
entity_donors <- replace(2660, "Garrett for NC")
entity_donors <- replace(2665, "Mid-Atlantic Emergency Medical Associates PAC")
entity_donors <- replace(2667, "Mike Causey Campaign")
entity_donors <- replace(2673:2676, "Committee to Elect Mitchell Setzer")
entity_donors <- replace(c(2677, 2706:2707), "Mohammed for North Carolina")
entity_donors <- replace(2690:2704, "Motorola Solutions Inc PAC")
entity_donors <- replace(2705, "MS Consultants Inc PAC")
entity_donors <- replace(c(2708, 2824:2831, 4616:4617), "NC Academy of Physician Assistants")
entity_donors <- replace(c(2711,4063:4093,4773:4774), "NC Outdoor Advertising Association PAC")
entity_donors <- replace(c(2712, 4147:4154, 4780), "NC Poultry Federation PAC")
entity_donors <- replace(c(2713:2714,4375:4407), "NC Veterinary Medical Association PAC")
entity_donors <- replace(c(2716, 2815:2816, 3344:3345), "NC Chapter National Association of Social Workers PAC")
entity_donors <- replace(c(2717, 2740:2759,2812:2814,4458,4563:4598), "National Federation of Independent Business NC PAC")
entity_donors <- replace(2718, "Friends of Natasha Marcus")
entity_donors <-
  replace(c(2719, 2808:2810, 3030, 3078:3084,3098:3104, 4624),
          "NC Association of Insurance and Financial Advisors PAC")
entity_donors <- replace(c(2760, 4897:4902), "NRA Political Victory Fund")
entity_donors <- replace(2761:2807, "Nationwide Carolina Political Participation Fund")
entity_donors <-
  replace(c(2811,
            2904:3027,
            3076:3077,
            3094:3096,
            3506:3508,
            4620,
            4622,
            4733,
            5199:5209,
            5334:5352),
          "NC Association of Electric Cooperatives Rural Electric Action Program")
entity_donors <- replace(2817, "NC 12th Congressional District Republican Party")
entity_donors <- replace(c(2854,4312:4317), "NC State AFL-CIO Committee on Political Education")
entity_donors <- replace(2855, "NC Ambulatory Surgical PAC")
entity_donors <- replace(c(2856, 3969:4032, 4767:4768, 4434:4436, 4627), "NC American Nurses Association PAC")
entity_donors <- replace(c(2857:2858, 3073,3090, 3114:3116, 4437:4455, 4619, 4627:4633), "NC Association of Private Investigators PAC")
entity_donors <- replace(2859:2871, "NC Assisted Living Association PAC")
entity_donors <- replace(c(2872:2874, 3074,3091:3092,3105, 4457), "NC Association Long Term Care Facilities")
entity_donors <- replace(c(2875:2903, 3093, 3412:3414,3322,4408), "NC Associates of CPAs PAC")

## 3028:4000 ----
entity_donors <- replace(c(3028:3029,3097,4621,4623), "NC Association of Health Underwriters PAC")
entity_donors <- replace(c(3031:3072, 3075, 3085:3089, 3106:3113, 3966,4625:4626), "NC Association of Nurse Anesthetists")
entity_donors <- replace(c(3212:3225,4640), "NC Bail Agents Association PAC")
entity_donors <- replace(c(3226:3235,4641:4671), "NC Bankers Association PAC")
entity_donors <- replace(3236:3314, "NC Beer & Wine Wholesalers Association PAC")
entity_donors <- replace(c(3323:3343,4672:4673), "NC Chamber PAC")
entity_donors <- replace(c(3346:3377,4674:4675), "NC Chiropractic Association Inc PAC")
entity_donors <- replace(3378:3405, "NC Clean Energy Business Alliance PAC")
entity_donors <- replace(c(3415:3420,4729,4676:4721), "NC Democratic Party")
entity_donors <- replace(c(3421:3494,4730:4732), "NC Dental Society PAC")
entity_donors <- replace(c(3496:3505), "NC Dermatology Association PAC")
entity_donors <- replace(c(3509:3578,4734), "NC Farm Bureau Federation Inc PAC")
entity_donors <- replace(c(3586,4782:4787), "North Carolina Republican House Caucus")
entity_donors <- replace(c(3587:3644), "NC Health Care Facilities Association PAC")
entity_donors <- replace(c(3781:3834, 4735:4736), "NC Hospital Association PAC")
entity_donors <- replace(c(3851:3852), "NC Land Title Association PAC")
entity_donors <- replace(c(3856:3893,4760:4763, 4770),
                         "NC Medical Society State PAC")
entity_donors <- replace(c(3895:3961,4277:4278,4459,4764), "NC Merchants PAC Inc")
entity_donors <- replace(3962:3963, "NC National Organization for Women")
entity_donors <- replace(3965, "NC Nurses PAC")
entity_donors <- replace(c(3964,3967,4765:4766), "NC Nurse Midwives PAC")


## 4033:5000 ----
entity_donors <- replace(c(4033:4034, 4908:4909), "NC Ob-Gyn Society PAC")
entity_donors <- replace(c(4035, 4319:4372, 4769, 4842:4852), "NC State Optometric Society PAC")
entity_donors <- replace(c(4036:4062, 4771:4772), "NC Orthopaedic Association PAC")
entity_donors <- replace(c(4094:4098, 5016:5035), "NC Petroleum & Convenience Marketers PAC")
entity_donors <- replace(c(4105:4146, 4776:4779), "NC Pork Council PAC")
entity_donors <- replace(4155:4256, "NC Association of Realtors PAC")
entity_donors <- replace(c(4257, 4788:4817, 4456), "North Carolina Republican Party")
entity_donors <- replace(c(4258:4276, 4820), "NC Restaurant & Lodging Association PAC")
entity_donors <- replace(4279:4287, "NC Senate Democratic Caucus")
entity_donors <- replace(c(4298:4310, 4838:4841), "NC Society of Eye Physicians & Surgeons PAC")
entity_donors <- replace(c(4311, 4509:4510), "NC Society of Surveyors PAC")
entity_donors <- replace(c(4373:4374, 4853), "NC Trucking Association Committee for Good Government")
entity_donors <- replace(c(4409:4432, 4634), "NC Association of Educators")
entity_donors <- replace(c(4460:4461, 4781), "NC Propane Gas Association")
entity_donors <- replace(c(4462:4508, 4903), "NC Association of Student Financial Aid Administrators PAC")
entity_donors <- replace(4511, "NC Utility Contractors Association PAC")
entity_donors <- replace(c(2713:2714, 4375:4407, 4512), "NC Veterinary Medical Association")
entity_donors <- replace(c(4513:4514), "NEA Fund for Children and Public Education PAC")
entity_donors <- replace(4534:4562, "Nexsen Pruet NC PAC")
entity_donors <- replace(c(4600, 4875:4887), "Novartis Corp PAC")
entity_donors <- replace(4601:4614, "Norfolk Southern Corporation Good Government Fund")
entity_donors <- replace(4722:4728, "NC House Democratic Caucus")
entity_donors <- replace(c(4775, 5452:5513), "Southern States Police Benevolent Association PAC")
entity_donors <- replace(4821:4837, "NC Senate Majority Fund")
entity_donors <- replace(4818:4819, "NC Senate Republican Caucus")
entity_donors <- replace(4854:4855, "NC Vending Association PAC")
entity_donors <- replace(4889:4892, "Now or Never NC")
entity_donors <- replace(4893:4896, "Nurse Practitioner PAC")
entity_donors <- replace(4904:4905, "Nucor Corporation PAC")
entity_donors <- replace(4916:4949, "Orthocarolina PA Federal PAC")
entity_donors <- replace(4950:4952, "Our States Matter PAC")
entity_donors <- replace(4961:4968, "Partners for Educational Freedom PAC")
entity_donors <- replace(4980, "Kinsey Campaign Committee")
entity_donors <- replace(4985, "Penny Rich for County Commissioner")
entity_donors <- replace(4989:5014, "PepsiCo, Inc Concerned Citizens Fund")

## 5036:6122 ----
entity_donors <- replace(5036:5054, "Pfizer Inc PAC")
entity_donors <- replace(5056, "Philip E. Berger Committee")
entity_donors <- replace(5064:5082, "Piedmont Traid Anesthesia PA Federal PAC")
entity_donors <- replace(5083:5086, "Pill PAC (Formally NC Association of Pharmacists)")
entity_donors <- replace(5087, "Pinehurst Resort")
entity_donors <- replace(5091:5115, "Planned Parenthood Action PAC NC")
entity_donors <- replace(5116:5117, "Polk County Republican Party")
entity_donors <- replace(5118, "Precinct 206 Democrats - Mecklenburg County")
entity_donors <- replace(5119:5120, "Piedmont Radiologists Interested in Medical Excellence")
entity_donors <- replace(5123:5164, "Providence Anesthesiology Associates PA Federal PAC")
entity_donors <- replace(c(1314:1366,5165:5167), "Dominion Energy Inc PAC")
entity_donors <- replace(5168:5181, "Publix Supermarkets Inc Association PAC")
entity_donors <- replace(5193, "Ralph Hise for NC Senate")
entity_donors <- replace(5194:5196, "Ramey Kemp & Associates PAC")
entity_donors <- replace(5198, "Committee to Re-Elect Gary Banks Sheriff")
entity_donors <- replace(5210:5213, "Regional Anesthesia PLLC PAC")
entity_donors <- replace(5215, "Citizens to Elect	Rena Turner")
entity_donors <- replace(5219:5253, "Resident Lenders of NC PAC")
entity_donors <- replace(c(5254:5284, 5285), "Reynolds American Inc PAC")
entity_donors <- replace(5301, "Gunn for NC Senate")
entity_donors <- replace(c(5304, 5306:5307), "Ricky Buchanan for Sheriff")
entity_donors <- replace(5309:5313, "Right 2 Vape PAC")
entity_donors <- replace(5321, "Roberson for Mayor Committee")
entity_donors <- replace(5324:5325, "Rock Holdings Inc State PAC")
entity_donors <- replace(5330, "Ronnie for NC")
entity_donors <- replace(c(5353,5357), "Sam's Club")
entity_donors <- replace(5354, "Sampson County Republican Party")
entity_donors <- replace(5358:5363, "Sandhills Anesthesiologists PAC")
entity_donors <- replace(5364:5366, "Sarah for NC")
entity_donors <- replace(5377:5386, "Scott Cooper for Congress")
entity_donors <- replace(5407, "Service Employees International Union")
entity_donors <- replace(5411, "Singh Development PAC")
entity_donors <- replace(5417, "Smart TD PAC")
entity_donors <- replace(5433:5442, "Smithfield Foods Inc PAC")
entity_donors <- replace(5444, "Southeast Republican Men's Association")
entity_donors <- replace(5445:5448, "Southeastern Radiology Organization PAC")
entity_donors <- replace(5449, "Southern Crop Production Association PAC")
entity_donors <- replace(5516:5520, "SRH Media Inc")
entity_donors <- replace(c(5527,5532), "Stokes County Republican Ladies")
entity_donors <- replace(5528:5529, "Stokes County Republican Men's Club")
entity_donors <- replace(5534:5542, "Syngenta Corp Employee PAC")
entity_donors <- replace(5543:5548, "Tar Heel Prosperity PAC")
entity_donors <- replace(5549:5552, "Taylor Morrison, Building Strong Business PAC")
entity_donors <- replace(5553:5559, "Ted Budd for Congress")
entity_donors <- replace(5561, "Terry Bradley Campaign Committee")
entity_donors <- replace(5597, "The UPS Store")
entity_donors <- replace(c(5598:5611, 6083:6086), "The Williams Companies Inc PAC")
entity_donors <- replace(5612:5613, "Committee to Elect Thomas S. Hester NC Senator")
entity_donors <- replace(5615:5641, "Friends of Tim Moore")
entity_donors <- replace(5647:5652, "Triad Radiologists Interested in Advancing Disease Detection PAC")
entity_donors <- replace(5653:5657, "Triangle Apartment Association PAC")
entity_donors <- replace(c(5659:5703,5707), "Truist NC PAC")
entity_donors <- replace(5704:5705, "Truliant Federal Credit Union PAC")
entity_donors <- replace(c(5709, 5711:5721), "UCB PAC")
entity_donors <- replace(c(5722,5776:5808), "University Development Coalition")
entity_donors <- replace(c(5723:5726, 5771:5775), "Universal Leaf Tobacco Company Inc PAC")
entity_donors <- replace(c(5727,5730:5770), "UnitedHealth Group Inc PAC")
entity_donors <- replace(c(5809:5810, 5813:5826), "US Acute Care Solutions PAC")
entity_donors <- replace(5827:5828, "USPS")
entity_donors <- replace(5829:5830, "Foushee for NC")
entity_donors <- replace(5835:5843, "Verizon Good Government Club of NC")
entity_donors <- replace(5844:5847, "Verla Insko for State House")
entity_donors <- replace(5848, "Committee to Elect Vickie Sawyer")
entity_donors <- replace(5853:5854, "Vote Tripling PAC")
entity_donors <- replace(5861:5869, "Vulcan Material Company PAC")
entity_donors <- replace(5872:5901, "Wake Emergency Physicians PA PAC")
entity_donors <- replace(c(5903:5925, 5931:5932), "Wal-Mart Stores Inc PAC for Responsible Government") #Also an assortment of individual wal-mart stores
entity_donors <- replace(5933:5969, "Ward and Smith PAC")
entity_donors <- replace(5970, "Warren Daniel for NC State")
entity_donors <- replace(5971:5982, "Waste Management Employees Better Government Fund")
entity_donors <- replace(5983:5984, "Watauga County Republican Women's Club")
entity_donors <- replace(5990, "WBTV - Gray Television")
entity_donors <- replace(6003:6051, "Wells Fargo and Company NC Employees Good Government Fund")
entity_donors <- replace(6053:6058, "Western Radiologists and Surgeons PAC")
entity_donors <- replace(6059:6076, "Weyerhaeuser Company PAC")
entity_donors <- replace(6078, "Whit Davis for Judge")
entity_donors <- replace(6079:6082, "Wiley Nickel for NC")
entity_donors <- replace(6087:6094, "Wilmington Anesthesiology PAC")
entity_donors <- replace(6095:6102, "Windstream Corp PAC")
entity_donors <- replace(6109:6113, "Woody White for New Hanover County Commissioner")
entity_donors <- replace(6119, "WSP USA Inc PAC")

## Misc for cleaning donors ----

entity_donors %>%
  distinct(donor_name) %>%
  count()
#result 729

entity_donors %>%
  group_by(donor_name) %>%
  mutate(count = n()) %>%
  select(donor_name, count, id) %>%
  distinct(donor_name, .keep_all = TRUE) %>%
  View()

# Could not find or verify existence of:
# Carolina PAC (ids 591:592) at PO BOX 368 YADKINVILLE	NC, 27055
# Seanlem PAC (id 5406)
# Vantiv (5833) and Vantive Commerce/funds (5834)


# Clean Amounts ----
# Amount discrepancies were introduced by candidate's campaigns recording receipts without standardized names
entity_donors <- entity_donors %>%
  group_by(district, member, donor_name) %>%
  summarise(tot_from_donor = sum(tot_from_donor))

donors <-
  donors %>%
  distinct(district, member, party, tot_to_candidate, counties, term_start)

entity_donors <- full_join(entity_donors, donors, "member") %>%
  select(district.x, member, donor_name, tot_from_donor, tot_to_candidate, party, counties, term_start) %>%
  rename(seat_won = district.x)

entity_donors <- entity_donors %>%
  mutate(seat_won = gsub("North Carolina House of Representatives", "Representative", seat_won),
         seat_won = gsub("North Carolina State Senate", "Senator", seat_won))

#Prep for SHP Files ----
reps <- entity_donors %>%
  filter(grepl("Representative", seat_won)) %>%
  mutate(house_district = str_sub(seat_won, 25, 27)
  ) %>%
  select(member, house_district)

senate <- entity_donors %>%
  filter(grepl("Senator", seat_won)) %>%
  mutate(senate_district = str_sub(seat_won, 18, 21)
  ) %>%
  select(member, senate_district)

entity_donors <- entity_donors %>% #Dude, idk why all types of joins are screwing me so using distinct as a quick fix rn
  left_join(reps, "member") %>%
  left_join(senate, "member") %>%
  distinct(member, donor_name, .keep_all = TRUE)

write.csv(entity_donors, "D:/RStudio/state_congress/CSVs/entity_donors.csv")

#Adding missing districts in excel because... I'm in a hurry?
