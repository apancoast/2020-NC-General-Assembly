# General Assembly of North Carolina
Exploring the 2020 General Assembly of NC members and their donors for that election phase.

# Project Method
## Phase 1 - Obtaining donors and donations
scrapping_donors_v1.R scraps candidates and donor information from transparencyusa.org.

Used a loop for the first time to create links to each donor page per candidate (Row #150). Wanted to have the loop create links for each candidate according to number in another column, but couldn't figure that out so I created a, uh, work around (#158).

## Phase 2 - Obtaining additional candidate information
scrapping_terms.R scraps candidates terms and party from ballotpedia.org.

## Phase 3 - Standardizing donor names
cleaning_donors.R standardizes names for the scrapped donors. My primarily goals are to make sure each entity is represented only once and with a name that accurately portrays it's funding.

If their is only one uniform entity name that is generally understandable, i.e. not an acronym, I don't necessarily verify it's PAC or company status.

Otherwise, like 90% of the entities, there is a variety of name for each entity. I first try to verify and find registered entity name via https://cf.ncsbe.gov/CFOrgLkup/, then https://www.fec.gov/data/committees/, and last resort I Google it. Another way to verify, is searching the receipts of the receiving candidates on the NCSBE site and using the address provided with the donor to compare to other PAC's registered addresses.

As often as possible, I try to provide the registered entity name in my standardize data. In select cases (Carolinas Association of General Contractors Inc NC PAC, for example), I opt to use a fuller, explanatory name rather than the registered acronym, as it providers more clarity to the reader.

NOTE: I was unable to find or verify existence of:
- Carolina PAC (ids 591:592) at PO BOX 368 YADKINVILLE	NC, 27055
- Seanlem PAC (5406)
- Vantiv (5833)
- Vantive Commerce/funds (5834)

cleaning_donors.R ends with a small bit of transformation to prepare the data for export to Tableua.

## Phase 4 - Tableau
entity_donors.csv and Shapefiles from https://ncleg.gov/Redistricting, Enacted 2019.


# Future Plans
## Explore
- Donor's industries
- Donors by county
- How many candidates/GA members donate to each other?
