# General Assembly of North Carolina
Exploring GA of NC members and their donors.


#Project Method
##Phase 1
scrapping_donors_v1.R scraps candidates and donor information from transparencyusa.org

Used a loop for the first time to create links to each donor page per candidate (#150). Wanted to have the loop create links for each candidate according to number in another column, but couldn't figure that out so I created a, uh, work around (#158).

##Phase 2
scrapping_terms.R scraps candidates terms and party from ballotpedia.org.

##Phase 3
cleaning_donors.R standardizes names for the scrapped donors. My primarily goals are to make sure each entity is represented only once and with a name that accurately portrays it's funding.



If their is only one uniform entity name that is generally understandable, i.e. not an acronym, I don't necessarily verify it's PAC or company status.

Verifying PACs on https://cf.ncsbe.gov/CFOrgLkup/ and https://www.fec.gov/data/committees/?committee_type=O&committee_type=V&committee_type=W



## Notes for later exploration
How many candidates/GA members donate to each other?
