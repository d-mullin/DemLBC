# Efficiently install multiple packages at once ---- 
# from https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, finalfit, here)

# the function p_load() from {pacman} checks to see if a package is installed, 
# if not it attempts to install the package and then loads it.

# read in data ----
# ensure date variables are recognised as such

consensus <- read_csv("raw_data/dem_consensus_outcomes.csv", 
  col_types = cols(consensus_meeting_date = col_date(format = "%d/%m/%Y"), 
  consensus_diagnosis_date = col_date(format = "%d/%m/%Y")))
                    
# inspect dataset ----
glimpse(consensus)

# inspect key variables ----
table(consensus$dementia) 
# All-cause Dementia coding (‘dementia’)
#0	No dementia
#1	Dementia
#2	Possible dementia
#3	MCI

table(consensus$dementia_sub)
#Dementia subtype coding (‘dementia_sub’)
#0 	No Dementia
#1	 Dementia Due to Alzheimer Disease
#2	Dementia Due to Cerebrovascular Disease (Vascular Dementia)
#3	Alzheimer Disease Dementia, Mixed Type, with Cerebrovascular Disease
#4	Dementia Due to Lewy Body Disease
#5	Frontotemporal Dementia
#6	Dementia Due to Psychoactive Substances Including Medications
#7	Dementia Due to Diseases Classified Elsewhere
#8	Dementia, Other Specified Cause
#9	Dementia, Unknown or Unspecified Cause
#NA		Possible dementia OR MCI – see “Consensus dementia diagnosis” 

table(consensus$dem_death_cert)
#Dementia on death certificate part I/II as at 14/03/2022 (‘dem_death_cert’)
#0	Dead, no dementia on death certificate
#1	Dead, dementia on death certificate
#NA          Alive

table(consensus$dem_imaging)
#Brain imaging results at consensus meeting (‘dem_imaging’)
#0	No imaging available
#1	CT head
#2	MRI
#3	Other – DAT/SPECT/PET

# read in full LBC dataset then full_join with consensus data----
lbcdata <- read_csv("~/Dropbox/Academic/PhD/LBC/mcr_project/MCRinLBC/MCRcombinedRecoded.csv")
nrow(lbcdata)
lbcconsensus <- full_join(lbcdata, consensus, by = "lbc36no")
glimpse(lbcconsensus)
nrow(lbcconsensus) # n = 1091
#subset wave 2-5 group as consent first requested at w2----
lbcconsensus <- lbcconsensus %>% 
  filter(!is.na(agedays_w2))  # filtering for rows where agedays_w2 is not missing)
# could have written this last line filter(agedays_w2 != "NA")
# Check it worked
nrow(lbcconsensus) # n = 866

# remove the one participant in wave 2 who did not consent
lbcconsensus <- lbcconsensus %>% 
  filter(lbc36no != "LBC360663")
nrow(lbcconsensus) # n = 865

# save processed and joined consensus LBC dataset----
write_csv(lbcconsensus, file = here::here("processed_data", "lbcconsensus.csv"))


