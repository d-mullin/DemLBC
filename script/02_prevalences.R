# calculate prevalences---- 
# load processed data----
# note: lbcconsensus data is: 
# 1. merged dem consensus meeting data + my whole lbc data for MCR project 
#     (not all variables)
# 2. filtered for only those who consented to record linkage - ie wave 2 
#     onwards except for one participant who was filtered out

lbcconsensus <- read_csv(here::here("processed_data", "lbcconsensus.csv"))
glimpse(lbcconsensus)

# inspect key variables ----
table(lbcconsensus$dementia) 
# All-cause Dementia coding (‘dementia’)
#0	No dementia
#1	Dementia
#2	Possible dementia
#3	MCI

table(lbcconsensus$dementia_sub)
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

table(lbcconsensus$dem_death_cert)
#Dementia on death certificate part I/II as at 14/03/2022 (‘dem_death_cert’)
#0	Dead, no dementia on death certificate
#1	Dead, dementia on death certificate
#NA          Alive

table(lbcconsensus$dem_imaging)
#Brain imaging results at consensus meeting (‘dem_imaging’)
#0	No imaging available
#1	CT head
#2	MRI
#3	Other – DAT/SPECT/PET

#Prevalence and CIs Dement consensus----
# Probable all-cause dementia (n=118)
binom.test(118, 865, 
0.5,
alternative = "two.sided", 
conf.level = 0.95)

# Probable all-cause dementia (n=118)
binom.test(118, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)

#1	 Dementia Due to Alzheimer Disease
binom_test(58, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#2	Dementia Due to Cerebrovascular Disease (Vascular Dementia)
binom_test(10, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#3	Alzheimer Disease Dementia, Mixed Type, with Cerebrovascular Disease
binom_test(20, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#4	Dementia Due to Lewy Body Disease
binom_test(4, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#5	Frontotemporal Dementia
# none

#6	Dementia Due to Psychoactive Substances Including Medications
binom_test(2, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#7	Dementia Due to Diseases Classified Elsewhere
binom_test(5, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#8	Dementia, Other Specified Cause
# none

#9	Dementia, Unknown or Unspecified Cause
binom_test(19, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)

#Proportion of each subtype and CIs----

# Probable all-cause dementia (n=118)


#1	 Dementia Due to Alzheimer Disease
binom_test(58, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#2	Dementia Due to Cerebrovascular Disease (Vascular Dementia)
binom_test(10, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#3	Alzheimer Disease Dementia, Mixed Type, with Cerebrovascular Disease
binom_test(20, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#4	Dementia Due to Lewy Body Disease
binom_test(4, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#5	Frontotemporal Dementia
# none

#6	Dementia Due to Psychoactive Substances Including Medications
binom_test(2, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#7	Dementia Due to Diseases Classified Elsewhere
binom_test(5, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
#8	Dementia, Other Specified Cause
# none

#9	Dementia, Unknown or Unspecified Cause
binom_test(19, 118, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)
