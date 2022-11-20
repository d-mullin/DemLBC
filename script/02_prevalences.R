# Author ----
# Donncha Mullin
# 27th Sept 2022
# Main goal of this script is to calculate dementia (and subtype) prevlances in LBC ----


# install.packages("stargazer") # summary stats about variables (incl histograms)
# install.packages("skimr") # publication quality tables

library(pacman)
pacman::p_load(tidyverse, finalfit, here, rstatix, skimr, stargazer)


# load pre-processed data----
# note: lbcconsensus data is: 
# 1. merged dem_consensus_outcomes data + my whole lbc data for MCR project + death variables - this preprocessing is in script 01_cleaning_combining
# 2. filtered for only those who consented to record linkage - ie wave 2
#     onwards plus one w2 participant who was filtered out as did not consent

lbcconsensus <- read_csv(here::here("processed_data", "lbcconsensus.csv"))
glimpse(lbcconsensus)
nrow(lbcconsensus) # n = 865

# Inspect key variables ----
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

summary(lbcconsensus$ageyrs_death)

# keep only probable dementia yes or no outcomes ----
# makes the 7 possible dementias and the 13 MCI cases a 'no dementia'. 
lbcconsensus <- lbcconsensus %>% 
  mutate(
    dementia_yesno = case_when(dementia == 1 ~ 1,
                               dementia == 0 | 2 | 3 ~ 0,
                               dementia == NA ~ NA_real_)
  )
table(lbcconsensus$dementia_yesno)# 118 

# Self-reported Vs ascertained dementia at w2-5 ----
# overview of each wave's status
lbcconsensus %>%  count(dement_w2, dementia_yesno)
lbcconsensus %>%  count(dement_w3, dementia_yesno)
lbcconsensus %>%  count(dement_w4, dementia_yesno)
lbcconsensus %>%  count(dementia_w5, dementia_yesno)

## create self-reported dem at any wave variable ----
lbcconsensus <- lbcconsensus %>% 
  mutate(
    dementia_selfanywave = case_when(dement_w2 | dement_w3 | dement_w4 | dementia_w5 == 1 ~ 1,
                                     dement_w2 | dement_w3 | dement_w4 | dementia_w5 == 0  ~ 0,
                                     dement_w2 | dement_w3 | dement_w4 | dementia_w5 == NA ~ NA_real_)
  )
table(lbcconsensus$dementia_selfanywave) # 118 

## overall count of self-rep Dem at any stage Vs Dem ascertained (which is always at any stage)----
lbcconsensus %>%  count(dementia_selfanywave, dementia_yesno)

## Filter dataset to keep key variables ----
self_DA_data <- lbcconsensus %>%
  select(lbc36no, dementia_yesno, consensus_diagnosis_date, dement_w1, dement_w2, dement_w3, dement_w4, dementia_w5)
glimpse(self_DA_data)

write_csv(self_DA_data, file = here::here("processed_data", "self_DA_data.csv"))

self_DA_data <- self_DA_data %>% 
  mutate(
    da2010 = if_else(consensus_diagnosis_date >= 2006/01/01 & consensus_diagnosis_date <= 2010/12/12 ~ 1, 0,))

help("if_else")

## subset by DA date range ----
# Work out who had dementia ascertained at the end of each wave - so, end of years 2010, 2013, 2017, 2019
# first, ensure consensus_diagnosis_date is recognised as a date rather than a character
library(lubridate)
as.Date(lbcconsensus$consensus_diagnosis_date, "%d/%m/%Y")

lbcconsensus <-
  lbcconsensus %>%  
  mutate(dementia_ascert_year = case_when(
    consensus_diagnosis_date >= "01/01/2006" & consensus_diagnosis_date <= "12/12/2010" ~ "DA by 2010", consensus_diagnosis_date >= "01/01/2011" & consensus_diagnosis_date <= "12/12/2013" ~ "DA by 2013",
    consensus_diagnosis_date >= "01/01/2014" & consensus_diagnosis_date <= "12/12/2017" ~ "DA by 2017",
    consensus_diagnosis_date >= "01/01/2018" & consensus_diagnosis_date <= "12/12/2019" ~ "DA by 2019"))

# change date order
lbcconsensus <-
  lbcconsensus %>%  
  mutate(dementia_ascert_year = case_when(
    consensus_diagnosis_date >= "2006/01/01" & consensus_diagnosis_date <= "2010/12/12" ~ "DA by 2010", consensus_diagnosis_date >= "2011/01/01" & consensus_diagnosis_date <= "2013/12/12" ~ "DA by 2013",
    consensus_diagnosis_date >= "2014/01/01" & consensus_diagnosis_date <= "2017/12/12" ~ "DA by 2017",
    consensus_diagnosis_date >= "2018/01/01" & consensus_diagnosis_date <= "2019/12/12" ~ "DA by 2019"))

# remove quotations
lbcconsensus <-
  lbcconsensus %>%  
  mutate(dementia_ascert_year = case_when(
    consensus_diagnosis_date >= 2006/01/01 & consensus_diagnosis_date <= 2010/12/12 ~ "DA by 2010", consensus_diagnosis_date >= 2011/01/01 & consensus_diagnosis_date <= 2013/12/12 ~ "DA by 2013",
    consensus_diagnosis_date >= 2014/01/01 & consensus_diagnosis_date <= 2017/12/12 ~ "DA by 2017",
    consensus_diagnosis_date >= 2018/01/01 & consensus_diagnosis_date <= 2019/12/12 ~ "DA by 2019"))

summary(lbcconsensus$dementia_ascert_year)

dem_2010 <-
  lbcconsensus %>%  
  filter(consensus_diagnosis_date <= as.Date("2010/12/12"))
summary(dem_2010)

dem_2010 <-lbcconsensus[lbcconsensus$consensus_diagnosis_date >= "2006/01/01" & lbcconsensus$consensus_diagnosis_date <= "2010/12/12", ]

ff_glimpse(dem_2010)


 
#Prevalence and CIs Dement consensus----
# Probable AND possible dementia (n=163)
binom.test(163, 865, 
           0.5,
           alternative = "two.sided", 
           conf.level = 0.95)

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



# Subanalysis prevalence - Age----
# in order to work out prev by age, we need to know how many people are alive at each age group
# First, create dementia_diagnosis age groups

lbcconsensus <-
  lbcconsensus %>%  
  mutate(dementia_age_group = case_when(
                        age_at_dem_diagnosis > 65 & age_at_dem_diagnosis <= 69.9999 ~ "65-70",
                        age_at_dem_diagnosis >= 70 & age_at_dem_diagnosis <= 74.9999 ~ "70-75",
                        age_at_dem_diagnosis >= 75 & age_at_dem_diagnosis <= 79.9999 ~ "75-80",
                        age_at_dem_diagnosis >= 80 & age_at_dem_diagnosis <= 84.9999 ~ "80-85",
                        age_at_dem_diagnosis >= 85 & age_at_dem_diagnosis <= 89.9999 ~ "85-90",
                        age_at_dem_diagnosis >= 90 ~ "90+"))

table(lbcconsensus$dementia_age_group)
#65-70 70-75 75-80 80-85 85-90 
#  1     7    36    61    13
hist(lbcconsensus$a85to90)

# Explore 85to90 group further----
# first, filter by just dem age 85-90 (n=13)
dem_age85to90 <-
  lbcconsensus %>%  
  filter(age_at_dem_diagnosis >= 85 & age_at_dem_diagnosis <= 89.9999)

summary(dem_age85to90$age_at_dem_diagnosis)
hist(dem_age85to90$age_at_dem_diagnosis)

# second, filter by just age 85-90 (n=544)
age85to90 <-
  lbcconsensus %>%  
  filter(age_group == "85-90")

summary(age85to90$AgedaysApx_LastCensor)
hist(age85to90$age_at_dem_diagnosis)

lbcconsensus <-
  lbcconsensus %>%  
  mutate(a85to90 = case_when(
    age_at_dem_diagnosis >= 85 & age_at_dem_diagnosis <= 89.9999 ~ "85-90"))


# Second, create death age groups
# first, summarise ageyrs_death variable for overview

summary(lbcconsensus$ageyrs_death)# 544 NAs (out of 865, thus 321 died since wave 2)

# next, mutate dead_age_group variable with levels based on age group

lbcconsensus <- lbcconsensus %>% 
  mutate(dead_age_group = case_when(ageyrs_death >= 65 & ageyrs_death <69.9999 ~ "65-70",
                                    ageyrs_death >= 70 & ageyrs_death <74.9999 ~ "70-75",
                                    ageyrs_death >= 75 & ageyrs_death <79.9999 ~ "75-80",
                                    ageyrs_death >= 80 & ageyrs_death < 84.9999 ~ "80-85",
                                    ageyrs_death >= 85 & ageyrs_death <= 89.9999 ~ "85-90",
                                    ageyrs_death >= 90 ~ "90+"))
  
table(lbcconsensus$dead_age_group) 
#70-75 75-80 80-85 85-90 
# 26   103   166    26 
# Total n = 321

## Died with dementia----
table(lbcconsensus$dead) # 321
table(lbcconsensus$dementia)

lbcconsensus <- lbcconsensus %>%  
  mutate(died_w_dementia = if_else(dead == 1 & dementia == 1, 1, 0))
table(lbcconsensus$died_w_dementia) # n  = 64 died, 54 alive

## Died w dementia, by age group ----
lbcconsensus %>% 
  group_by(dead_age_group, Sex.factor) %>% 
  summarise(sum(dementia.yesno))

# Subanalysis prevalence - Sex----
# subgroup male dataset
nrow(lbcconsensus) #865 # check no before subsetting

m_consensus <- lbcconsensus %>% 
  filter(sex == 1)
nrow(m_consensus) # 448

f_consensus <- lbcconsensus %>% 
  filter(sex == 2)
nrow(f_consensus) # 417

# check dementia by age for each sex
table(m_consensus$dementia_age_group)
table(f_consensus$dementia_age_group)

# check deaths by age group for each sex
table(m_consensus$dead_age_group)
# 70-75 75-80 80-85 85-90 
# 18    62    98    16 

table(f_consensus$dead_age_group)
#70-75 75-80 80-85 85-90 
#8    41    68    10 

# check deaths_w_dementia for each sex group
table(m_consensus$died_w_dementia) # 41

table(f_consensus$died_w_dementia) #23

view(lbcconsensus$dead)


# Calculate prevalence by age group ----
# From Tom's email Sept 22: 
# I think you’d need to be able to work out their age at a particular date, 
# but even without a DOB, you could just use the year in the first instance – 
# e.g. a diagnosis in 1996 would be assigned the age 60.
# You’d then need to create a number of dummy variables – dementia by 65, 
# dementia by 70 etc. These would be zero for everyone without dementia and 
# zero if the person hadn’t developed dementia by that age. 
# For instance, someone who developed dementia at 68 would have 0 for the dementia 
# by 65 variable and 1 for dementia by 70. You could then calculate the prevalence 
# for each of those dummy variables which would indicate the prevalence at each age. 
# I’d do it cumulatively like that, rather than 70-75, 75-80 etc. 
# You’ll need to think carefully about the denominator as someone who died at age 
# 70 should be excluded from the denominator for developing dementia at an older 
# age than that.

# I clarified with Tom that drop out between waves doesn't matter - just those who've died

# Step 2: create age categories accounting for deaths
lbcconsensus <- lbcconsensus %>% 
  mutate(a65_70 = if_else(ageyrs_death >=65 & ageyrs_death <70, 1, 0)) 
lbcconsensus <- lbcconsensus %>% 
  mutate(a70_75 = if_else(ageyrs_death >= 70 & ageyrs_death <75, 1, 0))
lbcconsensus <- lbcconsensus %>% 
  mutate(a75_80 = if_else(ageyrs_death >= 75 & ageyrs_death <80, 1, 0))
lbcconsensus <- lbcconsensus %>% 
  mutate(a80_85 = if_else(ageyrs_death >= 80 & ageyrs_death <85, 1, 0))
lbcconsensus <- lbcconsensus %>% 
  mutate(a85_90 = if_else(ageyrs_death >= 85 & ageyrs_death <90, 1, 0))

table(lbcconsensus$a65_70) # 0 out of 321 deaths occured before age 70
table(lbcconsensus$a70_75) # 26
table(lbcconsensus$a75_80) # 103
table(lbcconsensus$a80_85) # 166
table(lbcconsensus$a85_90) # 26


# imaging data ----
table(lbcconsensus$dem_imaging)
#Brain imaging results at consensus meeting (‘dem_imaging’) for ALL 163 ppl
#0	No imaging available
#1	CT head
#2	MRI
#3	Other – DAT/SPECT/PET


## first create logical vectors ----
# this is necessary for the case_when function later

### keep only probable dementia yes or no outcomes ----
# makes the 7 possible dementias and the 13 MCI cases a 'no dementia'. 
lbcconsensus <- lbcconsensus %>% 
  mutate(
    dementia.yesno = case_when(dementia.yesno == 1 ~ 1,
                               dementia.yesno == 0 | 2 | 3 ~ 0,
                               dementia.yesno == NA ~ NA_real_)
  )
table(lbcconsensus$dementia.yesno) # 118 

### second, create logical vectors of the various imaging w dementia outcomes ----

lbcconsensus <- lbcconsensus %>% 
  mutate(
    noimage = case_when(dem_imaging == 0 ~ 1,
                        dem_imaging == 1 | 2 | 3 ~ 0,
                        dem_imaging == NA ~ NA_real_)
  )
table(lbcconsensus$noimage) # 31 

lbcconsensus <- lbcconsensus %>% 
  mutate(
    ct = case_when(dem_imaging == 1 ~ 1,
                        dem_imaging == 0 | 2 | 3 ~ 0,
                        dem_imaging == NA ~ NA_real_)
  )
table(lbcconsensus$ct) # 73 

lbcconsensus <- lbcconsensus %>% 
  mutate(
    mri = case_when(dem_imaging == 2 ~ 1,
                        dem_imaging == 1 | 0 | 3 ~ 0,
                        dem_imaging == NA ~ NA_real_)
  )
table(lbcconsensus$mri) # 56

lbcconsensus <- lbcconsensus %>% 
  mutate(
    other = case_when(dem_imaging == 3 ~ 1,
                        dem_imaging == 1 | 2 | 0 ~ 0,
                        dem_imaging == NA ~ NA_real_)
  )
table(lbcconsensus$other) # 3 

## finally, combine dementia and imaging ----
lbcconsensus <- lbcconsensus %>%  
  mutate(dem_noimage = if_else(dementia.yesno == 1 & noimage == 1, 1, 0,),
         dem_ct = if_else(dementia.yesno == 1 & ct == 1, 1, 0,),
         dem_mri  = if_else(dementia.yesno == 1 & mri == 1, 1, 0,),
         dem_other  = if_else(dementia.yesno == 1 & other == 1, 1, 0,))
table(lbcconsensus$dem_noimage) #19
table(lbcconsensus$dem_ct) # 53
table(lbcconsensus$dem_mri) #43
table(lbcconsensus$dem_other) #3

# Euler figure of contributing sources----
install.packages("eulerr")
library(eulerr)

# A = EMR, B = Imaging, C = Death certificate, D = Home visit

fit1 <- euler(c("A" = 118, "B" = 99, "C" = 64, "D" = 10,
                "A&B" = 118, "A&C" =118, "A&D" = 118, 
                "B&C" = 99, "B&D" = 99, "C&D" = 0))

plot(fit1)




## ARCHIVED ATTEMPTS ----
### Check age range at each wave ----

# first, have a look at sumstats of age variables
## create dataset of age variables to skim and call it skimset
skimset <- lbcdata %>% 
  select(ageyears_w1, ageyears_w2, ageyears_w3, ageyears_w4, ageyears_w5)
skimr::skim(skimset) # gives nice tidy overview of these variables, plus histograms
write.table(table_skimset, file = here::here("tables","ageyears_skim.txt"), sep = ",", quote = FALSE, row.names = F) 

## other option for more detailed summary stats
df<- data.frame(lbcdata)
cols <- c('ageyears_w1', 'ageyears_w2', 'ageyears_w3', 'ageyears_w4', 'ageyears_w5')
table_ageyears <- stargazer(
  df[, cols], type = "text",
  summary.stat = c("min", "p25", "median", "p75", "max", "median", "sd")
)

save(table_ageyears, file = here::here("tables", "ageyears.rda"))
write.table(table_ageyears, file = here::here("tables","ageyears.txt"), sep = ",", quote = FALSE, row.names = F)


###prev by age group ----
# first, read in fresh LBC dataset (prior to any filtering for wave or dementia status)
lbcdata <- read_csv("~/Dropbox/Academic/PhD/LBC/mcr_project/MCRinLBC/MCRcombinedRecoded.csv")  
nrow(lbcdata) # 1091

# create dementia_by_age groups
# PROBLEM only counts people once so they are all used up by 70-74.99 group!
lbcdata<- lbcdata %>% 
  mutate(
    age_group = case_when(
      ageyears_w1 >=65 & ageyears_w1 <70 ~ "65 - 69.99", 
      ageyears_w1 | ageyears_w2 >=70 & ageyears_w1 | ageyears_w2 <75 ~ "70 - 74.99", 
      ageyears_w3 | ageyears_w4 >=75 & ageyears_w3 | ageyears_w4 <80 ~ "75 - 79.99", 
      ageyears_w4 | ageyears_w5 >=80 & ageyears_w4 | ageyears_w5 <85 ~ "80 - 84.99",   
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("65 - 69.99", "70 - 74.99","75 - 79.99", "80 - 84.99")
    ))
table(lbcdata$age_group) 

# PROBLEM - doing it separately results in participants being double-counted
lbcdata <- lbcdata %>% 
  mutate(a65_70 = if_else(ageyears_w1 >=65 & ageyears_w1 <70, 1, 0)) 
lbcdata <- lbcdata %>% 
  mutate(a70_75 = if_else(ageyears_w2 >= 70 & ageyears_w2 <75, 1, 0))
lbcdata <- lbcdata %>% 
  mutate(a70_75extra = if_else(ageyears_w3  <75, 1, 0))
lbcdata <- lbcdata %>% 
  mutate(a75_80 = if_else(ageyears_w3 >= 75 & ageyears_w3 <80, 1, 0))
lbcdata <- lbcdata %>% 
  mutate(a75_80extra = if_else(ageyears_w4 <80, 1, 0))
lbcdata <- lbcdata %>% 
  mutate(a80_85 = if_else(ageyears_w5 >= 80 & ageyears_w5 <85, 1, 0))
lbcdata <- lbcdata %>% 
  mutate(a80_85extra = if_else(ageyears_w4 >80, 1, 0))

table(lbcdata$a65_70) #730
table(lbcdata$a70_75) #866
table(lbcdata$a70_75extra) #20
table(lbcdata$a75_80) #677
table(lbcdata$a75_80extra) #460
table(lbcdata$a80_85) # 431
table(lbcdata$a80_85extra) #90


# Step 2: create age categories accounting for deaths - see tidier way of doing just this in actual script above. 
lbcconsensus <- lbcconsensus %>% 
  mutate(a65_70 = if_else(ageyrs_death >=65 & ageyrs_death <70, 1, 0)) 
lbcconsensus <- lbcconsensus %>% 
  mutate(a70_75 = if_else(ageyrs_death >= 70 & ageyrs_death <75, 1, 0))
lbcconsensus <- lbcconsensus %>% 
  mutate(a75_80 = if_else(ageyrs_death >= 75 & ageyrs_death <80, 1, 0))
lbcconsensus <- lbcconsensus %>% 
  mutate(a80_85 = if_else(ageyrs_death >= 80 & ageyrs_death <85, 1, 0))
lbcconsensus <- lbcconsensus %>% 
  mutate(a85_90 = if_else(ageyrs_death >= 85 & ageyrs_death <90, 1, 0))

table(lbcconsensus$a65_70) # 0 out of 321 deaths occured before age 70
table(lbcconsensus$a70_75) # 26
table(lbcconsensus$a75_80) # 103
table(lbcconsensus$a80_85) # 166
table(lbcconsensus$a85_90) # 26

### who died, when ----
age_at_dem_diagnosis == "65-70" & dead == 1 ~ "dead at 70", 
age_at_dem_diagnosis == "70-75" & dead == 1 ~ "dead at 75"))
table(lbcconsensus$alive_w_dementia)    
lbcconsensus <- lbcconsensus %>% 
  mutate(alive_w_dementia = if_else(age_at_dem_diagnosis == "65-70" & dead == 0, 1,0))
lbcconsensus <- lbcconsensus %>% 
  mutate(alive_w_dementia = if_else(age_at_dem_diagnosis == "70-75" & dead == 0, 1,0))

,  
age_at_dem_diagnosis >= 70 & age_at_dem_diagnosis <= 74.99 ~ "70-75",
age_at_dem_diagnosis >= 75 & age_at_dem_diagnosis <= 79.99 ~ "75-80",
age_at_dem_diagnosis >= 80 & age_at_dem_diagnosis <= 84.99 ~ "80-85",
age_at_dem_diagnosis >= 85 & age_at_dem_diagnosis <= 89.99 ~ "85-90",
age_at_dem_diagnosis >= 90 ~ "90+"))
