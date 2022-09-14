library(pacman)
pacman::p_load(tidyverse, finalfit, here)

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

# Subanalysis prevalence - Age----
# First, create dementia_diagnosis age groups
lbcconsensus <-
  lbcconsensus %>%  
  mutate(dementia_age_group = case_when(
                        age_at_diagnosis > 65 & age_at_diagnosis <= 69.99 ~ "65-70",
                        age_at_diagnosis >= 70 & age_at_diagnosis <= 74.99 ~ "70-75",
                        age_at_diagnosis >= 75 & age_at_diagnosis <= 79.99 ~ "75-80",
                        age_at_diagnosis >= 80 & age_at_diagnosis <= 84.99 ~ "80-85",
                        age_at_diagnosis >= 85 & age_at_diagnosis <= 89.99 ~ "85-90",
                        age_at_diagnosis >= 90 ~ "90+"))

table(lbcconsensus$dementia_age_group)


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


# Check age range at each wave ----
# install.packages("stargazer") 
# install.packages("skimr")
library(skimr) # summary stats about variables (incl histograms)
library(stargazer) # publication quality tables

# create dataset of age variables to skim and call it skimset
skimset <- lbcdata %>% 
  select(ageyears_w1, ageyears_w2, ageyears_w3, ageyears_w4, ageyears_w5)
skimr::skim(skimset) # gives nice tidy overview of these variables, plus histograms
write.table(table_skimset, file = here::here("tables","ageyears_skim.txt"), sep = ",", quote = FALSE, row.names = F) 

# other option for more detailed summary stats
df<- data.frame(lbcdata)
cols <- c('ageyears_w1', 'ageyears_w2', 'ageyears_w3', 'ageyears_w4', 'ageyears_w5')
table_ageyears <- stargazer(
  df[, cols], type = "text",
  summary.stat = c("min", "p25", "median", "p75", "max", "median", "sd")
)

save(table_ageyears, file = here::here("tables", "ageyears.rda"))
write.table(table_ageyears, file = here::here("tables","ageyears.txt"), sep = ",", quote = FALSE, row.names = F)



# Calculate prevalence by age group ----
# first, read in fresh LBC dataset (prior to any filtering for wave or dementia status)
lbcdata <- read_csv("~/Dropbox/Academic/PhD/LBC/mcr_project/MCRinLBC/MCRcombinedRecoded.csv")  
nrow(lbcdata) # 1091

# create age groups
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


