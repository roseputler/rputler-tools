library(tidyverse)

#### Read in data ####
# There are 3 sets of cases, 3 of controls
cases <- list(cases_DayOf = read_csv("cases_abxdayof_swabs.csv"),
              cases_Week = read_csv("cases_abxweek_swabs.csv"),
              cases_Prior = read_csv("cases_prior_nosepsis.csv"))
cases <- lapply(cases,FUN = rename, DOB = `DOB(YY/MM/DD)`)

controls <- list(controls_DayOf = read_csv("controls_abxdayof_swabs.csv"),
                 controls_Week = read_csv("controls_abxweek_swabs.csv"),
                 controls_Prior = read_csv("controls_prior_nosepsis.csv"))
controls <- lapply(lapply(controls, FUN = rename, Gender = GenderCode),FUN = select,EncounterID,MRN,Gender,DOB,Order,ADM.DT,COLLECTION_DATE)

#### Matching code ####

# So now we match in each category of case/control (abx day of swab, abx week of swab, abx prior swab no sepsis)
# Our matching criteria is: 
## DOB w/in 5 years
## Swab w/in 30 days
## Gender
# And we want 2 controls per case
swab_match <- function(cases, controls){
  cases <- cases %>% # Take all rows of the data that are yes by attributable CDC severity
    arrange(DOB,COLLECTION_DATE) %>% # Sort by decreasing "score" (which I don't have, so must be something Krishna created before)
    mutate(case_status = 1,
           stratum = 1:n(),
           ageDiffYrs = as.numeric(NA),
           sampleDiffDays = as.numeric(NA)) # Set case "stratum" to row number/rank by score
  
  controls <- controls %>%  # All rows that are no by attributable CDC severity
    arrange(DOB,COLLECTION_DATE) %>%  # Sort by decreasing "score"
    mutate(case_status = 0)
  
  match <- data.frame() # Create data frame of matches to fill
  
  for (i in cases$stratum) # Loop over all rows in the cases
  {
    tmp <- controls %>% 
      mutate(stratum = i,
             ageDiffYrs = as.numeric(difftime(cases$DOB[i],DOB,units = "days")/365), # age difference between case and controls, in years
             sampleDiffDays = as.numeric(difftime(cases$COLLECTION_DATE[i],COLLECTION_DATE,units = "days"))) %>%  # sample collection date difference between case and controls, in days
      filter(Gender == cases$Gender[i] & # same gender
               abs(ageDiffYrs) < 5 & # age within 5 years
               abs(sampleDiffDays) < 30) # sample within 30 days
    
    if(nrow(tmp)>1){
      tmp <- tmp %>% 
        filter(MRN %in% sample(unique(MRN),2)) %>% # choose 2 unique MRNs, or 1 if there's only 1
        group_by(MRN) %>% 
        slice(sample(n(),1)) # choose 1 sample per MRN
    }
    
    if(nrow(tmp) > 0){ # If there were any matches, combine the previous matches, the current case and its matches.
      match <- match %>%
        rbind(data.frame(cases[i,],stringsAsFactors = F)) %>%
        rbind(data.frame(tmp,stringsAsFactors = F))
    }
    
    controls <- controls %>% 
      filter(!EncounterID %in% tmp$EncounterID) # Remove those controls that were already matched
  }
  
  return(match)
}

#### Matching for our data ####
set.seed(6103682)
matches_DayOf <- swab_match(cases = cases[[1]],controls = controls[[1]])
write.csv(matches_DayOf, "matches_abxdayof_swabs.csv", row.names = F, col.names = T, quote = F)
cases[[1]] %>% 
  filter(! (EncounterID %in% matches_DayOf$EncounterID) & ! ( Order %in% matches_DayOf$Order)) %>% 
  write.csv("nomatch_abxdayof_swabs.csv", row.names = F, col.names = T, quote = F)

matches_Week <- swab_match(cases = cases[[2]],controls = controls[[2]])
write.csv(matches_Week, "matches_abxweek_swabs.csv", row.names = F, col.names = T, quote = F)
cases[[2]] %>% 
  filter(! (EncounterID %in% matches_Week$EncounterID) & ! ( Order %in% matches_Week$Order)) %>% 
  write.csv("nomatch_abxweek_swabs.csv", row.names = F, col.names = T, quote = F)

matches_Prior <- swab_match(cases = cases[[3]],controls = controls[[3]])
write.csv(matches_Prior, "matches_prior_nosepsis.csv", row.names = F, col.names = T, quote = F)
cases[[3]] %>% 
  filter(! (EncounterID %in% matches_Prior$EncounterID) & ! ( Order %in% matches_Prior$Order)) %>% 
  write.csv("nomatch_prior_nosepsis.csv", row.names = F, col.names = T, quote = F)

