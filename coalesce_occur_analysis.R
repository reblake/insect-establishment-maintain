#######################################
# coalesce_occur for analysis function

#######################################

coalesce_occur_analysis <- function(df) {
  # test whether there are multiple rows
  if(nrow(df) == 1) {no_dup <- df %>%
    select(genus_species, year, region, intentional_release,
           established_indoors_or_outdoors,  eradicated) %>%
    mutate_at(vars(genus_species, region, intentional_release,
                   established_indoors_or_outdoors, 
                   eradicated),
              list(as.character)) %>%
    mutate_at(vars(year), list(as.numeric))
  } else {
    
    # coalesce to earliest year
    yr <- df %>%
      select(genus_species, region, year) %>%
      group_by(genus_species, region) %>%
      # summarize_all( ~ ifelse(nrow(year)>1 , , year)) %>%
      filter(rank(year, ties.method = "first") == 1) %>%
      ungroup() %>%
      mutate_at(vars(year), list(as.numeric))
    
    # take out duplicates in the genus_species/region columns
    gsr_dp <- df %>%
      select(-year) %>%
      group_by(genus_species, region) %>%
      summarize_all(DescTools::Mode, na.rm = TRUE) %>%
      ungroup() %>%
      select(genus_species, region, intentional_release,
             established_indoors_or_outdoors, eradicated) %>%
      mutate_at(vars(genus_species, region, intentional_release,
                     established_indoors_or_outdoors, 
                     eradicated),
                list(as.character))
    
    # put year and everything else back together
    no_dup <- gsr_dp %>%
      full_join(yr) %>%
      select(genus_species, year, region, intentional_release,
             established_indoors_or_outdoors, eradicated)
    
  }
  return(no_dup)
}
