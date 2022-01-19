####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Script to update Coleoptera and Lepidoptera families     #####
##### created by Rachael Blake    03/23/2021                   #####
####################################################################

### load packages
library(tidyverse) ; library(readxl)

### load taxonomy table
tax_table <- read_csv("nfs_data/data/clean_data/taxonomy_table.csv")

### load Coleoptera fixes
coleop_fixes <- read_excel("nfs_data/data/raw_data/taxonomic_reference/Coleoptera_family_synonyms.xlsx")

### load Lepidoptera fixes
lepi_fixes <- read_csv("nfs_data/data/raw_data/taxonomic_reference/Lep_genus_family_transfers.csv") %>% 
              dplyr::rename(family_lep = family)

### merge Coleoptera fixes with taxonomy table
tt_col <- tax_table %>% 
          left_join(coleop_fixes, by = c("family" = "user_supplied_family_name")) %>% 
          mutate(family = ifelse(!is.na(actual_family), actual_family, family),
                 family = ifelse(family %in% c("Dryophthoridae","Brachyceridae"), "Curculionidae", family)) %>% 
          select(-actual_family)
  
 
### merge Lepidoptera fixes with Coleoptera-taxonomy table
tt_col_lep <- tt_col %>% 
              mutate(genus = ifelse(is.na(genus), word(genus_species, 1), genus)) %>%  # fill in genus if missing
              left_join(lepi_fixes, by = "genus") %>% 
              mutate(family = ifelse(!is.na(family_lep), family_lep, family)) %>% 
              select(-family_lep) %>% 
              mutate(family = ifelse(family %in% c("Lymantriidae","Arctiidae"), "Erebidae", family),
                     family = ifelse(family == "Acrolophidae", "Tineidae", family))


#####################################
### Write file                    ###
#####################################
# write the clean taxonomy table to a CSV file
readr::write_csv(tt_col_lep, "nfs_data/data/clean_data/taxonomy_table_cole_lep.csv")


