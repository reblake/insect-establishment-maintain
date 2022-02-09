####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean taxonomy table            #####
##### created by Rachael Blake    11/15/2018                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(taxize) ; library(rgbif) ; library(purrr)
library(stringr) ; library(here)
library(googledrive) # authenticate to the Google account which can access the shared team drive

####################################
# source the custom functions 
# source("./custom_taxonomy_funcs.R")
library(insectcleanr)

####################################
# List all the raw data files 
# Raw data files on Google Drive Shared Drive called "SESYNC Insect Invasions"
####################################
# file_list <- dir(path="nfs_data/data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
# file_listp <- paste0("nfs_data/data/raw_data/raw_by_country/", file_list)         # adds path to file names
# store the folder url 
folder_url <- "https://drive.google.com/drive/folders/1WD7yqnmKEJc8PK-lNmwHniEuwl-xqIby"
# identify this folder on Drive ; let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url))
# identify the csv files in that folder
file_list <- drive_ls(folder, type = "csv")
# create new empty directory in working directory
if (file.exists("raw_data_files")) {cat("The folder already exists.")
} else {
dir.create(file.path(here(), "raw_data_files"))
}
# make vector of file destination full paths
dest_paths <- paste0(here(), "/raw_data_files/", file_list$name)
# download them all to directory
walk2(file_list$id, dest_paths, ~ drive_download(as_id(.x), overwrite = TRUE, 
                                                 path = .y))
# make list of files to send to the functions below
file_listp <- paste0("./raw_data_files/", file_list$name)
# file_listp1 <- file_csv[!file_csv %in% grep("*FIXED.csv", file_csv, value = TRUE)]
# file_listp <- file_listp1[!file_listp1 %in% grep("origin_corr*", file_listp1, value = TRUE)]           


####################################
### Making the taxonomic table   ###
####################################
# apply the separate_taxonomy function over the list of dataframes
tax_list <- lapply(file_listp, separate_taxonomy_csv) 

# put all taxonomy dataframes into one large dataframe
tax_df <- tax_list %>% 
          purrr::reduce(full_join) %>%  
          mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% 
          # mutate(authority = ifelse(is.na(authority), taxonomic_authority, authority)) %>% 
          # dplyr::select(-taxonomic_authority) %>% 
          dplyr::rename(taxonomic_authority = authority) %>% 
          dplyr::arrange(genus_species) #%>% 
         # dplyr::filter(!(genus_species == "Baridinae gen")) 

# define what taxonomic columns might be named        
tax_class <- c("kingdom", "phylum", "class", "order", "family", "super_family",
               "genus", "species", "genus_species", "taxonomic_authority", "taxonomy_system") 


#####################################
### Make large table into vector of all taxa names
tax_df1 <- tax_df %>% 
           mutate_at(vars(genus_species), str_squish) %>% 
           mutate(user_supplied_name = genus_species) %>% 
           dplyr::filter(!genus_species == "") %>%  # remove blank entries
           distinct(genus_species) %>%  # remove species duplicates          
           dplyr::arrange(genus_species) # arrange alphabetically
  

#####################################
### Make vectors of genus names (no species info) and species names
# make character vector of names only to genus
g_sp <- filter(tax_df1, (str_count(genus_species, " ") + 1) == 1)

tax_vec_gn <- unlist(g_sp, use.names = FALSE) # %>%  # gsub(" [a-zA-Z0-9]*", "", .) %>%
              # magrittr::extract(!(. == "Tasconotus")) # remove this species


# makes character vector of names only to species 
tax_vec_sp <- tax_df1 %>% 
              filter(!(genus_species %in% g_sp$genus_species))  %>% 
              # magrittr::extract(!(. %in% g_sp)) %>% 
              # magrittr::extract(!(. %in% g_spp)) %>% 
              unlist(., use.names = FALSE)# %>% 
            #  magrittr::extract(!(. == "Baridinae")) # this family put with genus above

#####################################
### Get taxonomy info from GBIF   ###
#####################################
xtra_cols <- c("kingdomkey", "phylumkey", "classkey", "orderkey", "specieskey",
               "note", "familykey", "genuskey", "scientificname", "canonicalname", "confidence")

######################
# apply the get_accepted_taxonomy function over the vector of species names
tax_acc_l <- lapply(tax_vec_sp, get_accepted_taxonomy) 

# make dataframe of all results
suppressMessages(
tax_acc <- tax_acc_l %>% 
           purrr::reduce(full_join) %>% 
           mutate(genus_species = str_squish(genus_species)) %>% 
           select(-one_of(xtra_cols))
)
########
# taxa that went missing in the code 
mia_s <- c(setdiff(tax_acc$user_supplied_name, tax_vec_sp), 
           setdiff(tax_vec_sp, tax_acc$user_supplied_name))


######################
# apply the get_accepted_taxonomy function over the vector of genus names
gn_acc_l <- lapply(tax_vec_gn, get_accepted_taxonomy) 

# make dataframe of all results
suppressMessages(
gen_acc <- gn_acc_l %>% 
           purrr::reduce(full_join) %>% 
           mutate(genus_species = str_squish(genus_species)) %>% 
           select(-one_of(xtra_cols))
)
########
# taxa that went missing in the code 
mia_g <- c(setdiff(gen_acc$user_supplied_name, tax_vec_gn), 
           setdiff(tax_vec_gn, gen_acc$user_supplied_name))

######################
# resolve species without accepted species names

########
# genus level matches from get_accepted_taxonomy results
genus_only <- tax_acc %>% 
              dplyr::filter(rank == "genus") #%>% 
              # filter out those where user_supplied_name was only genus to begin with
              #dplyr::filter(!word(user_supplied_name,-1) == "sp")

go_vec <- unlist(genus_only$user_supplied_name, use.names = FALSE)

# apply the function over the vector of species names
tax_go_l <- lapply(go_vec, get_more_info) 

# make dataframe of all species rank matches that were originally genus only
suppressMessages(
tax_go <- tax_go_l %>% 
          purrr::reduce(full_join) %>% # join all data frames from list
          dplyr::filter(!(matched_name2 == "species not found")) %>% # remove taxa that didn't provide a species-level match (no new info)
          dplyr::filter((str_count(matched_name2, '\\s+')+1) %in% c(2,3)) %>% 
          mutate(genus = ifelse((str_count(matched_name2, '\\s+')+1) == 1, matched_name2, stringr::word(matched_name2, 1)),
                 species = ifelse((str_count(matched_name2, '\\s+')+1) %in% c(2,3), matched_name2, NA_character_),
                 genus_species = ifelse(is.na(species), paste(genus, "sp"), species)) %>% 
          select(-matched_name2)
)
########
# taxa that went missing in the code 
# this is equivalent to no_lower below
mia_go <- c(setdiff(tax_go$user_supplied_name, go_vec), 
            setdiff(go_vec, tax_go$user_supplied_name))


# # send the species rank back through GBIF to identify synonyms
# synon_l <- lapply(tax_go$genus_species, get_accepted_taxonomy)
# 
# # How many synonyms were found on retesting in GBIF?
# suppressMessages(
# synon_retest <- synon_l %>%
#                 purrr::reduce(full_join) %>%
#                 mutate(genus_species = str_squish(genus_species)) %>%
#                 select(-one_of(xtra_cols)) %>% 
#                 filter(synonym == TRUE & rank == "species")
# )
# 
# # tax_go[tax_go$genus_species %in% c("Exochomus quadripustulatus", "Opius makii"),]
# 
# # tax_go results minus the two retested synonyms
# syn <- synon_retest$user_supplied_name
# 
# tax_go2 <- tax_go %>% 
#            filter(!(genus_species %in% syn))

                 
# How many did not return lower rank? 
suppressMessages(
no_lower <- tax_go_l %>% 
            purrr::reduce(full_join) %>% # join all data frames from list
            # filter to taxa that only returned genus (no new info)
            dplyr::filter((str_count(matched_name2, '\\s+')+1) == 1|
                           matched_name2 == "species not found") 
)

# from no_lower, the not found
no_lower_not_found <- no_lower %>% filter(matched_name2 == "species not found")

# from no_lower, the genus_only matches
no_lower_genus <- no_lower %>% filter(!(matched_name2 == "species not found"))

########
# species not found at all from get_accepted_taxonomy results
not_found <- tax_acc %>% 
             dplyr::filter(genus_species == "species not found",
                           !(is.na(user_supplied_name))) %>% 
             bind_rows(no_lower_not_found)

not_found_vec <- unlist(not_found$user_supplied_name, use.names = FALSE)

# apply the function over the vector of species names
tax_nf_l <- lapply(not_found_vec, get_more_info) 

# make dataframe of matches at species rank
suppressMessages(
tax_nf <- tax_nf_l %>% 
          purrr::reduce(full_join) %>% 
          dplyr::filter(!(matched_name2 == "species not found")) %>% 
          dplyr::filter((str_count(matched_name2, '\\s+')+1) == 2) %>% 
          mutate(genus = ifelse((str_count(matched_name2, '\\s+')+1) == 1, matched_name2, NA_character_),
                 species = ifelse((str_count(matched_name2, '\\s+')+1) %in% c(2,3), matched_name2, NA_character_),
                 genus_species = ifelse(is.na(species), genus, species)) %>% 
          mutate(genus = ifelse(is.na(genus), stringr::word(species, 1), genus)) %>% 
          select(-matched_name2)
)
########
# taxa that went missing in the code 
mia_nf <- c(setdiff(tax_nf$user_supplied_name, not_found_vec), 
            setdiff(not_found_vec, tax_nf$user_supplied_name))

# How many were not found? 
suppressMessages(
no_match <- tax_nf_l %>% 
            purrr::reduce(full_join) %>% # join all data frames from list
            dplyr::filter(matched_name2 == "species not found")
)

# How many returned at genus level rank?  
suppressMessages(
nf_go <- tax_nf_l %>% 
         purrr::reduce(full_join) %>%  # join all data frames from list
         dplyr::filter((str_count(matched_name2, '\\s+')+1) == 1)
) 

########
# put together genus-level only matches
genus_matches <- no_lower_genus %>% 
                 bind_rows(nf_go) %>% 
                 mutate(genus = matched_name2) %>% 
                 dplyr::rename(genus_species = matched_name2)

#############################
# bring in manual corrections 
# identify the folder on Drive ; let googledrive know this is a file ID or URL, as opposed to file name
fix_folder <- drive_get(as_id("https://drive.google.com/drive/folders/1t0W54RR-rpEvEYRBzas7Szz1ApM3DxzF"))
# identify the FIXED file in that folder
file_info <- drive_ls(fix_folder, pattern = "FIXED")
# download FIXED file to working directory
drive_download(as_id(file_info$id), overwrite = TRUE)

sal_taxa <- read_csv(here("genus_only_resolution_FIXED.csv"), trim_ws = TRUE,
                     col_types = cols(up_to_date_name = col_character()))

# add manual corrections to correct genus-level only matches
genus_match_SAL <- genus_matches %>% 
                   left_join(sal_taxa, by = "user_supplied_name") %>% 
                   transmute(user_supplied_name, 
                             taxonomy_system = ifelse(!(is.na(genus_species.y)), taxonomy_system.y, taxonomy_system.x),
                             #kingdom, phylum, class, 
                             order, family,
                             genus = ifelse(!(is.na(genus_species.y)), word(genus_species.y, 1), genus),
                             species = ifelse(!(is.na(genus_species.y)) & rank == "species",
                                              genus_species.y, NA_character_),
                             genus_species = ifelse(!(is.na(genus_species.y)), genus_species.y, genus_species.x),
                             rank, synonym) %>% 
                   mutate(synonym = as.character(synonym))

manually_matched <- subset(genus_match_SAL, (user_supplied_name %in% sal_taxa$user_supplied_name))

########
# dataframes of remaining unmatched taxa and 
# remaining manual corrections (will be implemented by row replacement below)

# taxa still missing a genus match
still_no_match <- subset(genus_match_SAL, !(user_supplied_name %in% sal_taxa$user_supplied_name))

# taxa included in sal_taxa but not matched in genus_matches (could be from interception data)
man_correct_remain <- subset(sal_taxa, !(user_supplied_name %in% manually_matched$user_supplied_name)) 

########
# put together dataframes with new info
new_sp_info <- tax_nf %>% 
               full_join(tax_go) %>% 
               dplyr::left_join(select(genus_only, user_supplied_name, kingdom,   # this and the transmute adds back in the higher rank info
                                phylum, class, order, family), by = "user_supplied_name") %>% 
               # full_join(synon_retest) %>% 
               full_join(manually_matched) %>%  # df of manual corrections  
               mutate(genus = ifelse(is.na(genus), word(genus_species, 1), genus),
                      rank = ifelse(is.na(rank) & str_count(genus_species, '\\w+')%in% c(2,3),  
                                "species", rank),
                      kingdom = ifelse(is.na(kingdom), "Animalia", kingdom),
                      phylum = ifelse(is.na(phylum), "Arthropoda", phylum),
                      class = ifelse(is.na(class), "Insecta", class))

              
#######################################################################
### Combine species list and GBIF accepted names                    ###
#######################################################################
tax_combo <- dplyr::filter(tax_acc, rank %in% c("species", "subspecies")) %>% # GBIF matches to species rank  
             full_join(gen_acc) %>%  # df of taxa where user supplied name was genus only to start with
             full_join(new_sp_info, by = "user_supplied_name") %>%  # bind in the new info from auto and manual resolution
             transmute(user_supplied_name,  
                       rank = ifelse(is.na(rank.y), rank.x, rank.y),
                       status, # = ifelse(is.na(status.y), status.x, status.y), 
                       matchtype, # = ifelse(is.na(matchtype.y), matchtype.x, matchtype.y), 
                       usagekey, # = ifelse(is.na(usagekey.y), usagekey.x, usagekey.y),
                       synonym = ifelse(is.na(synonym.y), synonym.x, synonym.y),
                       acceptedusagekey, # = ifelse(is.na(acceptedusagekey.y), acceptedusagekey.x, acceptedusagekey.y), 
                       kingdom = ifelse(is.na(kingdom.y), kingdom.x, kingdom.y),
                       phylum = ifelse(is.na(phylum.y), phylum.x, phylum.y), 
                       class = ifelse(is.na(class.y), class.x, class.y), 
                       order = ifelse(is.na(order.y), order.x, order.y), 
                       family = ifelse(is.na(family.y), family.x, family.y), 
                       genus = ifelse(is.na(genus.y), genus.x, genus.y),
                       species = ifelse(is.na(species.y), species.x, species.y),
                       genus_species = ifelse(is.na(genus_species.y), genus_species.x, genus_species.y),
                       taxonomy_system = ifelse(is.na(taxonomy_system.y), taxonomy_system.x, taxonomy_system.y),
                       taxonomic_authority) %>% # = ifelse(is.na(taxonomic_authority.y), taxonomic_authority.x, taxonomic_authority.y)) %>% 
             # a bit more cleaning from Rebecca Turner
             mutate(family = ifelse(family %in% c("Rutelidae","Melolonthidae", "Dynastidae"), "Scarabaeidae", family),
                    family = ifelse(genus == "Dermestes", "Dermestidae", family),
                    family = ifelse(family == "Eumenidae", "Vespidae", family)) %>% 
             dplyr::filter(!(is.na(user_supplied_name))) # remove blank rows

# subset remaining manual fixes for those user supplied names that are in tax_combo to get rows that need to be replaced
# rows_2_replace <- subset(man_correct_remain, (user_supplied_name %in% tax_combo$user_supplied_name)) 

# replace rows with new info, and add rows from interception data
tax_final <- tax_combo %>% 
             full_join(man_correct_remain, by = "user_supplied_name") %>% 
             transmute(user_supplied_name, 
                       status , #= ifelse(!is.na(rank.y), NA_character_, status), 
                       matchtype = ifelse(!is.na(rank.y), NA_character_, matchtype), 
                       usagekey = ifelse(!is.na(rank.y), NA_character_, usagekey), 
                       rank = ifelse(!is.na(rank.y), rank.y, rank.x), 
                       synonym = ifelse(!is.na(rank.y), synonym.y, synonym.x),
                       acceptedusagekey = ifelse(!is.na(rank.y), NA_character_, acceptedusagekey), 
                       kingdom, phylum, class, 
                       order = ifelse(!is.na(order.y), order.y, order.x),
                       family = ifelse(!is.na(family.y), family.y, family.x),
                       genus = ifelse(!is.na(family.y), word(genus_species.y, 1), genus),
                       species = ifelse(!is.na(family.y), word(genus_species.y, 2), species), 
                       genus_species = ifelse(!is.na(genus_species.y), genus_species.y, genus_species.x),
                       taxonomy_system = ifelse(!is.na(taxonomy_system.y), taxonomy_system.y, taxonomy_system.x),
                       taxonomic_authority = ifelse(!is.na(taxonomic_authority.y), taxonomic_authority.y, taxonomic_authority.x)) %>% 
             mutate(kingdom = ifelse(is.na(kingdom), "Animalia", kingdom),
                    phylum = ifelse(is.na(phylum), "Arthropoda", phylum),
                    class = ifelse(is.na(class), "Insecta", class),
                    genus_species = ifelse(genus_species == "species not found", NA_character_, genus_species)) %>% 
             mutate(genus = ifelse(is.na(genus), word(genus_species, 1), genus),
                    species = ifelse(is.na(species), genus_species, species)) %>% 
             arrange(user_supplied_name) 

########
# taxa that went missing in the code 
# this is equivalent to no_lower below
# mia_all <- c(setdiff(tax_final$user_supplied_name, tax_df1$genus_species), 
#              setdiff(tax_df1$genus_species, tax_final$user_supplied_name))

# duplicates
# dups <- tax_final %>% group_by(user_supplied_name) %>% filter(n()>1)

# Bostrichidae
# bos <- tax_combo %>% filter(family == "Bostrichidae")

# Rutelidae, Melolonthidae, and Dynastidae
# RMD <- tax_combo %>% filter(family %in% c("Rutelidae", "Melolonthidae", "Dynastidae"))

#####################################
### Write file                    ###
#####################################
# write the clean taxonomy table to a CSV file
readr::write_csv(tax_final, paste0(here(), "/taxonomy_table_", Sys.Date(), ".csv"))


