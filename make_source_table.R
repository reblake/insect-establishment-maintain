####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean source table              #####
##### created by Rebecca Turner 21/01/2022                     #####
##### based on make_occurrence_table by Rachael Blake           #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(countrycode) ; library(DescTools)
library(here)
library(googledrive) # authenticate to the Google account which can access the shared team drive

# source the custom functions if they aren't in your R environment
# source("nfs_data/custom_taxonomy_funcs.R")
library(insectcleanr)

# List all the data files # RT 21/01/22 this needs to be edited right?
# file_list <- dir(path="nfs_data/data/raw_data/raw_by_country", pattern='*.xlsx')  # makes list of the files
# file_listp <- paste0("nfs_data/data/raw_data/raw_by_country/", file_list)         # adds path to file names
# store the folder url 
folder_url <- "https://drive.google.com/drive/folders/1WD7yqnmKEJc8PK-lNmwHniEuwl-xqIby"
# identify this folder on Drive ; let googledrive know this is a file ID or URL, as opposed to file name
folder <- drive_get(as_id(folder_url))
# identify the csv files in that folder
file_list <- drive_ls(folder, type = "csv")
# download them all to working directory
walk(file_list$id, ~ drive_download(as_id(.x), overwrite = TRUE))
# make list of files that are now in local working directory
file_csv <- dir(path = here(), pattern = "*.csv")
file_listp <- file_csv[!file_csv %in% grep("*FIXED.csv", file_csv, value = TRUE)]


#####################################
### Making the source table       ###
#####################################

# apply that function over the list of dataframes
source_list <- lapply(file_listp, separate_source) 

# put all occurrence dataframes into one large dataframe
df_source <- source_list %>% 
             purrr::reduce(full_join) %>% 
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
             # remove Arachnid
             filter(!(genus_species == "Trixacarus caviae")) %>% 
             # clean up year
             mutate(year = ifelse(year == -999, NA, year)) %>% # RT 21/01/2022 these is more to do here
             # clean up some species names
             mutate(genus_species = gsub("Mycetophila\xa0propria", "Mycetophila propria", genus_species),
                    genus_species = gsub("Mycetophila\xa0vulgaris", "Mycetophila vulgaris", genus_species),
                    genus_species = gsub("Mycetophila\xa0marginepunctata", "Mycetophila marginepunctata", genus_species)) %>%         
             # clean up year column
             mutate(year = ifelse(year %in% c("N/A", "Na"), NA_character_, year),
                    year = gsub("\\s", "", year, perl=TRUE)) %>% 
             # clean up eradicated
             mutate(eradicated = ifelse(eradicated %in% c("Na"), NA_character_, eradicated),
                    eradicated = ifelse(intentional_release == "Eradicated", "Yes", eradicated)) %>%  
             # clean up intentional release column
             mutate(intentional_release = ifelse(intentional_release %in% c("N", "0","Eradicated"), "No", # RT edited
                                          ifelse(intentional_release %in% c("1", "I", "Y"), "Yes", intentional_release))) %>% 
             mutate(intentional_release = ifelse(intentional_release %in% c("Na"), NA_character_, intentional_release)) %>% 
             mutate(genus_species = gsub("\xa0", " ", genus_species , perl=TRUE)) %>% # trying to get rid of weird characters
             dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s) # RT different
tax_table <- read.csv("nfs_data/data/clean_data/taxonomy_table.csv", stringsAsFactors=F)  # read in the taxonomy table

# make final source dataframe
source_df <- df_source %>%
             mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl = TRUE)) %>%  # remove rogue white spaces
             dplyr::rename(user_supplied_name = genus_species) %>% # have to rename genus_species to user_supplied_name so matches are correct
             dplyr::right_join(y = select(tax_table, c(user_supplied_name, genus_species,family, order,rank)), # RT changed to right join - should have all source rows
                               by = "user_supplied_name") %>% # join in the taxonomy info
             mutate(genus_species = gsub("<a0>", " ", genus_species, perl=TRUE)) %>% 
             select(user_supplied_name, genus_species, family, order,rank, year, region, 
                    intentional_release, established_indoors_or_outdoors, 
                    eradicated,source
                    ) %>% # make user_supplied_name column the first column; arrange other columns
             dplyr::arrange(user_supplied_name) # order by user_supplied_name


source_df2 <- source_df %>% 
              # remove ">" and other characters from year column
              mutate(year = gsub("\\D", "", year, perl = TRUE))

##### NOTE: there were no cases where there were different entries for intentional release in a
##### given genus_species/region combo


#####################################
### Write file                    ###
#####################################
# write the clean occurrence table to a CSV file
readr::write_csv(source_df2, paste0(here(), "/source_table_", Sys.Date(), ".csv")) # need to change route





