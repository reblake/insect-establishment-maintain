#######################################################################
##### Insect Invasions Pursuit @ SESYNC                           #####
##### Example script to create clean occurrence table             #####
##### which is analysis ready:                                    #####
##### contains no duplicates of genus_species region combinations #####
##### only contains columns which have been curated               #####
##### based on code created by Rachael Blake    11/19/2018        #####
#######################################################################

# RT: I've made some edits to make this work on my computor.
# RT: the purpose of this file is to check that the script runs smoothly on my computor

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(countrycode) ; library(DescTools)
library(here)
library(googledrive) # authenticate to the Google account which can access the shared team drive

# source the custom functions if they aren't in your R environment
#source("nfs_data/custom_taxonomy_funcs.R")
library(insectcleanr) # RT: given that the insectcleanr package is not updated, 
# I copied functions from the main branch of the insectclear project into 
# the temporary_insectcleanr_functions.R script to load the function into the environment 

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


#####################################
### Making the occurrence table   ###
#####################################

# apply that function over the list of dataframes
occurr_list <- lapply(file_listp, separate_occurrence_csv) 
tax_class<-c("genus_species","year","eradicated","intentional_release","region","established_indoors_or_outdoors")

# put all occurrence dataframes into one large dataframe
df_occurr <- occurr_list %>% 
  purrr::reduce(full_join) %>% 
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>% # remove rogue white spaces
  select(tidyselect::one_of(tax_class)) %>%
  # remove Arachnid
  filter(!(genus_species == "Trixacarus caviae")) %>% 
  # clean up/fill in country column
  mutate(year = ifelse(year == -999, NA, year),) %>% 
  # clean up some species names
  mutate(genus_species = gsub("  ", " ", genus_species),
         genus_species = gsub("Mycetophila\xa0propria", "Mycetophila propria", genus_species),
         genus_species = gsub("Mycetophila\xa0vulgaris", "Mycetophila vulgaris", genus_species),
         genus_species = gsub("Mycetophila\xa0marginepunctata", "Mycetophila marginepunctata", genus_species),
         genus_species = ifelse(genus_species == "Xylocoris", "Xylocoris sp", genus_species)) %>%         
  # clean up year column
  mutate(year = ifelse(year %in% c("N/A", "Na"), NA_character_, year),
         year = gsub("\\s", "", year, perl=TRUE)) %>% 
  # clean up eradicated
  mutate(eradicated = ifelse(eradicated %in% c("Na"), NA_character_, eradicated),
         eradicated = ifelse(eradicated %in% c("Yes","pending"), "Yes", eradicated),
         eradicated = ifelse(!is.na(intentional_release)&(intentional_release == "Eradicated"), "Yes",eradicated),) %>% 
  # clean up intentional release column
  mutate(intentional_release = ifelse(intentional_release %in% c("N","0", "Eradicated"), "No", 
                                      ifelse(intentional_release %in% c("1", "I", "Y"), "Yes", intentional_release))) %>% 
  mutate(intentional_release = ifelse(intentional_release %in% c("Na"), NA_character_, intentional_release)) %>% 
  mutate(genus_species = gsub("\xa0", " ", genus_species , perl=TRUE)) %>% # trying to get rid of weird characters
  # dplyr::select(-canada_or_us, -nz_region) %>% 
  dplyr::arrange(genus_species) 

# add the unique ID column and delete genus species column(s)
# tax_table <- read.csv(here("taxonomy_table.csv"), stringsAsFactors=F)  # read in the taxonomy table # RT: maybe this only works for google drive
# alternative local version
tax_table <- read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/SESYNC/SESYNC_Repos/insect-establishment-maintain/taxonomy_table_2022-02-10.csv", stringsAsFactors=F)

# make final occurrence dataframe
occurr_df <- df_occurr %>%
  mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl = TRUE)) %>%  # remove rogue white spaces
  dplyr::rename(user_supplied_name = genus_species) %>% # have to rename genus_species to user_supplied_name so matches are correct
  dplyr::left_join(y = select(tax_table, c(user_supplied_name, genus_species,rank)),
                   by = "user_supplied_name") %>% # join in the taxonomy info
  mutate(genus_species = gsub("<a0>", " ", genus_species, perl=TRUE)) %>% 
  select(user_supplied_name, genus_species, year, region, rank,
         intentional_release, established_indoors_or_outdoors,
         eradicated
  ) %>% # make taxon_id column the first column; arrange other columns
  dplyr::arrange(user_supplied_name) # order by taxon_id


occurr_df2 <- occurr_df %>% 
  # remove ">" and other characters from year column
  mutate(year = gsub("\\D", "", year, perl = TRUE),year = gsub("pre", "", year, perl = TRUE)) %>% 
  # take out duplicates in the genus_species/region columns
  group_split(genus_species, region) %>% 
  map(~coalesce_occur_analysis(.x)) %>% # RT: what is the coalesce_occur function?
  bind_rows() 

##### NOTE: there were no cases where there were different entries for intentional release in a
##### given genus_species/region combo
occurr_df3<-occurr_df2[!is.na(occurr_df2$genus_species),] #13308

#checks
temp<-distinct(occurr_df3[,c("genus_species","region")]) # 13308 - no replicates?
temp<-occurr_df3
temp$uni<-paste(temp$genus_species,temp$region)
combo<-temp$uni
counts<-table(temp$uni)
counts[counts>1]
temp<-occurr_df
temp$uni<-paste(temp$genus_species,temp$region)
counts<-table(temp$uni)
counts[counts>1]
check<-setdiff(temp$uni,combo) # just the NAs

#####################################
### Write file                    ###
#####################################
# write the clean occurrence table to a CSV file
readr::write_csv(occurr_df3, paste0(here(), "/occurrence_table_analysis_ready_", Sys.Date(), ".csv"))

#####################
# make occurrence_table_analysis_ready_col_lep
tax_tableCL <- read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/SESYNC/SESYNC_Repos/insect-establishment-maintain/taxonomy_table_cole_lep_2022-02-10.csv", stringsAsFactors=F)
# check, this should only differ by family names
# some differences in special characters!
tax_col<-c("user_supplied_name","rank","genus_species","family","order")
temp<-setdiff(tax_tableCL[,tax_col],tax_table[,tax_col])
occurr_df3_CL<-left_join(occurr_df3,distinct(tax_tableCL[tax_tableCL$rank%in%c("species","genus"),c("rank","genus_species","family","order")]),by="genus_species")

readr::write_csv(occurr_df3_CL, paste0(here(), "/occurrence_table_analysis_ready_ColLep", Sys.Date(), ".csv"))



##################################################
# stats on difference with previous establishment table:

EstOld<-read.csv("C:/Users/TurnerR/OneDrive - scion/Data/Raw_Data/Establishment2021April15ColLep.csv",stringsAsFactors = FALSE)

sp<-unique(occurr_df3_CL$genus_species)
new_sp<-unique(setdiff(occurr_df3_CL$genus_species,EstOld$genus_species)) # 794
removed_sp<-unique(setdiff(EstOld$genus_species,occurr_df3_CL$genus_species)) # 371
Net_add_sp<-length(unique(occurr_df3_CL$genus_species))-length(unique(EstOld$genus_species))
