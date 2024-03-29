####################################################################
##### Insect Invasions Pursuit @ SESYNC                        #####
##### Example script to create clean attribute table           #####
##### created by Rachael Blake    09/23/2019                   #####
####################################################################

# Load packages needed for this script
library(tidyverse) ; library(readxl) ; library(purrr) ; library(DescTools) 
library(stringr) ; library(here)
library(googledrive) # authenticate to the Google account which can access the shared team drive


# source the custom functions if they aren't in your R environment
# source("nfs_data/custom_taxonomy_funcs.R")
library(insectcleanr)

# List all the data files
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
### Making the attribute table    ###
#####################################

# apply that function over the list of dataframes
attrib_list <- lapply(file_listp, separate_attributes_csv) 

df_attrib <- attrib_list %>% 
             purrr::reduce(full_join) %>% 
             # clean up origin column
             mutate(origin = gsub("&", "", origin),
                    origin = gsub("Indomaraya|indomalaya", "Indomalaya", origin),
                    origin = gsub("IndomalayaOceania", "Indomalaya, Oceania", origin),
                    origin = gsub("Middle East", "Middle_East", origin),
                    origin = gsub("cosmopolitan|Cosmoploitan", "Cosmopolitan", origin),
                    origin = gsub("S.\\sAfrica|Sth\\sAfrica", "South_Africa", origin),
                    origin = gsub("\\(Taiwan", "Taiwan", origin),
                    origin = gsub("\\(Okinawa|\\(Okinawa\\)", "Okinawa", origin),
                    origin = gsub("\\(Ogasawara", "Ogasawara", origin),
                    origin = gsub("\\(Java", "Java", origin),
                    origin = gsub("N.\\sAmerica", "North_America", origin),
                    origin = gsub("S.\\sAmerica", "South_America", origin),
                    origin = gsub("C.\\sAmerica", "Central_America", origin),
                    origin = gsub("Palearctic\\(Asia\\)|Plearctic\\(Asia\\)", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\s\\(Asia\\)|Paleartic\\(Asia\\)", "Palearctic_Asia", origin),
                    origin = gsub("Ppalearctic\\(Asia\\)|Palearctic\\(Asia", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\s\\(Asia|Paleartic\\(Asia", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\s\\(E.\\sAsia|Palearctic\\s\\(Central\\sAsia", "Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\(Europe\\)|Palearctic\\s\\(Europe\\)", "Palearctic_Europe", origin),
                    origin = gsub("alearctic\\(Europe\\)|Palearctic\\(Europe", "Palearctic_Europe", origin),
                    origin = gsub("Palearctic\\s\\(Europe|Paleartic\\(Europe", "Palearctic_Europe", origin),
                    origin = gsub("Parearctic\\(Europe", "Palearctic_Europe", origin),
                    origin = gsub("Palearctic\\s\\(Eurasia", "Palearctic_Europe, Palearctic_Asia", origin),
                    origin = gsub("Palearctic\\(Europe\\)Nearctic", "Palearctic_Europe, Nearctic", origin),
                    origin = gsub("Nearctic Nearctic", "Nearctic", origin),
                    origin = gsub("Nearctic\\(Europe\\)", "Palearctic_Europe", origin),
                    origin = gsub("\\\"Old World\\\"\\/Europe", "Old_World_Europe", origin),
                    origin = gsub("Sri\\sLanka\\sor\\sAustralasia\\?\\s\\(Dugdale,\\s1988", "Sri Lanka Australasia", origin),
                    origin = gsub("C\\/C.\\sAmerica\\?\\sOld\\sworld\\stropics\\s\\(Mound\\s&\\sWalker,\\s1982", 
                                  "Cosmopolitan, Central_America, Old_World_tropics", origin),
                    origin = gsub(" ", ", ", origin),
                    origin = gsub(", , ", ", ", origin),
                    origin = gsub(",, ", ", ", origin) 
                    ) %>% 
             mutate_at(vars("host_group", "phagy", "pest_type", "ecozone", "phagy_main", "feeding_type", "feeding_main"), 
                       list(~ replace(., . %in% c("NA", "Na"), NA_character_))) %>%  # make true NAs
             dplyr::rename("user_supplied_name" = "genus_species")  # NOTE: even though changed name to user_supplied_name,
                                                                    # these data have not been run through GBIF!  You still 
                                                                    # must join the attribute table with the taxonomy table!!!

# bring in taxonomic table for order, family, and genus columns
tax_table <- read.csv(here("taxonomy_table.csv"), stringsAsFactors=F)
tax_cols <- tax_table %>% select(taxon_id, user_supplied_name, order, family, genus, genus_species)

# origin_correspondence_table.csv for the 8 biogeographic regions
# identify the folder on Drive ; let googledrive know this is a file ID or URL, as opposed to file name
fix_folder <- drive_get(as_id("https://drive.google.com/drive/folders/1t0W54RR-rpEvEYRBzas7Szz1ApM3DxzF"))
# identify the origin_correspondence file in that folder
file_info <- drive_ls(fix_folder, pattern = "origin_correspondence")
# download origin_correspondence file to working directory
drive_download(as_id(file_info$id), overwrite = TRUE)
# now read the file into R from the working directory
o_corr_file <- read.csv(here("origin_correspondence_table.csv"), strip.white = TRUE) 

o_corr_table <- o_corr_file %>% 
                mutate_at(vars(starts_with("origin_")), list(~ replace(., . %in% c("NA"), NA_character_)))

# plant feeding attribute column from the non-plant-feeding_taxa file
# identify the non-plant-feeding file in that folder
file_info <- drive_ls(fix_folder, pattern = "non-plant-feeding")
# download non-plant-feeding file to working directory
drive_download(as_id(file_info$id), overwrite = TRUE)
# read the downloaded file into R
npf_file <- here("non-plant-feeding_taxa_updatedOct07.xlsx")
npf_ord <- read_excel(npf_file, sheet = 2, trim_ws = TRUE, col_types = "text")
npf_fams <- read_excel(npf_file, sheet = 3, trim_ws = TRUE, col_types = "text")
npf_gen <- read_excel(npf_file, sheet = 4, trim_ws = TRUE, col_types = "text")
pf_gen <- read_excel(npf_file, sheet = 6, trim_ws = TRUE, col_types = "text")
pf_sp <- read_excel(npf_file, sheet = 7, trim_ws = TRUE, col_types = "text")

# make plant feeding taxa names title case; make vectors using dplyr::pull() 
npf_ord <- npf_ord %>% mutate(npf_orders = str_to_title(npf_orders)) %>% pull()
npf_fams <- npf_fams %>% mutate(npf_families = str_to_title(npf_families)) %>% pull()
npf_gen <- npf_gen %>% mutate(npf_genus = str_to_title(npf_genus)) %>% pull()
pf_gen <- pf_gen %>% pull()
pf_sp <- pf_sp %>% pull()

# function to collapse rows with multiple entries
# coalesce_by_column <- function(df) {
#                       return(dplyr::coalesce(!!! as.list(df)))
#                       }
#############################
# make attribute table by user_supplied_name but using new manual coalesce custom function
#############################
df_attrib_o <- df_attrib %>% 
               left_join(tax_cols, by = "user_supplied_name") %>% # merge in taxonomic info
               left_join(o_corr_table) %>%  # merge in origin correspondence table
               # add plant feeding attribute column
               mutate(plant_feeding = "Y",
                      plant_feeding = ifelse(order %in% npf_ord, "N", plant_feeding),
                      plant_feeding = ifelse((order == "Blattodea" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Coleoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Hemiptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Hymenoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Lepidoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family %in% npf_fams), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Phlaeothripidae" & genus %in% npf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Thysanoptera" & family == "Thripidae" & genus %in% npf_gen), "N", plant_feeding),
                      plant_feeding = ifelse((order == "Coleoptera" & family == "Coccinellidae" & genus %in% pf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Muscidae" & genus %in% pf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Phoridae" & genus %in% pf_gen), "Y", plant_feeding),
                      plant_feeding = ifelse((order == "Diptera" & family == "Drosophilidae" & user_supplied_name %in% pf_sp), "Y", plant_feeding)
                      ) %>% 
               # remove irrelevant columns
               select(-origin, -country_nm, -nz_region, -order, -family, -genus) %>% 
               # coalesce rows to one per user_supplied_name
               group_split(user_supplied_name) %>% 
               map(~coalesce_manual(.x)) %>%
               bind_rows() %>% 
               # arrange rows and columns
               arrange(user_supplied_name) %>% 
               select(#taxon_id, 
                      user_supplied_name, genus_species,  
                      origin_Nearctic, origin_Neotropic, origin_European_Palearctic, origin_Asian_Palearctic, 
                      origin_Indomalaya, origin_Afrotropic, origin_Australasia, origin_Oceania, 
                      plant_feeding, host_type, host_group, phagy, pest_type, ecozone, phagy_main, 
                      feeding_type, feeding_main, notes
                      )

#####################################
### Write file                    ###
#####################################
# write out the attribute table
readr::write_csv(df_attrib_o, paste0(here(), "/attribute_table_", Sys.Date(), ".csv"))
                             

########################################################################
### Code to filter to unique GBIF genus_species, and do manual coalesce using new custom function
########################################################################


df_attrib_gbif <- df_attrib %>% 
                  left_join(tax_cols, by = "user_supplied_name") %>% # merge in taxonomic info
                  left_join(o_corr_table) %>%  # merge in origin correspondence table
                  # add plant feeding attribute column
                  mutate(plant_feeding = "Y",
                         plant_feeding = ifelse(order %in% npf_ord, "N", plant_feeding),
                         plant_feeding = ifelse((order == "Blattodea" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Coleoptera" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Diptera" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Hemiptera" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Hymenoptera" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Lepidoptera" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Thysanoptera" & family %in% npf_fams), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Thysanoptera" & family == "Phlaeothripidae" & genus %in% npf_gen), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Thysanoptera" & family == "Thripidae" & genus %in% npf_gen), "N", plant_feeding),
                         plant_feeding = ifelse((order == "Coleoptera" & family == "Coccinellidae" & genus %in% pf_gen), "Y", plant_feeding),
                         plant_feeding = ifelse((order == "Diptera" & family == "Muscidae" & genus %in% pf_gen), "Y", plant_feeding),
                         plant_feeding = ifelse((order == "Diptera" & family == "Phoridae" & genus %in% pf_gen), "Y", plant_feeding),
                         plant_feeding = ifelse((order == "Diptera" & family == "Drosophilidae" & user_supplied_name %in% pf_sp), "Y", plant_feeding)
                         ) %>% 
                  # removed irrelevant columns
                  select(-origin, -country_nm, -nz_region, -user_supplied_name, -order, -family, -genus, -notes,
                         -intentional_release) %>% 
                  # set column types
                  mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                                 origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                                 origin_Australasia, origin_Oceania),
                            list(as.numeric)) %>%         
                  # coalesce rows to one per GBIF genus_species
                  group_split(genus_species) %>% 
                  map(~coalesce_manual(.x)) %>%
                  bind_rows() %>% 
                  # arrange rows and columns
                  arrange(genus_species) %>% 
                  select(#-taxon_id, 
                         genus_species,
                         origin_Nearctic, origin_Neotropic, origin_European_Palearctic, origin_Asian_Palearctic, 
                         origin_Indomalaya, origin_Afrotropic, origin_Australasia, origin_Oceania, plant_feeding, 
                         host_type, host_group, phagy, pest_type, ecozone, phagy_main, feeding_type, feeding_main
                         #intentional_release, ever_introduced_anywhere
                         )



#####################################
### Write another file            ###
#####################################
# write out the attribute table
readr::write_csv(df_attrib_gbif, paste0(here(), "/attribute_table_gbif_", Sys.Date(), ".csv"))



