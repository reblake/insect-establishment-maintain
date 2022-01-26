#################################################
# temporary file to load current insectcleanr functions before package gets updated.

# functions copied from insectcleanr package main branch 26 Jan 2022
# and the separate_source function from the source_table branch
# and coalesce_manual
# and coalesce_occur - this file has disappeared from the recent update to the main branch, found it in the old custom functions file

#################################################
####################################################################
##### Functions for separating taxonomic and related info      #####
##### originally created by Rachael Blake    04/11/2019        #####
####################################################################

######################
#' separate_taxonomy_xl: a function that cleans the dataframes and separates taxonomy columns using xl files
#'
#' @param df_location location of files containing raw insect data
#'
#' @import dplyr
#' @importFrom readxl read_excel
#' @importFrom tidyselect one_of
#'
#' @return dataframe
#' @export
#'
#' @examples
#' file_list <- system.file("extdata", "Japan_taxa.xlsx", package = "insectcleanr", mustWork = TRUE)
#' taxa_list <- lapply(file_list, separate_taxonomy_xl)
separate_taxonomy_xl <- function(df_location){
  # reads the excel file in
  df <- readxl::read_excel(df_location, trim_ws = TRUE, col_types = "text")
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE)) %>%
    mutate_all(~gsub("\\.", "", . , perl=TRUE))
  
  # define what taxonomic columns might be named
  tax_class <- c("kingdom", "phylum", "class", "order", "family",
                 "genus", "species", "genus_species", "authority",
                 "super_family", "taxonomic_authority", "taxonomy_system")
  
  # split off any columns with any taxonomic column names
  df_2 <- df_1 %>%
    select(tidyselect::one_of(tax_class)) %>%
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    )
  
  # return df_2
  return(df_2)
}

######################
#' separate_taxonomy_csv: a function that cleans the dataframes and separates taxonomy columns using csv files
#'
#' @param df_location
#'
#' @import dplyr
#' @importFrom tidyselect one_of
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_taxonomy_csv <- function(df_location){
  # reads the excel file in
  df <- read.csv(df_location, strip.white = TRUE)
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE)) %>%
    mutate_all(~gsub("\\.", "", . , perl=TRUE))
  
  # define what taxonomic columns might be named
  tax_class <- c("kingdom", "phylum", "class", "order", "family",
                 "genus", "species", "genus_species", "authority",
                 "super_family", "taxonomic_authority", "taxonomy_system")
  
  # split off any columns with any taxonomic column names
  df_2 <- df_1 %>%
    select(tidyselect::one_of(tax_class)) %>%
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    )
  
  # return df_2
  return(df_2)
}


###########################################################
#' separate_occurrence_xl: function to read in and separate occurrence info using xl files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#' @importFrom readxl read_excel
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_occurrence_xl <- function(df_location){
  # reads the excel file in
  df <- readxl::read_excel(df_location, trim_ws = TRUE, col_types = "text")
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE))
  
  # define region
  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  
  df_2 <- df_1 %>%
    # split off any columns that are not relevant
    select(-one_of("kingdom", "phylum", "class", "order", "family",
                   "genus", "species", "authority", "super_family",
                   "suborder", "author", "common_name", "taxonomy_system",
                   "phagy", "host_group", "pest_type",
                   "jp_name", "source", "reference", "status", "synonym",
                   "origin2", "tsn", "comment", "original_species_name",
                   "rank", "name_changed___1_yes__0__no_", "phagy_main",
                   "feeding_type", "feeding_main", "size_mm_",
                   "current_distribution_cosmopolitan_", "town", "rege_date_source",
                   "nz_area_code", "life_form", "data_quality", "first_record_orig"
    )) %>%
    # add the name of the country as a column
    mutate(region = country_nm) %>%
    mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%
    # replace any non-numerical values in year column with NA
    mutate(year = gsub("u", NA_character_, year, perl=TRUE)) %>%
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    )
  
  # return df_2
  return(df_2)
}


###########################################################
#' separate_occurrence_csv: function to read in and separate occurrence info using csv files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_occurrence_csv <- function(df_location){
  # reads the csv file in
  df <- read.csv(df_location, strip.white = TRUE)
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE))
  
  # define region
  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  
  df_2 <- df_1 %>%
    # split off any columns that are not relevant
    select(-one_of("kingdom", "phylum", "class", "order", "family",
                   "genus", "species", "authority", "super_family",
                   "suborder", "author", "common_name", "taxonomy_system",
                   "phagy", "host_group", "pest_type",
                   "jp_name", "source", "reference", "status", "synonym",
                   "origin2", "tsn", "comment", "original_species_name",
                   "rank", "name_changed___1_yes__0__no_", "phagy_main",
                   "feeding_type", "feeding_main", "size_mm_",
                   "current_distribution_cosmopolitan_", "town", "rege_date_source",
                   "nz_area_code", "life_form", "data_quality", "first_record_orig"
    )) %>%
    # add the name of the country as a column
    mutate(region = country_nm) %>%
    mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%
    # replace any non-numerical values in year column with NA
    mutate(year = gsub("u", NA_character_, year, perl=TRUE)) %>%
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    )
  
  # return df_2
  return(df_2)
}



############################################################
#' separate_attributes_xl: function to separate attribute info for each taxa from the raw files using xl files
#'
#' @param df_location location of file containing taxa info
#'
#' @import dplyr
#' @importFrom readxl read_excel
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_attributes_xl <- function(df_location){
  # reads the excel file in
  df <- readxl::read_excel(df_location, trim_ws = TRUE, col_types = "text")
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE))
  
  # define region
  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  df_2 <- df_1 %>%
    # split off any columns that are not relevant
    select(-one_of("kingdom", "phylum", "class", "order", "family",
                   "genus", "species", "authority", "super_family",
                   "suborder", "author", "common_name", "taxonomy_system",
                   "jp_name", "source", "reference", "status", "synonym",
                   "origin2", "tsn", "comment", "original_species_name",
                   "rank", "name_changed___1_yes__0__no_", "size_mm_",
                   "town", "rege_date_source", "nz_area_code", "life_form",
                   "data_quality", "year", "canada_or_us"
    )) %>%
    mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%
    # clean up the taxonomic names
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    ) %>%
    # add country_nm column
    mutate(country_nm = country_nm)
  
  return(df_2)
}



############################################################
#' separate_attributes_csv: function to separate attribute info for each taxa from the raw files using csv files
#'
#' @param df_location location of file containing taxa info
#'
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_attributes_csv <- function(df_location){
  # reads the excel file in
  df <- read.csv(df_location, strip.white = TRUE)
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE))
  
  # define region
  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  df_2 <- df_1 %>%
    # split off any columns that are not relevant
    select(-one_of("kingdom", "phylum", "class", "order", "family",
                   "genus", "species", "authority", "super_family",
                   "suborder", "author", "common_name", "taxonomy_system",
                   "jp_name", "source", "reference", "status", "synonym",
                   "origin2", "tsn", "comment", "original_species_name",
                   "rank", "name_changed___1_yes__0__no_", "size_mm_",
                   "town", "rege_date_source", "nz_area_code", "life_form",
                   "data_quality", "year", "canada_or_us"
    )) %>%
    mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%
    # clean up the taxonomic names
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    ) %>%
    # add country_nm column
    mutate(country_nm = country_nm)
  
  return(df_2)
}



###################################
# from the source_table branch

###########################################################
#' separate_source_csv: function to read in and separate source info using csv files
#'
#' @param df_location location of files containing taxa info
#'
#' @import dplyr
#'
#' @return dataframe
#' @export
#'
#' @examples
separate_source_csv <- function(df_location){ # RT renamed function
  # reads the excel file in
  df <- read.csv(df_location, strip.white = TRUE)
  
  # clean up column names, capitalization, etc.
  df_1 <- df %>%
    # replace " " and "." with "_" in column names
    select_all(~gsub("\\s+|\\.", "_", .)) %>%
    select_all(tolower) %>%  # make all column names lower case
    mutate_all(~gsub("\\b([[:upper:]])([[:upper:]]+)",
                     "\\U\\1\\L\\2", . , perl=TRUE)) %>%
    mutate_all(~gsub("\\.", "", . , perl=TRUE))
  
  # define region
  file_name <- sapply(strsplit(as.character(df_location), split="/") , function(x) x[5])
  country_nm <- sapply(strsplit(as.character(file_name), split="_") , function(x) x[1])
  
  # define what relevant columns to select - note that the original column caled genus_species will become the user_supplied_name column
  source_columns <- c("genus_species", "year","intentional_release","eradicated","established_indoors_or_outdoors","source")
  
  # split off any columns with any taxonomic column names
  df_2 <- df_1 %>%
    select(tidyselect::one_of(source_columns)) %>%
    # add the name of the country as a column
    mutate(region = country_nm) %>%
    mutate_all(~gsub("(*UCP)\\s\\+|\\W+$", "", . , perl=TRUE)) %>%
    # replace any non-numerical values in year column with NA
    mutate(year = gsub("u", NA_character_, year, perl=TRUE)) %>%
    mutate(genus_species = gsub("\\ssp\\s[a-z]+\\s[a-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\s[A-Za-z]+\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.\\d\\s\\s[A-Za-z]+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s[A-Z]\\.[A-Z]\\.[A-Z][a-z]+\\,\\s\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]sp", "", genus_species, perl=TRUE),
           genus_species = gsub("[^\x20-\x7E]", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\([^()]*\\)", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\([A-Z].*$", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("^([A-Z][a-z]+\\s\\S+).*", "\\1", genus_species, perl=TRUE),
           genus_species = gsub("\\sssp\\.\\s[a-z].*$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp\\.[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp[A-Z]$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp(\\.|p|\\d|\\.\\d)$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\.", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn\\.sp\\.$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\sn$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ssp$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\d+$", "", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\ss", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\s\\s", " ", genus_species, perl=TRUE),
           genus_species = gsub("\\ssp$", "", genus_species, perl=TRUE)
    )
  
  # return df_2
  return(df_2)
}

######################################################

#######################################################
#' coalesce_manual: function to manually coalesce rows in attribute table
#'
#' @param df dataframe that may need coalescing
#'
#' @import dplyr
#' @importFrom DescTools Mode
#'
#' @return dataframe
#' @export
#'
#' @examples
#' file <- system.file("extdata", "Japan_taxa.xlsx", package = "insectcleanr", mustWork = TRUE)
#' data_j <- read_excel(file)
#' data_co <- coalesce_manual(data_j)
coalesce_manual <- function(df) {
  # test whether there are multiple rows
  if(nrow(df) == 1){coal_manual <- df %>%
    mutate_at(vars(taxon_id, genus_species, plant_feeding,
                   intentional_release, ever_introduced_anywhere,
                   host_type, established_indoors_or_outdoors, host_group,
                   phagy, pest_type, ecozone, phagy_main,
                   current_distribution_cosmopolitan_, feeding_type, feeding_main,
                   confirmed_establishment),
              list(as.character)) %>%
    mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                   origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                   origin_Australasia, origin_Oceania),
              list(as.numeric))
  } else {
    # coalesce non-origin columns
    coal_other <- df %>%
      select(taxon_id, genus_species, plant_feeding, intentional_release, ever_introduced_anywhere,
             host_type, established_indoors_or_outdoors, host_group, phagy, pest_type,
             ecozone, current_distribution_cosmopolitan_, phagy_main, feeding_type, feeding_main,
             confirmed_establishment) %>%
      group_by(genus_species) %>%
      summarize_all(DescTools::Mode, na.rm = TRUE) %>%
      ungroup()
    
    # coalesce origin columns
    coal_origin <- df %>%
      select(genus_species, starts_with("origin_")) %>%
      mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                     origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                     origin_Australasia, origin_Oceania),
                list(as.numeric)) %>%
      group_by(genus_species) %>%
      summarize_all( ~ ifelse((sum(., na.rm = TRUE) %in% c(1:10)), 1, 0)) %>%
      ungroup()
    
    # bind together origin and non-origin columns
    coal_manual <- full_join(coal_other, coal_origin) %>%
      select(genus_species, origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
             origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
             origin_Australasia, origin_Oceania, plant_feeding, intentional_release,
             ever_introduced_anywhere, everything()) %>%
      mutate_at(vars(taxon_id, genus_species, plant_feeding,
                     intentional_release, ever_introduced_anywhere,
                     host_type, established_indoors_or_outdoors, host_group,
                     phagy, pest_type, ecozone, phagy_main,
                     current_distribution_cosmopolitan_, feeding_type, feeding_main,
                     confirmed_establishment),
                list(as.character)) %>%
      mutate_at(vars(origin_Nearctic, origin_Neotropic, origin_European_Palearctic,
                     origin_Asian_Palearctic, origin_Indomalaya, origin_Afrotropic,
                     origin_Australasia, origin_Oceania),
                list(as.numeric))
  }
  
  return(coal_manual)
  
}

########################################
#######################################################
#' Title: function to manually coalesce rows in occurrence table   
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
coalesce_occur <- function(df) {  
  # test whether there are multiple rows
  if(nrow(df) == 1) {no_dup <- df %>% 
    select(genus_species, year, region, country, origin, 
           host_type, ecozone, intentional_release, 
           established_indoors_or_outdoors, confirmed_establishment, eradicated,
           present_status) %>% 
    mutate_at(vars(genus_species, region, country, 
                   host_type, origin, ecozone, intentional_release,
                   established_indoors_or_outdoors, confirmed_establishment, 
                   eradicated, present_status), 
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
      select(genus_species, region, country, origin, 
             host_type, ecozone, intentional_release, 
             established_indoors_or_outdoors, confirmed_establishment, eradicated,
             present_status) %>% 
      mutate_at(vars(genus_species, region, country, 
                     host_type, origin, ecozone, intentional_release,
                     established_indoors_or_outdoors, confirmed_establishment, 
                     eradicated, present_status),
                list(as.character)) 
    
    # put year and everything else back together
    no_dup <- gsr_dp %>% 
      full_join(yr) %>% 
      select(genus_species, year, region, country, origin, 
             host_type, ecozone, intentional_release, 
             established_indoors_or_outdoors, confirmed_establishment, eradicated,
             present_status)
    
  }
  return(no_dup)
}        
