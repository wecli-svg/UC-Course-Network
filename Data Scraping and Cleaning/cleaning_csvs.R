## install packages
library(RJSONIO)
library(RCurl)
library(XML)
library(RSelenium)
library(stringr)
library(dplyr)
library(tidyr)

## UC Davis ----
UCD = read.csv("./Uncleaned/ucd_course_catalog.csv", check.names = FALSE)

UCD_clean = UCD %>% 
  mutate(Units = gsub(" units| unit", "", Units),
         Units_Min = gsub("-[0-9\\.]+", "", Units),
         Units_Max = gsub("^[0-9\\.]+-", "", Units),
         Subject = subject,
         Subject_Code = sub("^(.*) ([^ ]+)$", "\\1", Code),
         Course_Code = sub("^(.*) ([^ ]+)$", "\\2", Code)) %>% 
  relocate(Subject, Subject_Code, Course_Code, Title, Units_Min, Units_Max) %>% 
  select(-subject, -Units, -Code)

write.csv(UCD_clean, "./Cleaned/ucd_course_catalog_CLEAN.csv", row.names = FALSE, 
          fileEncoding = "UTF-8", na = "")

## UCLA ----
UCLA = read.csv("./Uncleaned/UCLA.csv")

activities = list("Lecture", "Laboratory", "Seminar", "Discussion", "Tutorial", 
                  "Studio", "Fieldwork", "Activity", "Field studies", "Clinic", "Proseminar",
                  "Research group meeting", "Practicum", "Recitation", "Readings",
                  "Off-campus field archaeology", "Site-based", "Colloquium",
                  "Workshop", "Student presentation")
activities_regex = paste(unlist(activities), collapse = "|")
activities_regex = paste0("(", activities_regex, ")", 
                          "[^.]*\\b(hours?|minutes?|to be arranged)\\b[^.]*\\.")

requisties = list("Requisites?", "Recommended?:", "Corequisites?", "Prerequisites?",
                  "Requisites? for", "Enforced requisites?", "Enforced corequisites?",
                  "Recommended requisites?", "Strongly recommended requisites?",
                  "Requisites? or corequisites?", "Highly recommended requisites?", 
                  "Recommended corequisites?")
requisties = lapply(requisties, function(x) paste0(x, "[^\\.]+\\."))
requisties_regex = paste(unlist(requisties), collapse = "|")
requisties_regex = paste0(requisties_regex, "|[^.]*\\b(requisite to)\\b[^.]*\\.")

limited = list("^Limited", "^Mandatory for and limited", "^Normally limited")
limited = lapply(limited, function(x) paste0(x, "[^\\.]+\\. "))
limited_regex = paste(unlist(limited), collapse = "|")

preparation = list("Preparation:", "Recommended preparation:")
preparation = lapply(preparation, function(x) paste0(x, "[^\\n]*?(?<!\\d)\\."))
preparation_regex = paste(unlist(preparation), collapse = "|")

UCLA_clean = UCLA %>% 
  mutate(course_number = gsub(". (?<=. ).*", "", course_title, perl = TRUE),
         course_title = gsub("^[^.]+. ", "", course_title),
         subj_area_nm = gsub(" \\([A-Z ]+\\) $", "", subj_area_nm),
         unt_min = gsub(" to [0-9\\.]+| or [0-9\\.]+", "", unt_rng),
         unt_max = gsub("[0-9\\.]+ to |[0-9\\.]+ or ", "", unt_rng),
         
         formerly = str_extract(crs_desc, "\\(Formerly [^\\)]+\\)"),
         formerly = gsub("\\(Formerly numbered |\\.\\)", "", formerly),
         crs_desc = gsub("\\(Formerly [^\\)]+\\) ", "", crs_desc),
         
         same_as = str_extract(crs_desc, "\\(Same [^\\)]+\\)"),
         same_as = gsub("\\(Same as |[.)]", "", same_as),
         crs_desc = gsub("\\(Same [^\\)]+\\) ", "", crs_desc),
         
         activities = str_extract(crs_desc, activities_regex),
         crs_desc = gsub(activities_regex, "", crs_desc),
         
         crs_desc = trimws(gsub("\\s+", " ", crs_desc)),
         
         requisites = str_extract(crs_desc, requisties_regex),
         crs_desc = gsub(requisties_regex, "", crs_desc),
         requisites = trimws(gsub("\\s+", " ", requisites)),
         
         preparation = str_extract(crs_desc, preparation_regex),
         crs_desc = gsub(preparation_regex, "", crs_desc, perl = TRUE),
         
         crs_desc = trimws(gsub("\\s+", " ", crs_desc)),
         
         limited_to = str_extract(crs_desc, limited_regex),
         crs_desc = gsub(limited_regex, "", crs_desc),
         
         grading = str_extract(crs_desc, "[^.]*\\bgrading\\b[^.]*\\.?"),
         crs_desc = gsub("[^.]*\\bgrading\\b[^.]*\\.?", "", crs_desc),
         grading = trimws(gsub("\\s+", " ", grading)),
         
         concurrently = str_extract(crs_desc, "Concurrently[^\\.]+\\."),
         crs_desc = gsub("Concurrently[^\\.]+\\.", "", crs_desc),
         
         may_be = str_extract(crs_desc, "May be[^\\.]+\\."),
         crs_desc = gsub("May be[^\\.]+\\.", "", crs_desc),
         
         not_open_credit = str_extract(crs_desc, "Not open[^\\.]+\\."),
         crs_desc = gsub("Not open[^\\.]+\\.", "", crs_desc),
         
         crs_desc = trimws(gsub("\\s+", " ", crs_desc))) %>% 
  relocate(subj_area_nm, subj_area_cd, course_number) %>% 
  select(-unt_rng)

write.csv(UCLA_clean, "./Cleaned/ucla_CLEAN.csv", row.names = FALSE, 
          fileEncoding = "UTF-8", na = "")

## UC Irvine ----
UCI = read.csv("./Uncleaned/uci_courses_catalog.csv", check.names = FALSE)

UCI_clean = UCI %>% 
  mutate(units = gsub(" Units\\.| Unit\\.", "", units),
         units_min = gsub("-[0-9\\.]+", "", units),
         units_max = gsub("^[0-9\\.]+-", "", units),
         subject_name = gsub(" \\([A-Z ]+\\)$", "", course_name),
         subject_code = sub("^(.*) ([^ ]+)$", "\\1", code),
         course_code = sub("^(.*) ([^ ]+)$", "\\2", code),
         course_code = gsub("\\.", "", course_code),
         prerequisites = gsub("Prerequisite: ", "", prerequisites),
         repeatability = gsub("Repeatability: ", "", repeatability)) %>% 
  relocate(subject_name, subject_code, course_code, title, units_min, units_max) %>% 
  select(-code, -course_name, -units)

write.csv(UCI_clean, "./Cleaned/uci_courses_catalog_CLEAN.csv", row.names = FALSE, 
          fileEncoding = "UTF-8", na = "")

## UC Santa Cruz ----
UCSC = read.csv("./Uncleaned/UCSC Uncleaned For Combine.csv", check.names = FALSE)

UCSC_clean = UCSC %>% 
  

## final dataset ----
UCD_short = UCD_clean %>% 
  mutate(Campus = "UCD") %>% 
  select(Campus,
         Subject, Subject_Code, Course_Code, Title, `Course Description`, 
         `Prerequisite(s)`, `Cross Listing`, Units_Min, Units_Max)

UCLA_short = UCLA_clean %>% 
  mutate(Campus = "UCLA") %>% 
  select(Campus,
         subj_area_nm, subj_area_cd, course_number, course_title, crs_desc, 
         requisites, same_as, unt_min, unt_max)
names(UCLA_short) = names(UCD_short)

UCI_short = UCI_clean %>% 
  mutate(Campus = "UCI") %>% 
  select(Campus,
         subject_name, subject_code, course_code, title, description, 
         prerequisites, units_min, units_max)
names(UCI_short) = names(UCD_short[-ncol(UCD_short)])

combined = bind_rows(UCD_short, UCLA_short, UCI_short)
write.csv(combined, "./Cleaned/combined_CLEAN.csv", row.names = FALSE, 
          fileEncoding = "UTF-8", na = "")
