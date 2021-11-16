library(tidyverse)

old_files <- list.files("C:/Users/n8nel/Desktop/STAT 5080/Project/STAT6080_Group_Project/Renamed JPGs", pattern = "*.jpg", full.names = TRUE)
old_files



df_old <- data.frame(old_files) %>% mutate(page.num = substr(old_files, 78, 83),
                                           dates = substr(old_files, 85, 105))
vec_date <- df_old$dates
vec_page <- df_old$page.num

new_files <- paste0("C:/Users/n8nel/Desktop/STAT 5080/Project/STAT6080_Group_Project/Renamed JPGs/", vec_date, "_", vec_page,".jpg")

new_files

file.copy(from = old_files, to = new_files)

