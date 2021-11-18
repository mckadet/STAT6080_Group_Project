library(tesseract)
library(magick)
library(magrittr)
library(imagefx)
library(pdftools)
library(magick)
library(tidyverse)
library(RColorBrewer)

# SET THIS DIRECTORY TO WHERE YOU HAVE THE FILES STORED ON YOUR COMPUTER
directory <- "C:/Users/n8nel/Desktop/STAT 5080/Project/STAT6080_Group_Project/Renamed JPGs/"

# Grabbing all of the JPGs from the directory that you gave and making a data.frame from them.
file.list <- list.files(directory, pattern = "*.jpg", full.names = TRUE)
file.name <- gsub(directory, "", file.list)
df <- data.frame(file.name) 

# for loop to create the dataframe from the files in the directory.
for (i in 1:length(file.name)) {
  df$s.date[i] <- substr(df$file.name[i], 1, 10)
  df$e.date[i] <- substr(df$file.name[i], 12, 21)
  df$p.num[i] <- substr(df$file.name[i], 27, 28)
  df$s.year[i] <- substr(df$file.name[i], 7, 10)
  df$e.year[i] <- substr(df$file.name[i], 18, 21)
  df$s.month[i] <- substr(df$file.name[i], 1, 2)
  df$e.month[i] <- substr(df$file.name[i], 12, 13)
  df$a1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 0, y_off = 193)) %>% ocr()
  df$a2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 0, y_off = 587)) %>% ocr()
  df$a3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 0, y_off = 980)) %>% ocr()
  df$a4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 0, y_off = 1373)) %>% ocr()
  df$b1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 291, y_off = 193)) %>% ocr()
  df$b2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 291, y_off = 587)) %>% ocr()
  df$b3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 291, y_off = 980)) %>% ocr()
  df$b4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 291, y_off = 1373)) %>% ocr()
  df$c1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 581, y_off = 193)) %>% ocr()
  df$c2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 581, y_off = 587)) %>% ocr()
  df$c3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 581, y_off = 980)) %>% ocr()
  df$c4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 581, y_off = 1373)) %>% ocr()
  df$d1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 871, y_off = 193)) %>% ocr()
  df$d2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 871, y_off = 587)) %>% ocr()
  df$d3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 871, y_off = 980)) %>% ocr()
  df$d4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 871, y_off = 1373)) %>% ocr()

# These measurements are the full grid but it adds a lot of extra random stuff from the product pictures.
  #df$a1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 0)) %>% ocr()
  #df$a2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 394)) %>% ocr()
  #df$a3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 787)) %>% ocr()
  #df$a4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 1180)) %>% ocr()
  #df$b1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 0)) %>% ocr()
  #df$b2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 394)) %>% ocr()
  #df$b3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 787)) %>% ocr()
  #df$b4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 1180)) %>% ocr()
  #df$c1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 0)) %>% ocr()
  #df$c2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 394)) %>% ocr()
  #df$c3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 787)) %>% ocr()
  #df$c4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 1180)) %>% ocr()
  #df$d1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 0)) %>% ocr()
  #df$d2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 394)) %>% ocr()
  #df$d3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 787)) %>% ocr()
  #df$d4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 1180)) %>% ocr()
}

# This pivots the data.frame into a tidy format and also removeds the \n characters and 
# any line that doesn't have a % symbol.
pivot.df <- df %>% 
  pivot_longer(cols = a1:d4,
               names_to = "location",
               values_to = "OCR") %>%
  filter(grepl("%", OCR)) %>%
  mutate(OCR = gsub("\n", " ", OCR))

# Write to .csv file so we can examine it better in Excel and see ways we can make the OCR better.
write.csv(pivot.df,paste0(gsub("Renamed JPGs/", "", directory), "Data.csv"), row.names = TRUE)

# Using this to find words that come up a lot so we can look at specific words if we would like.
wordcloud(pivot.df$OCR) 

# We may want to remove the ® symbol since it appears so often, if not we could 
# use it to get brand names perhaps.

