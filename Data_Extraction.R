library(tesseract)
library(magick)
library(magrittr)
library(imagefx)
library(pdftools)
library(magick)
library(tidyverse)
library(RColorBrewer)

# SET DIRECTORY TO WHERE YOU HAVE THE FILES STORED ON YOUR COMPUTER
directory <- paste0("C:/Users/n8nel/Desktop/STAT 5080/",
                    "Project/STAT6080_Group_Project/Renamed JPGs/")

# grab all JPGs from the directory and create data frame
file.list <- list.files(directory, pattern = "*.jpg", full.names = TRUE)
file.name <- gsub(directory, "", file.list)
df <- data.frame(file.name) 

# for loop to create the data frame from the files in the directory.
for (i in 1:length(file.name)) {
  df$s.date[i] <- substr(df$file.name[i], 1, 10)
  df$e.date[i] <- substr(df$file.name[i], 12, 21)
  df$p.num[i] <- substr(df$file.name[i], 27, 28)
  df$s.year[i] <- substr(df$file.name[i], 7, 10)
  df$e.year[i] <- substr(df$file.name[i], 18, 21)
  df$s.month[i] <- substr(df$file.name[i], 1, 2)
  df$e.month[i] <- substr(df$file.name[i], 12, 13)
  df$a1[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 0, 
                                        y_off = 193)) %>% ocr()
  df$a2[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 0, 
                                        y_off = 587)) %>% ocr()
  df$a3[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 0, 
                                        y_off = 980)) %>% ocr()
  df$a4[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 0, 
                                        y_off = 1373)) %>% ocr()
  df$b1[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 291, 
                                        y_off = 193)) %>% ocr()
  df$b2[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200,
                                        x_off = 291, 
                                        y_off = 587)) %>% ocr()
  df$b3[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 291, 
                                        y_off = 980)) %>% ocr()
  df$b4[i] <- paste0(directory, df$file.name[i]) %>%
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 291, 
                                        y_off = 1373)) %>% ocr()
  df$c1[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290,
                                        height = 200, 
                                        x_off = 581,
                                        y_off = 193)) %>% ocr()
  df$c2[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290,
                                        height = 200, 
                                        x_off = 581,
                                        y_off = 587)) %>% ocr()
  df$c3[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 581,
                                        y_off = 980)) %>% ocr()
  df$c4[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 581,
                                        y_off = 1373)) %>% ocr()
  df$d1[i] <- paste0(directory, df$file.name[i]) %>%
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 871, 
                                        y_off = 193)) %>% ocr()
  df$d2[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 871, 
                                        y_off = 587)) %>% ocr()
  df$d3[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200,
                                        x_off = 871, 
                                        y_off = 980)) %>% ocr()
  df$d4[i] <- paste0(directory, df$file.name[i]) %>% 
    image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, 
                                        height = 200, 
                                        x_off = 871, 
                                        y_off = 1373)) %>% ocr()
}

# pivot the data frame into a tidy format and also remove \n characters 
# remove lines without % symbol.
pivot.df <- df %>% 
  pivot_longer(cols = a1:d4,
               names_to = "location",
               values_to = "OCR") %>%
  filter(grepl("%", OCR)) %>%
  mutate(OCR = gsub("\n", " ", OCR))

save(pivot.df, file = "RData.Rda")

