library(tesseract)
library(magick)
library(magrittr)
library(imagefx)
library(pdftools)
library(magick)
library(tidyverse)

file.list <- list.files("C:/Users/n8nel/Desktop/STAT 5080/Project/STAT6080_Group_Project/Renamed JPGs", pattern = "*.jpg", full.names = TRUE)

file.name <- gsub("C:/Users/n8nel/Desktop/STAT 5080/Project/STAT6080_Group_Project/Renamed JPGs/", "", file.list)

directory <- "C:/Users/n8nel/Desktop/STAT 5080/Project/STAT6080_Group_Project/Renamed JPGs/"
df <- data.frame(file.name) 

for (i in 1:length(file.name)) {
  df$s.date[i] <- substr(df$file.name[i], 1, 10)
  df$e.date[i] <- substr(df$file.name[i], 12, 21)
  df$p.num[i] <- substr(df$file.name[i], 27, 28)
  df$s.year[i] <- substr(df$file.name[i], 7, 10)
  df$e.year[i] <- substr(df$file.name[i], 18, 21)
  df$s.month[i] <- substr(df$file.name[i], 1, 2)
  df$e.month[i] <- substr(df$file.name[i], 12, 13)
  df$a1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 0)) %>% ocr()
  df$a2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 394)) %>% ocr()
  df$a3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 787)) %>% ocr()
  df$a4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 0, y_off = 1180)) %>% ocr()
  df$b1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 0)) %>% ocr()
  df$b2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 394)) %>% ocr()
  df$b3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 787)) %>% ocr()
  df$b4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 291, y_off = 1180)) %>% ocr()
  df$c1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 0)) %>% ocr()
  df$c2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 394)) %>% ocr()
  df$c3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 787)) %>% ocr()
  df$c4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 581, y_off = 1180)) %>% ocr()
  df$d1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 0)) %>% ocr()
  df$d2[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 394)) %>% ocr()
  df$d3[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 787)) %>% ocr()
  df$d4[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% image_crop(geometry = geometry_area(width = 290, height = 393, x_off = 871, y_off = 1180)) %>% ocr()
}


df$c3[5]

