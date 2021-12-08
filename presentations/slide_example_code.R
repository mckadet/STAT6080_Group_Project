file.list <- list.files(directory, pattern = "*.jpg", full.names = TRUE)
file.name <- gsub(directory, "", file.list)
for (i in 1:length(file.name)) {
  df$s.date[i] <- substr(df$file.name[i], 1, 10)
  df$e.date[i] <- substr(df$file.name[i], 12, 21)
  df$p.num[i] <- substr(df$file.name[i], 27, 28)
  df$s.year[i] <- substr(df$file.name[i], 7, 10)
  df$e.year[i] <- substr(df$file.name[i], 18, 21)
  df$s.month[i] <- substr(df$file.name[i], 1, 2)
  df$e.month[i] <- substr(df$file.name[i], 12, 13)
  df$a1[i] <- paste0(directory, df$file.name[i]) %>% image_read() %>% 
    image_crop(geometry = geometry_area(width = 290, height = 200, x_off = 0, y_off = 193)) %>% ocr()
}