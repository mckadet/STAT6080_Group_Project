library(tesseract)
library(magick)
library(magrittr)
library(imagefx)
library(pdftools)
library(magick)

ad_test <- image_read("C:/Users/n8nel/Downloads/2752687-1200-100000.jpg")
text_ad_test <- ocr(ad_test)
cat(text_ad_test)

a1 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 150,
                                                   x_off = 50,
                                                   y_off = 590))
a2 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 125,
                                                   x_off = 50,
                                                   y_off = 975))
a3 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 125,
                                                   x_off = 50,
                                                   y_off = 1325))
b1 <- image_crop(ad_test, geometry = geometry_area(width = 275,
                                                   height = 150,
                                                   x_off = 330,
                                                   y_off = 590))
b2 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 125,
                                                   x_off = 330,
                                                   y_off = 960))
b3 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 130,
                                                   x_off = 330,
                                                   y_off = 1310))
c1 <- image_crop(ad_test, geometry = geometry_area(width = 275,
                                                   height = 150,
                                                   x_off = 600,
                                                   y_off = 575))
c2 <- image_crop(ad_test, geometry = geometry_area(width = 275,
                                                   height = 125,
                                                   x_off = 620,
                                                   y_off = 975))
c3 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 150,
                                                   x_off = 620,
                                                   y_off = 1300))
d1 <- image_crop(ad_test, geometry = geometry_area(width = 275,
                                                   height = 150,
                                                   x_off = 850,
                                                   y_off = 600))
d2 <- image_crop(ad_test, geometry = geometry_area(width = 275,
                                                   height = 125,
                                                   x_off = 870,
                                                   y_off = 945))
d3 <- image_crop(ad_test, geometry = geometry_area(width = 300,
                                                   height = 150,
                                                   x_off = 870,
                                                   y_off = 1300))
text_a1 <- ocr(a1)
cat(text_a1)
text_a2 <- ocr(a2)
cat(text_a2)
text_a3 <- ocr(a3)
cat(text_a3)
text_b1 <- ocr(b1)
cat(text_b1)
text_b2 <- ocr(b2)
cat(text_b2)
text_b3 <- ocr(b3)
cat(text_b3)
text_c1 <- ocr(c1)
cat(text_c1)
text_c2 <- ocr(c2)
cat(text_c2)
text_c3 <- ocr(c3)
cat(text_c3)
text_d1 <- ocr(d1)
cat(text_d1)
text_d2 <- ocr(d2)
cat(text_d2)
text_d3 <- ocr(d3)
cat(text_d3)
###############################################################################
# Extra code that I am still working with.




image_write(ad_test, "2752687-1200-100000.pdf", format = "pdf")

# based on https://www.r-bloggers.com/2017/08/tesseract-and-magick-high-quality-ocr-in-r/

ad_textProcessed <- ad_test %>%
  image_resize("2000") %>%
  image_convert(colorspace = "gray") %>%
  image_trim()

ad_textOCR <- ad_textProcessed %>%
  image_ocr()

cat(ad_textOCR)


ad_textOCRData <- ad_textProcessed %>%
  image_ocr_data()

print(ad_textOCRData, n = 10)


### - this one is the best

library(pdftools)

ad_textOCR2 <- pdf_ocr_text("2752687-1200-100000.pdf")

cat(ad_textOCR2)



# This may be helpful to clean up the image first...
# https://cran.r-project.org/web/packages/imagerExtra/vignettes/OCR_with_imagerExtra.html

library(imager)
library(imagerExtra)

plot(ad_test)

ad_testClean <- ad_test %>%
  magick2cimg(.) %>%
  grayscale(.) %>%
  DenoiseDCT(., 0.01) %>% 
  ThresholdAdaptive(., 0.1, range = c(0, 1))
# requires tuning of parameters !!!

plot(ad_testClean)

OCR(ad_testClean)

ad_textOCRData2 <- OCR_data(ad_testClean)

print(ad_textOCRData2, n = 10)



# image_modulate()
# image_noise()