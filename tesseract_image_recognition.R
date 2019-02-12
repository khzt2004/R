# https://ropensci.org/technotes/2018/11/06/tesseract-40/

library(tesseract)
library(magick)
# If you want to OCR french text:
# tesseract_download('fra')

image_read("index.jpg") %>%
  image_ocr() %>%
  cat()