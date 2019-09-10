library(bupaR)

# https://www.bupar.net/processmaps.html


patients

patients %>%
  process_map(type = frequency("absolute"))
