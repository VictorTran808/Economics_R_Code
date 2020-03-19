setwd("C:/Users/calamity/Documents/R_Research/Economics_R_Code")

library(pdftools)
library(tidyverse)

?pdf_text
PV_text <- pdf_text("C:/Users/calamity/Documents/R_Research/Economics_R_Code/Dataset/Solar_PV_Installation_In_Honolulu_Sep2017.pdf")%>%
  readr::read_lines()

PV_text

view(PV_text)

table_5 <- PV_text[467:503]
table_5

view(table_5)
