


#load libraries
pacman::p_load(quarto, sf, sp, tidyverse)

#load data
data <- read.table(file = "data/Data_Example_1.txt", header = T, sep = "\t")



months <- data %>% 
  distinct(month) %>% 
  pull(month) %>% 
  as.character()

reports <- tibble(
  input = "report.qmd",
  output_file = str_glue("{months}.qmd"),
  execute_params = map(months, ~list(month = .))
  )

pwalk(reports, quarto_render)
