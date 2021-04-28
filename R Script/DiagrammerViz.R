require(DiagrammeR)
require(readxl)
require(tidyverse)
require(shorty) ## Package on Github

## PoC WD
# Set your working directory where template files are filled out

ff <- file_frame(getwd())

poc <- read_excel(ff$File_path[1], sheet = 1)

dd <- read_excel(ff$File_path[1], sheet = 2)
  
poc_level_1 <- poc %>% 
  select(Starting) %>% 
  rename("Segment" = "Starting")

poc_level_2 <- poc %>% 
  select(End) %>% 
  rename("Segment" = "End")

poc_levels <- union_all(poc_level_1, poc_level_2) %>% 
  distinct_all() %>% 
  mutate("num" = row_number())


poc_join <- poc %>% 
  left_join(poc_levels %>% rename("Starting" = "Segment"), by = "Starting") %>% 
  rename("Starting_position" = "num") %>% 
  left_join(poc_levels %>% rename("End" = "Segment"), by = "End") %>% 
  rename("End_position" = "num") %>% 
  mutate(Detail_ind = if_else(is.na(Detail),1,0)) %>% 
  mutate(string = if_else(Detail_ind == 0, 
                          paste(Starting_position, "[", Starting,"]", "--", Detail, "-->", End_position, "[", End,"]", sep = ""),
                          paste(Starting_position, "[", Starting,"]", "-->", End_position, "[", End,"]", sep = "")))



pl2 <- poc %>% 
  select(`Graph Type`) %>% 
  filter(is.na(`Graph Type`) == F) %>% 
  inner_join(dd, by = "Graph Type") %>% 
  mutate(string = paste(" graph ", `MM type`, "
                        ", sep = "")) %>% 
  select(string)

mmd <- pl2 %>% 
  union_all(poc_join %>% select(string))

mmd <- mmd$string

mermaid(mmd)

