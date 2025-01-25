

require(DiagrammeR)
require(readxl)
require(tidyverse)
require(criticalpath)
require(shorty) ## Package on Github

## PoC WD
# Set your working directory where template files are filled out

setwd("~/R Projects/My Projects/Text_to_Mermaid_Viz/Data Model Diagram")

ff <- file_frame(getwd())

poc <- read_excel(ff$File_path[4], sheet = 1)

dd <- read_excel(ff$File_path[4], sheet = 2)

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
                          paste(Starting_position, "[", Starting, "<br> ", Duration, " Weeks","]", "-->", End_position, "[", End,"]", sep = "")))

## Inputs
sc <- union_all(poc_join %>% select(Starting) %>% rename("segment" = "Starting")
                , poc_join %>% select(End) %>% rename("segment" = "End")) %>% 
  distinct_all()

sc2 <- sc %>%
  left_join(poc_join %>% select(Starting,Duration) %>% rename("segment" = "Starting"), by = "segment") %>%
  distinct_all()

sc_Missing <- sc2 %>%
  filter(is.na(Duration)) %>% 
  select(segment) %>% 
  left_join(poc_join %>% 
              select(End,Duration) %>%
              rename("segment" = "End"), by = "segment") %>% 
  distinct_all()


task_duration <- sc2 %>% 
  left_join(sc_Missing, by = "segment") %>%
  mutate(Duration = if_else(is.na(Duration.x) == T, Duration.y, Duration.x)) %>% 
  select(segment, Duration)

task_duration2 <- as.integer(task_duration$Duration)


max_level <- nrow(task_duration)

Task_Names <- task_duration$segment

relationships_Start <- poc_join$Starting_position

relationships_end <- poc_join$End_position





## Schedule Creation
sch_dynamic <- sch_new() %>% 
  sch_title("Process review") %>% 
  sch_add_activities(
    id = 1:max_level
    ,name = Task_Names
    ,duration = task_duration2
  ) %>% 
  sch_add_relations(
    from = relationships_Start
    ,to = relationships_end
  ) %>% 
  sch_plan()


Duration_Time <- sch_duration(sch_dynamic)

# sch_activities(sch_dynamic)

Relationship_order <- sch_relations(sch_dynamic)

## Critical Path Isolation

Path_table <- sch_activities(sch_dynamic)

crit_path <- Path_table %>% filter(critical == T) %>% select(1:11)


##**************************************************************************************


## Create Visual

poc_join2 <- poc %>%
  left_join(poc_levels %>% rename("Starting" = "Segment"), by = "Starting") %>%
  rename("Starting_position" = "num") %>%
  left_join(poc_levels %>% rename("End" = "Segment"), by = "End") %>%
  rename("End_position" = "num") %>%
  left_join(crit_path %>%
              filter(duration > 0) %>%
              rename("Starting" = "name") %>%
              select(Starting,critical), by = "Starting") %>%
  mutate(critical = replace_na(critical, F)) %>%
  mutate(Detail_ind = if_else(is.na(Detail),1,0)) %>%
  mutate(string = if_else(Detail_ind == 0,
                          paste(Starting_position, "[", Starting,"]", "--", Detail, "-->", End_position, "[", End,"]", sep = ""),
                          if_else(critical == T
                                  ,paste(Starting_position, "[", Starting, "<br> ", Duration, " Weeks Critical","]", "-->", End_position, "[", End,"]", sep = "")
                                  ,paste(Starting_position, "[", Starting, "<br> ", Duration, " Weeks","]", "-->", End_position, "[", End,"]", sep = "")
                          )
  )
  ) %>% distinct_all()


pl2 <- poc %>% 
  select(`Graph Type`) %>% 
  filter(is.na(`Graph Type`) == F) %>% 
  inner_join(dd, by = "Graph Type") %>% 
  mutate(string = paste(" graph ", `MM type`, "
                        ", sep = "")) %>% 
  select(string)

mmd <- pl2 %>% 
  union_all(poc_join2 %>% select(string))

mmd <- mmd$string

mermaid(mmd)

