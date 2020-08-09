## pull refs for peter higgins
library(tidyverse)
library(scholar)
library(DT)
library(magrittr)
##--------------
#plan to build a function (calc_indices) that takes google scholar ID as argument,
#outputs DT of head of
#Author, total cites (max(cumsum)), h, g, i10, i50, i100, hc, m= slope of h

#could also use purrr with function and a vector of ids
#  do       map_df(id_list, calc_indices)
##------------


#Google scholar ID for author Peter DR Higgins
#take from web link, user = GOBBLEDYGOOK
id<- "UGJGFaAAAAAJ"

pubs<- get_publications(id, cstart = 0, pagesize = 1000, flush = FALSE)

## filter out those without Higgins in author field----
pubs %>% 
  mutate(title = str_trim(title)) %>% 
  mutate(author = str_trim(author)) ->
pubs

## identify posters -----
pubs$poster <- 0

pubs %>% 
  mutate(poster = case_when(
    str_detect(title, "^W\\d\\d|^P\\d\\d|^T\\d\\d|^M\\d\\d|^S\\d\\d|^Su\\d\\d|^Sa\\d\\d|^Mo\\d\\d|^Tu\\d\\d|^A\\d\\d|^P-\\d\\d|^8") ~ 1,
    TRUE ~ 0
  )) ->
pubs

## identify oral podium presentations -----
pubs$podium <- 0

pubs %>% 
  mutate(podium = case_when(
    str_detect(title, "^\\d\\d|^OP\\d\\d|^DOP\\d\\d|^O-\\d\\d|^O\\d\\d") ~ 1,
    TRUE ~ 0
  )) ->
  pubs

## identify meeting abstracts -----
pubs$abstract <- 0
pubs$abstract[pubs$poster ==1] <- 1
pubs$abstract[pubs$podium ==1] <- 1

pubs$manuscript <- 0
pubs$manuscript[pubs$abstract == 0] <- 1

pubs$poster[pubs$podium == 1] <-0

## create table output to confirm ----
pubs %>% 
  tabyl(manuscript, abstract) %>% 
  adorn_title()

pubs %>% 
  tabyl(poster,podium) %>% 
  adorn_title()

pubs %>% 
  tabyl(abstract,poster) %>% 
  adorn_title()

## list of oral podium presentations ----
pubs %>% 
  filter(podium == 1) %>% 
  arrange(year) %>% 
  glue::glue_data("{author}. {title}. {journal}. {year}; {number}. Citations: {cites}")

## list of poster presentations ----
pubs %>% 
  filter(poster == 1) %>% 
  arrange(year) %>% 
  glue::glue_data("{author}. {title}. {journal}. {year}; {number}. Citations: {cites}")

## list of manuscripts ----
pubs %>% 
  filter(manuscript == 1) %>% 
  arrange(year) %>% 
  glue::glue_data("{author}. {title}. {journal}. {year}; {number}. Citations: {cites}")