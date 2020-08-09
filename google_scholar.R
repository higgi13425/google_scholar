
library(tidyverse)
library(scholar)
library(DT)

##--------------
#plan to build a function (calc_indices) that takes google scholar ID as argument,
#outputs DT of head of
#Author, total cites (max(cumsum)), h, g, i10, i50, i100, hc, m= slope of h

#could also use purrr with function and a vector of ids
#  do       map_df(id_list, calc_indices)
##------------


#Google scholar ID for author Peter DR Higgins
#take from web link, user = GOBBLEDYGOOK
id1<- "UGJGFaAAAAAJ"

#Google scholar ID for Akbar Waljee
id2<- "JIBnulgAAAAJ"

#Google scholar ID for Shail Govani
id3<- "EIkJanYAAAAJ"

#Google scholar ID for Krishna Rao
id4<- "p6YtnDoAAAAJ"

#Google scholar ID for Shrinivas Bishu
id5<- "uxFxamsAAAAJ"

#Google scholar ID for Calen Steiner
id6<- "SrZ7n1cAAAAJ"


#list of scholar IDs
id_list <- c(id, id2, id3, id4, id5, id6)

#set id
id <- id5

profile <- get_profile(id)
get_num_distinct_journals(id)
get_num_articles(id)
get_citation_history(id)
get_oldest_article(id)

# vector of journals with impact factor in field of 10 or above
journals <- c("Gastroenterology", "Gut", "American Journal of Gastroenterology",
              "Clinical Journal of Gastroenterology", "New England Journal of Medicine",
              "Annals of Internal Medicine", "The Lancet", "JAMA", "JAMA Internal Medicine",
              "Journal of Clinical Investigation", "Science Translational Medicine",
              "Nature Medicine", "Nature Reviews Gastroenterology and Hepatology",
              "Lancet Infectious Diseases", "Clinical Infectious Diseases")
get_num_top_journals(id, journals)


#note that pubs includes abstracts in gastro, all with "S"
# also abstracts in Gut from UEGW until UEGJ started
# abstracts in JCC from ECCO - this one does not matter

#likely inflates prediction
predict_h_index(id, journals)

#test by taking Gastro out of journals list
journals2 <- c("Gut", "American Journal of Gastroenterology",
               "Clinical Journal of Gastroenterology", "New England Journal of Medicine",
               "Annals of Internal Medicine", "The Lancet", "JAMA", "JAMA Internal Medicine",
               "Journal of Clinical Investigation", "Science Translational Medicine",
               "Nature Medicine", "Nature Reviews Gastroenterology and Hepatology")
predict_h_index(id, journals2)

#test by taking Gastro and Gut out of journals list
journals3 <- c("American Journal of Gastroenterology",
               "Clinical Journal of Gastroenterology", "New England Journal of Medicine",
               "Annals of Internal Medicine", "The Lancet", "JAMA", "JAMA Internal Medicine",
               "Journal of Clinical Investigation", "Science Translational Medicine",
               "Nature Medicine", "Nature Reviews Gastroenterology and Hepatology",
               "Lancet Infectious Diseases", "Clinical Infectious Diseases")
predict_h_index(id, journals3)

#now predictions for Peter
predict_h_index(id1, journals)

#now predictions for Akbar
predict_h_index(id2, journals)

#now predictions for Shail
predict_h_index(id3, journals)

#now predictions for Krishna
predict_h_index(id4, journals)

#now predictions for Calen
predict_h_index(id6, journals)

#now predictions for Shrinivas
predict_h_index(id5, journals)

#---------------------
#function calc_indices
#calc_indices <- {
  library(scholar)
  library(dplyr)
  library(DT)
pubs<- get_publications(id, cstart = 0, pagesize = 1000, flush = FALSE)
#note that some pubs have year = NA. Clean up.
pubs$year[is.na(pubs$year)]<- 0

#add author
profile <- get_profile(id)
pubs$author <- profile$name


pubs$cumsum <- cumsum(pubs$cites)

pubs$citerank <- get_num_articles(id) - rank(pubs$cites, ties.method = "last") +1

pubs$htest <- (pubs$cites - pubs$citerank) >=0
pubs$hvalue <- sum(pubs$htest)

#note that Hirsch estimated that for physics, at 20 y of career
# successful scientist h=20
# outstanding scientist h=40
# truly unique h=60

pubs$gtest <- (pubs$cumsum -pubs$citerank^2) >=0
pubs$gvalue <- sum(pubs$gtest)

pubs$i10 <- pubs$cites >10
pubs$i10value <- sum(pubs$i10)

pubs$i50 <- pubs$cites >50
pubs$i50value <- sum(pubs$i50)

pubs$i100 <- pubs$cites >100
pubs$i100value <- sum(pubs$i100)

current_year <- as.integer(format(Sys.Date(), "%Y"))
pubs$age_weight <- 4 * (current_year - pubs$year+1)^-1
miss_year <- sum(is.na(pubs$year))
pubs$hc <- round(pubs$cites * pubs$age_weight, 2)
#pubs$hc[is.na(pubs$hc)]<- 0

hcrank<- c(1:get_num_articles(id))
pubs <- pubs %>%
  arrange(desc(hc)) %>% cbind(hcrank)

pubs <- pubs %>% 
  mutate(hctest = (hc - hcrank) >=0) %>%
  mutate(hcvalue = sum(hctest))

# hslope or m index - h index/publishing years
start_year <- min(pubs$year[pubs$year >1965])
pubs$hslope = round(pubs$hvalue[1] /(current_year - 2003),2)
pubs$m2 = pubs$hvalue[1] /(current_year - start_year)

#predicted h slope
pred_hslope_vector <- predict_h_index(id, journals)
pubs$pred_hslope <- round((max(pred_hslope_vector$h_index)-
                             min(pred_hslope_vector$h_index))/10, 2)

#output nice table
output <- pubs %>%
  select(author, cumsum, hvalue, gvalue, i10value, i50value, i100value,
         hcvalue, hslope, pred_hslope) %>% filter(cumsum==max(cumsum)) %>%
  head(1) %>%
  datatable(colnames = c("Author", "Citations", "h index", "g index", "i10",
                         "i50", "i100", "hc", "h slope", "pred h slope"))
output

#}

#estimates for M index- slope of h over time
# from http://blogs.plos.org/biologue/2012/10/19/why-i-love-the-h-index/
# <1.0    Average
# 1.0-2.0   Above average
# 2.0-3.0 Excellent
# > 3.0   Stellar