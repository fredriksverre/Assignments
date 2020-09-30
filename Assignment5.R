rm(list=ls())

library(data.table)
library(tidyverse)

# Read the csv data
county_csv <- fread("http://data.ssb.no/api/v0/dataset/95274.csv?lang=no")
head(county_csv)

whole_country_csv <- fread("http://data.ssb.no/api/v0/dataset/95293.csv?lang=no")
head(whole_country_csv)
# ugly
rm(county_csv, whole_country_csv)

# Or reading json, the whole country
library(rjstat)
url <- "http://data.ssb.no/api/v0/dataset/95276.json?lang=no"
results <- fromJSONstat(url)
table <- results[[1]]
table

###############################################################
rm(list = ls())

#install.packages("PxWebApiData")
library(PxWebApiData)

?ApiData

county <- ApiData("http://data.ssb.no/api/v0/dataset/95274.json?lang=no",
                  getDataByGET = TRUE)

whole_country <- ApiData("http://data.ssb.no/api/v0/dataset/95276.json?lang=no",
                         getDataByGET = TRUE)

# two similar lists, different labels and coding
head(county[[1]])
head(county[[2]])

head(whole_country[[1]])

# Use first list, rowbind both data
dframe <- bind_rows(county[[1]], whole_country[[1]])


# new names, could have used dplyr::rename()
names(dframe)
names(dframe) <- c("region", "date", "variable", "value")
str(dframe)

# Split date
dframe <- dframe %>% separate(date, 
                              into = c("year", "month"), 
                              sep = "M")
head(dframe)

# Make a new proper date variable
library(lubridate)
dframe <- dframe %>%  mutate(date = ymd(paste(year, month, 1)))
str(dframe)

# And how many levels has the variable?
dframe %>% select(variable) %>% unique()

# car::recode()
#dframe <- dframe %>%  mutate(variable1 = car::recode(dframe$variable,
                                                     ' "Utleigde rom"="rentedrooms";
"Pris per rom (kr)"="roomprice";
"Kapasitetsutnytting av rom (prosent)"="roomcap";
"Kapasitetsutnytting av senger (prosent)"="bedcap";
"Losjiomsetning (1 000 kr)"="revenue";
"Losjiomsetning per tilgjengeleg rom (kr)"="revperroom";
"Losjiomsetning, hittil i Ã¥r (1 000 kr)"="revsofar";
"Losjiomsetning per tilgjengeleg rom, hittil i Ã¥r (kr)"="revroomsofar";
"Pris per rom hittil i Ã¥r (kr)"="roompricesofar";
"Kapasitetsutnytting av rom hittil i Ã¥r (prosent)"="roomcapsofar";
"Kapasitetsutnytting av senger, hittil i Ã¥r (prosent)"="bedcapsofar" '))

dframe %>% select(variable) %>% unique()
with(dframe, table(variable, variable1))

# dplyr::recode()
#dframe <- dframe %>% mutate(variable = dplyr::recode(variable,
                                                      "Utleigde rom"="rentedrooms",
                                                      "Pris per rom (kr)"="roomprice",
                                                      "Kapasitetsutnytting av rom (prosent)"="roomcap",
                                                      "Kapasitetsutnytting av senger (prosent)"="bedcap",
                                                      "Losjiomsetning (1 000 kr)"="revenue",
                                                      "Losjiomsetning per tilgjengeleg rom (kr)"="revperroom",
                                                      "Losjiomsetning, hittil i Ã¥r (1 000 kr)"="revsofar",
                                                      "Losjiomsetning per tilgjengeleg rom, hittil i Ã¥r (kr)"="revroomsofar",
                                                      "Pris per rom hittil i Ã¥r (kr)"="roompricesofar",
                                                      "Kapasitetsutnytting av rom hittil i Ã¥r (prosent)"="roomcapsofar",
                                                      "Kapasitetsutnytting av senger, hittil i Ã¥r (prosent)"="bedcapsofar"))

dframe %>% select(variable2) %>% unique()
with(dframe, table(variable, variable2))

# or mutate & ifelse, a bit cumbersome, but flexible
#dframe <- 
 # dframe %>%
 # mutate(variable3 =
 #          ifelse(variable == "Utleigde rom", "rentedrooms",
#                  ifelse(variable == "Pris per rom (kr)", "roomprice",
#                         ifelse(variable == "Kapasitetsutnytting av rom (prosent)", "roomcap",
 #                               ifelse(variable == "Kapasitetsutnytting av senger (prosent)", "bedcap",
 #                                      ifelse(variable == "Losjiomsetning (1 000 kr)", "revenue",
 #                                             ifelse(variable == "Losjiomsetning per tilgjengeleg rom (kr)", "revperroom",
 #                                                    ifelse(variable == "Losjiomsetning, hittil i Ã¥r (1 000 kr)", "revsofar",
 #                                                           ifelse(variable == "Losjiomsetning per tilgjengeleg rom, hittil i Ã¥r (kr)", "revroomsofar",
 #                                                                  ifelse(variable == "Pris per rom hittil i Ã¥r (kr)", "roompricesofar",
  #                                                                        ifelse(variable == "Kapasitetsutnytting av rom hittil i Ã¥r (prosent)", "roomcapsofar", "bedcapsofar")))))))))))


dframe %>% select(variable) %>% unique()
with(dframe, table(variable, variable3))


# recode region
dframe <- dframe %>% mutate(region = 
                              ifelse(region == "Hele landet",
                                     "Whole country", region))

mosaic::tally(~region, data = dframe)

# we now have the data in long format ready for data wrangling

dframe %>% select(region) %>% unique()

dframe <- dframe %>% mutate(region = dplyr::recode(region,"Trøndelag - Trööndelage"="Trøndelag",
                                                      "Troms og Finnmark - Romsa ja Finnmárku"="Troms og Finnmark",
                                                   "Heile landet"="Whole contry"))
dframe %>% select(variable) %>% unique()

dframe <- dframe %>% mutate(variable = dplyr::recode(variable,
                                                     "Utleigde rom"="rentedrooms",
                                                     "Pris per rom (kr)"="roomprice",
                                                     "Kapasitetsutnytting av rom (prosent)"="roomcap",
                                                     "Kapasitetsutnytting av senger (prosent)"="bedcap",
                                                     "Losjiomsetning (1 000 kr)"="revenue",
                                                     "Losjiomsetning per tilgjengeleg rom (kr)"="revperroom",
                                                     "Losjiomsetning, hittil i år (1 000 kr)"="revsofar",
                                                     "Losjiomsetning per tilgjengeleg rom, hittil i år (kr)"="revroomsofar",
                                                     "Pris per rom hittil i år (kr)"="roompricesofar",
                                                     "Kapasitetsutnytting av rom hittil i år (prosent)"="roomcapsofar",
                                                     "Kapasitetsutnytting av senger, hittil i år (prosent)"="bedcapsofar"))

                                                     
 #tar koden fra lærer øystein som didligere brukt                                                                                                         

dframe %>%
  filter(variable == "roomcap") %>%
  ggplot(aes(x=date, y=value, group=region)) +
  geom_line(aes(color=region)) +
  ylab(expression("Percent occupied ")) +
  xlab("") +
  labs(title = "Busy hotels 2020 in Noway",
       subtitle = "Busy hotels from Jan to Jul 2020",
       caption = "percentage occupied in different counties ") +
  theme_grey()




