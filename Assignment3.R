library(rvest)
library(tidyverse)


##laster inn datasett
r<- read_html("https://www.datacamp.com/courses/tech:r")
python<- read_html("https://www.datacamp.com/courses/tech:python")

#henter riktig data ned fra html filene
r<-r %>%
  html_nodes("h4") %>%
  html_text()
p<-python %>%
  html_nodes("h4") %>%
  html_text() 

#her lager jeg en variabel som vi senere knytter til med å bruke r row
LanguageR<-c("r")
LanguageP<-c("Python")

#her knytter jeg til såråket som jeg lagde over
df_r<-cbind(r,LanguageR)
df_python<-cbind(p,LanguageP)

#her kombinerer jeg datasettene
longdf<-rbind(df_r,df_python)

#Endrer colonnenavn til det vi blir spurt om
colnames(longdf)<-c("Tech","Language")



