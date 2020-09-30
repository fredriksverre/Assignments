library(rvest)
library(tidyverse)

#laser inn hva jeg vil ha av data 
x<-read_html("http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list")
y<-html_table(html_nodes(x, "div table")[1:14])

#endrer fra list til datasett kode funnet https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
timeplan<- data.frame(matrix(unlist(y), nrow=14, byrow=T))

#beholder nødvendige kolonner
schedule<-subset(timeplan,select = c(X2,X4,X8,X10,X12))

#endrer navn
colnames(schedule)<-c("Date","Time","subject","Room","Teacher")




                    

