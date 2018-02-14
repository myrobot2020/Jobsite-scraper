#skills and w,xp,loc,co
#importing data
library(readxl)
library(Hmisc)
naukridata<-read_xlsx("NAUKRIALLINDIA.xlsx")
naukridata<-naukridata[1:5]
names(naukridata) <- c("wxploc","co","skill")
naukridata <- na.omit(naukridata[1])
naukridata$co <- naukridata$wxploc
naukridata$skill <- naukridata$wxploc

#3 coloumns for all naukri variables

df = naukridata
df$co <- Lag(df$wxploc,shift = 1)
df$skill <- Lag(df$wxploc,shift = 2)

#selected only relevent strings

newlist <- paste(df$wxploc,df$co,df$skill,sep = "~")
newlist1 <- newlist[agrep("Pref Loc",newlist)]
newlist2 <- newlist1[agrep("Lacs",newlist1)]

#seperating the variables
df= strsplit(newlist2,"~")
df$wxploc<-df[,3]
df= t(df)
View(df)
names(df)<-c("wxploc","co","skill")
names(df)






