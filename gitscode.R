library(readxl)
library(sqldf)
library(WriteXLS)
library(phonics)
library(dplyr)
library(data.table)
set.seed(123)
setwd("~/Desktop/")
NAUKRISCRAPER <- read_excel("NAUKRIBANGALORE.xlsx",col_names = FALSE)
#make sure that the excel file has only 1 coloumn 
#and it is pasted as text from the special type of pastes
#also ensure input file first row is blank
#the below steps will add 2 more coloumns to the df which are copies of the data in the first coloumn
#of the df
df = NAUKRISCRAPER
fix(df)
df$X__2<-df$X__1[1:nrow(df)]
names(df)[names(df) == "X__1"]<-"d1"
names(df)[names(df) == "X__2"]<-"d2"
df$X__4<-NULL
df$X__5<-NULL
df$X__6<-NULL
df$X__3<-NULL

#this step will align the target stringe ie. w,xp,co and loc into the same row for easier extraction
#it will do this by shifting down the newly created coloumns by 1 and 2 from their orignal place
df = transform(df,d2 = lead(d2))
#lag and lead are to shift cells up or down by 1
df$d3 = paste(df$d1, df$d2, sep="~")
#paste4 function is for concatenation
wxplocco<- df$d3[grep("~Current",df$d3)]
wxplocco<-tstrsplit(wxplocco,"~",names = TRUE)
names(wxplocco)<-c("wxploc","co")


#treating each coloumn
#this step of gsub selects the rows which have strings associated with
#the regex "Tata" or "Capgemini"
wxplocco$co<-gsub(".* at \\s*|Education.*", "", wxplocco$co)
#treating the coloumns of company by eliminating adjacent words
co<-sub(pattern = "Limited",replacement = "",x=wxplocco$co)
co=sub(pattern = "limited",replacement = "",x=wxplocco$co)
co=sub(pattern = "Ltd",replacement = "",x=wxplocco$co)
co=sub(pattern = "LTD",replacement = "",x=wxplocco$co)
co=sub(pattern = "ltd",replacement = "",x=wxplocco$co)
co=sub(pattern = "Private",replacement = "",x=wxplocco$co)
co=sub(pattern = "private",replacement = "",x=wxplocco$co)
co=sub(pattern = "Pvt",replacement = "",x=wxplocco$co)
co=sub(pattern = "PVT",replacement = "",x=wxplocco$co)
co=sub(pattern = "pvt",replacement = "",x=wxplocco$co)
co=sub(pattern = "Inc",replacement = "",x=wxplocco$co)
co=sub(pattern = "inc",replacement = "",x=wxplocco$co)
co=sub(pattern = "India",replacement = "",x=wxplocco$co)
co=sub(pattern = "india",replacement = "",x=wxplocco$co)
co<-gsub("[^a-z A-Z]","",wxplocco$co)
#treating the string which contains wages,experience and location
#the first 5 characters are xp and the 7:10 is wages
xp<- substr(wxplocco$wxploc,0,5)
xp<-sub(pattern = "yr",replacement = ".",x=xp)
xp<-sub(pattern = " ",replacement = "",x=xp)
xp<-as.numeric(xp)
w<-substr(wxplocco$wxploc,7,10)
w<-sub(pattern = "L",replacement = "",x=w)
w<-sub(pattern = "m",replacement = "",x=w)
w<-sub(pattern = "+",replacement = "",x=w)
w<-sub(pattern = "L",replacement = "",x=w)
w<-sub(pattern = "m",replacement = "",x=w)
w<-sub(pattern = "+",replacement = "",x=w)
loc<-substr(wxplocco$wxploc,12,100)
loc<-sub(pattern = "Lacs",replacement = "",x=loc)
loc<-sub(pattern = "Lacs",replacement = "",x=loc)
loc<-sub(pattern = "acs",replacement = "",x=loc)
loc<-sub(pattern = "cs",replacement = "",x=loc)
loc<-sub(pattern = "0",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
#normalisng city names
loc<-toupper(loc)
loc<-sub(pattern = "NOIDA",replacement = "DELHI",x=loc)
loc<-sub(pattern = "FARDIBAD",replacement = "DELHI",x=loc)
loc<-sub(pattern = "DELHI",replacement = "DELHI",x=loc)
loc<-sub(pattern = "GHAZIABAD",replacement = "DELHI",x=loc)
loc<-sub(pattern = "GURGAON",replacement = "DELHI",x=loc)
loc<-sub(pattern = "MUMBAISUBURBS",replacement = "MUMBAI",x=loc)
loc<-sub(pattern = "NAVIMUMBAI",replacement = "MUMBAI",x=loc)
loc<-sub(pattern = "COIMBATORE",replacement = "CHENNAI",x=loc)
loc<-sub(pattern = "GUNTUR",replacement = "HYDERABAD",x=loc)
loc<-sub(pattern = "VISAKHAPATNAM",replacement = "HYDERABAD",x=loc)
loc<-sub(pattern = "VIJAYAWADA",replacement = "HYDERABAD",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)
loc<-sub(pattern = " ",replacement = "",x=loc)

loc<-gsub("[^a-z A-Z]","",loc)
df=data.frame(cbind(w,xp,loc,co))

df$xp<-as.character(df$xp)
df$xp<-as.numeric(df$xp)
df$loc<-as.factor(df$loc)
df$w<-as.character(df$w)
df$w<-as.numeric(df$w)
dota<-df
#removing rows conditionally

df = dota
Compensationcalculator <- dota