library(readxl)
library(sqldf)
library(WriteXLS)
library(phonics)
library(dplyr)
library(data.table)

set.seed(123)
setwd("~/Desktop/")
NAUKRISCRAPER <- read_excel("NAUKRIALLINDIA.xlsx",col_names = FALSE)
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

dota$loc<-toupper(dota$loc)

#remaineder of code will be in the same order.

yr0xp<-sqldf("select loc from dota where xp = 0")
yr0xp<-table(yr0xp)
bang0<-yr0xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr1xp<-sqldf("select loc from dota where xp = 1")
yr1xp<-table(yr1xp)
bang1<-yr1xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]


yr2xp<-sqldf("select loc from dota where xp=2")
yr2xp<-table(yr2xp)
bang2<-yr2xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]


yr3xp<-sqldf("select loc from dota where xp=3")
yr3xp<-table(yr3xp)
bang3<-yr3xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]


yr4xp<-sqldf("select loc from dota where xp=4")
yr4xp<-table(yr4xp)
bang4<-yr4xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr5xp<-sqldf("select loc from dota where xp=5")
yr5xp<-table(yr5xp)
bang5<-yr5xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr6xp<-sqldf("select loc from dota where xp=6")
yr6xp<-table(yr6xp)
bang6<-yr6xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr7xp<-sqldf("select loc from dota where xp=7")
yr7xp<-table(yr7xp)
bang7<-yr7xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]


yr8xp<-sqldf("select loc from dota where xp=8")
yr8xp<-table(yr8xp)
bang8<-yr8xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr9xp<-sqldf("select loc from dota where xp=9")
yr9xp<-table(yr9xp)
bang9<-yr9xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr10xp<-sqldf("select loc from dota where xp=10")
yr10xp<-table(yr10xp)
bang10<-yr10xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr11xp<-sqldf("select loc from dota where xp=11")
yr11xp<-table(yr11xp)
bang11<-yr11xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr12xp<-sqldf("select loc from dota where xp=12")
yr12xp<-table(yr12xp)
bang12<-yr12xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr13xp<-sqldf("select loc from dota where xp=13")
yr13xp<-table(yr13xp)
bang13<-yr13xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr14xp<-sqldf("select loc from dota where xp=14")
yr14xp<-table(yr14xp)
bang14<-yr14xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr15xp<-sqldf("select loc from dota where xp=15")
yr15xp<-table(yr15xp)
bang15<-yr15xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

yr16xp<-sqldf("select loc from dota where xp=16")
yr16xp<-table(yr16xp)
bang16<-yr16xp[c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")]

bang0<-as.character(bang0)
bang1<-as.character(bang1)
bang2<-as.character(bang2)
bang3<-as.character(bang3)
bang4<-as.character(bang4)
bang5<-as.character(bang5)
bang6<-as.character(bang6)
bang7<-as.character(bang7)
bang8<-as.character(bang8)
bang9<-as.character(bang9)
bang10<-as.character(bang10)
bang11<-as.character(bang11)
bang12<-as.character(bang12)
bang13<-as.character(bang13)
bang14<-as.character(bang14)
bang15<-as.character(bang15)
bang16<-as.character(bang16)

allxploc<-data.frame(cbind(bang0,bang1,bang2,bang3,bang4,bang5,bang6,bang7,bang8,bang9,bang10,bang11,bang12,bang13,bang14,bang15,bang16))
colnames(allxploc) <-c("0yr","1yr","2yr","3yr","4yr","5yr","6yr","7yr","8yr","9yr","10yr","11yr","12yr","13yr","14yr","15yr","16yr")
row.names(allxploc)<- c("BANGALORE","CHENNAI","HYDERABAD","MUMBAI","PUNE","DELHI","KOLKATA")
allxploc<-t(allxploc)
allxploc<-as.data.frame(allxploc)
allxploc$co.x<-rownames(allxploc)
allxploc$code<-""
allxploc$co.y<-""

#all years experience in top 7 cities

# pivot2 viz company and loc using compsoundex.xls
B <- sqldf("select co from dota where loc = 'BANGALORE'")
C <- sqldf("select co from dota where loc = 'CHENNAI'")
H <- sqldf("select co from dota where loc = 'HYDERABAD'")
M <- sqldf("select co from dota where loc = 'MUMBAI'")
P <- sqldf("select co from dota where loc = 'PUNE'")
D <- sqldf("select co from dota where loc = 'DELHI'")
K <- sqldf("select co from dota where loc = 'KOLKATA'")

BANGALORE<- table(B)
CHENNAI<- table(C)
HYDERABAD<- table(H)
MUMBAI<- table(M)
PUNE<- table(P)
DELHI<- table(D)
KOLKATA<- table(K)
locco<-cbind(BANGALORE,CHENNAI,HYDERABAD,MUMBAI,PUNE,DELHI,KOLKATA)
locco<-as.data.frame(locco)
locco$co<-row.names(locco)
locco$code <- soundex(locco$co)

#this file is the list of soundex codes for the company database
compsoundex <- read_excel("compsoundex.xlsx",col_names = T)
compsoundex<-as.data.frame(compsoundex)
locco$code <- soundex(locco$co)
df = left_join(locco,compsoundex,by = "code")
locco = df
names(locco)
loccowrite<-df
loccowrite$BANGALORE <- as.factor(loccowrite$BANGALORE)
loccowrite$CHENNAI <- as.factor(loccowrite$CHENNAI)
loccowrite$HYDERABAD <- as.factor(loccowrite$HYDERABAD)
loccowrite$MUMBAI <- as.factor(loccowrite$MUMBAI)
loccowrite$KOLKATA <- as.factor(loccowrite$KOLKATA)
loccowrite$PUNE <- as.factor(loccowrite$PUNE)
loccowrite$DELHI <- as.factor(loccowrite$DELHI)
rep<-rbind(allxploc,loccowrite)


rep$rows<-rownames(rep)
report1<-rep[c(1:9,10)]

df = report1
names(df)[names(df)=="co.x"] <- "Raw Company names"
names(df)[names(df)=="co.y"] <- "Normalised Company names"
report1 =df
WriteXLS(report1)
View(Compensationcalculator)




