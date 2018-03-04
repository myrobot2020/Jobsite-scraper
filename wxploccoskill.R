  library(readxl)
  library(data.table)
  
  setwd("~/directory")
  df<-read_xlsx("NAUKBLORE.xlsx")
  df<-na.omit(df)
  df[2] <- df[1]
  df[3] <- df[1]
  
  df[1] <- shift(df[1],n=2)
  df[2] <- shift(df[2],n=1)
  
  names(df) <- c("a","b","c")
  
  d<-paste(df$a,df$b,df$c,sep = "~")
  
  e <- d[grep("Skill",d)]
  f <- e[grep("Lacs",e)]
  g <- f[grep("Current",f)]
  h <-tstrsplit(g,"~")
  h<-data.frame(h)
  colnames(h)<-c("h","co","skill")
  
  edu<-sub(pattern = "Education",replacement = "~",x=h$co)
  edu<-tstrsplit(edu,"~")
  names(edu)<-c("company","education")
  education<-substr(edu$education,0,12)
  education<-!grepl(pattern = "B.",x = skilllist)
  
  
  skilllist <- h$skill
  SQL<-grepl(pattern = "SQL",x = skilllist)
  Algo<-grepl(pattern = "Algorithim",x = skilllist)
  Web<-grepl(pattern = "Web",x = skilllist)
  Java<-grepl(pattern = "Java",x = skilllist)
  Python<-grepl(pattern = "Python",x = skilllist)
  
  #treating the coloumns of company by eliminating adjacent words
  
  co1<-gsub(".* at \\s*|Education.*", "", h$co)
  co<-sub(pattern = "Limited",replacement = "",x=co1)
  co=sub(pattern = "limited",replacement = "",x=h$co)
  co=sub(pattern = "Ltd",replacement = "",x=h$co)
  co=sub(pattern = "LTD",replacement = "",x=h$co)
  co=sub(pattern = "ltd",replacement = "",x=h$co)
  co=sub(pattern = "Private",replacement = "",x=h$co)
  co=sub(pattern = "private",replacement = "",x=h$co)
  co=sub(pattern = "Pvt",replacement = "",x=h$co)
  co=sub(pattern = "PVT",replacement = "",x=h$co)
  co=sub(pattern = "pvt",replacement = "",x=h$co)
  co=sub(pattern = "Inc",replacement = "",x=h$co)
  co=sub(pattern = "inc",replacement = "",x=h$co)
  co=sub(pattern = "India",replacement = "",x=h$co)
  co=sub(pattern = "india",replacement = "",x=h$co)
  co<-gsub("[^a-z A-Z]","",h$co)
  
  h$co<-gsub(".* at \\s*|Education.*", "", h$co)
  
  xp<- substr(h$h,0,5)
  xp<-sub(pattern = "yr",replacement = ".",x=xp)
  xp<-sub(pattern = " ",replacement = "",x=xp)
  xp<-as.numeric(xp)
  
  w<-substr(h$h,7,10)
  w<-sub(pattern = "L",replacement = "",x=w)
  w<-sub(pattern = "m",replacement = "",x=w)
  w<-sub(pattern = "+",replacement = "",x=w)
  w<-sub(pattern = "L",replacement = "",x=w)
  w<-sub(pattern = "m",replacement = "",x=w)
  w<-sub(pattern = "+",replacement = "",x=w)
  w<-as.numeric(w)

  
  loc<-substr(h$h,12,100)
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
  
  skilllist <- h$skill
  SQL<-grepl(pattern = "SQL",x = skilllist)
  Algo<-grepl(pattern = "Algorithim",x = skilllist)
  Web<-grepl(pattern = "Web",x = skilllist)
  Java<-grepl(pattern = "Java",x = skilllist)
  Python<-grepl(pattern = "Python",x = skilllist)
  Regression<-grepl(pattern = "Regression",x = skilllist)
  R<-grepl(pattern = "R",x = skilllist)
  Text<-grepl(pattern = "Text",x = skilllist)
  Business<-grepl(pattern = "Business",x = skilllist)
   
  compdummy<-h$co
  
  Accenture<-grepl(pattern = "Accenture",x = compdummy)
  Wipro<-grepl(pattern = "Wipro",x = compdummy)
  IBM<-grepl(pattern = "IBM",x = compdummy)
  Infosys<-grepl(pattern = "Infosys",x = compdummy)
  Capgemini<-grepl(pattern = "Capgemini",x = compdummy)
  HCL<-grepl(pattern = "HCL",x = compdummy)
  Cognizant<-grepl(pattern = "Cognizant",x = compdummy)
  Tata<-grepl(pattern = "Tata",x = compdummy)
  Mu<-grepl(pattern = "Mu",x = compdummy)
  Hewlett<-grepl(pattern = "Hewlett",x = compdummy)
  
  companiesdummies<-cbind(w,xp,loc,co1,education,SQL,Algo,Web,Python,Regression,R,Text,Business,))
  
  df=data.frame(cbind(w,xp,loc,co1,education,SQL,Algo,Web,Python,Regression,R,Text,Business,companiesdummies))
 
  allvariables<-cbind(w,xp,loc,co1,education,SQL,Algo,Web,Python,Regression,R,Text,Business,Accenture,Wipro,IBM,Infosys,Capgemini,HCL,Cognizant,Tata
                        ,Mu,Hewlett)
  
   
  #L = df$Python == 
  #sqldry<-df[L,]
  
  chisq.test(x = w,y = Python)
  chisq.test(x = w,y = Regression) #p< 0.01
  chisq.test(x = w,y = education)
  chisq.test(x = w,y = Java) #p<0.1
  chisq.test(x = w,y = Business)#p<0.1
  chisq.test(x = w,y = R)#p<0.01
  
  probitmodel<- glm(w ~ education + SQL + Web + Python + Regression + R + Text + Business +Java 
                    + Accenture + Wipro + IBM + Infosys + Capgemini + HCL +Cognizant + Tata +
                      Mu + Hewlett,
                    family = "binomial"(link = "probit"),data =df)
  
  summary(probitmodel)
  millratio<-invMillsRatio(probitmodel,all = F)
  predictedmillratio<-millratio$IMR1
  strings<-round(predictedmillratio,0,6)
  plot(strings)
  
  stringsnownumb<-as.numeric(strings)
  stringsnownumb
  predictedmillratio<-round(predictedmillratio,9)
  plot(predictedmillratio)
  predictedmillratio<-as.integer(predictedmillratio)

    predictedmillratio<-cut(predictedmillratio,breaks = c(0,1,2,3,4,5,6,7,8,9,10))

  
  #plot(w[1:700],predictedmillratio[1:700])
  
  
  predictedmillratio<-as.integer(predictedmillratio)
  predictedmillratio*1
  plot<-plot(lm(w ~ xp + education + SQL + Algo + Web + Python + Regression + R + Text + Business,sqldry))
  
  
  
  View(df)
  
  
  
  
  
  
