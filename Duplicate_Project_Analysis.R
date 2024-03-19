library(readr)
library(stargazer)
library(jtools)
library(coefplot)
library(ggpubr)
library(labelled)
data1<-anes_timeseries_2016
data3<-data.frame(data1$V161156,data1$V162034a,data1$V161158x,data1$V161140,data1$V161140a,data1$V161235,data1$V161235a,data1$V162175,
                  data1$V162176,data1$V162152a,data1$V162152b,data1$V161178,data1$V161189,
                  data1$V162148,data1$V161232,data1$V161231,data1$V162158,data1$V161192,data1$V162157,data1$V162211,
                  data1$V162212,data1$V162213,data1$V162214,data1$V161508,data1$V161509,data1$V161510,data1$V161507,
                  data1$V162170,data1$V162263,data1$V162261,data1$V162262,data1$V162260,data1$V162193)
bruh<-data.frame(data1$V162152a,data1$V162152b)



#,data1$V162176a

data1["V162152a"][data1["V162152a"]<0]<-0
data1["V162152b"][data1["V162152b"]<0]<-0
data1["V162152b"][data1["V162152b"]==99]<-3
#data4["data1.V162152b"][data4["data1.V162152b"]<0]<-NA
data1$V162152c<-paste(data1$V162152a,data3$V162152b)

data3<-data.frame(data1$V162152c,data1$V161156,data1$V162034a,data1$V161158x,data1$V161140,data1$V161140a,data1$V161235,data1$V161235a,data1$V162175,
                  data1$V162176,data1$V161178,data1$V161189,
                  data1$V162148,data1$V161232,data1$V161231,data1$V162158,data1$V161192,data1$V162157,data1$V162211,
                  data1$V162212,data1$V162213,data1$V162214,data1$V161508,data1$V161509,data1$V161510,data1$V161507,
                  data1$V162170,data1$V162263,data1$V162261,data1$V162262,data1$V162260,data1$V162193)


data4<-data3
data4<-subset(data4, data1.V162034a>0)
data4<-subset(data4, data1.V161158x>0)
mean(data1$V162263)

data4["data1.V162152c"][data4["data1.V162152c"]==0]<-NA
data4["data1.V162034a"][data4["data1.V162034a"]!=2]<-0
data4["data1.V162034a"][data4["data1.V162034a"]==2]<-1

data4["data1.V161140"][data4["data1.V161140"]<0]<-NA
data4["data1.V161235"][data4["data1.V161235"]<0]<-NA
data4["data1.V161235"][data4["data1.V161235"]==2]<-4
data4["data1.V161235"][data4["data1.V161235"]==3]<-2
data4["data1.V161235"][data4["data1.V161235"]==4]<-3
data4["data1.V162175"][data4["data1.V162175"]<0]<-NA
data4["data1.V162175"][data4["data1.V162175"]==2]<-4
data4["data1.V162175"][data4["data1.V162175"]==3]<-2
data4["data1.V162175"][data4["data1.V162175"]==4]<-3
data4["data1.V162176"][data4["data1.V162176"]<0]<-NA
data4["data1.V162176"][data4["data1.V162176"]==2]<-4
data4["data1.V162176"][data4["data1.V162176"]==3]<-2
data4["data1.V162176"][data4["data1.V162176"]==4]<-3
#data4["data1.V162176a"][data4["data1.V162176a"]<0]<-NA
#data4["data1.V162176a"][data4["data1.V162176a"]==1]<-4
#data4["data1.V162176a"][data4["data1.V162176a"]==3]<-1
#data4["data1.V162176a"][data4["data1.V162176a"]==4]<-3








data4["data1.V161178"][data4["data1.V161178"]<0]<-NA
data4["data1.V161178"][data4["data1.V161178"]==99]<-0
data4["data1.V161189"][data4["data1.V161189"]==99]<-0
data4["data1.V161189"][data4["data1.V161189"]<0]<-NA
data4["data1.V162148"][data4["data1.V162148"]<0]<-NA
data4["data1.V162148"][data4["data1.V162148"]==2]<-4
data4["data1.V162148"][data4["data1.V162148"]==3]<-2
data4["data1.V162148"][data4["data1.V162148"]==4]<-3
data4["data1.V161232"][data4["data1.V161232"]<0]<-NA
data4["data1.V161232"][data4["data1.V161232"]==5]<-0
data4["data1.V161231"][data4["data1.V161231"]<0]<-NA
data4["data1.V162158"][data4["data1.V162158"]<0]<-NA
data4["data1.V161192"][data4["data1.V161192"]<0]<-NA
data4["data1.V162157"][data4["data1.V162157"]<0]<-NA
data4["data1.V162211"][data4["data1.V162211"]<0]<-NA
data4["data1.V162212"][data4["data1.V162212"]<0]<-NA
data4["data1.V162213"][data4["data1.V162213"]<0]<-NA
data4["data1.V162214"][data4["data1.V162214"]<0]<-NA
data4["data1.V161507"][data4["data1.V161507"]<0]<-NA
data4["data1.V161508"][data4["data1.V161508"]<0]<-NA
data4["data1.V161509"][data4["data1.V161509"]<0]<-NA
data4["data1.V161510"][data4["data1.V161510"]<0]<-NA
data4["data1.V162170"][data4["data1.V162170"]<0]<-NA
data4["data1.V162263"][data4["data1.V162263"]<0]<-NA
data4["data1.V162261"][data4["data1.V162261"]<0]<-NA
data4["data1.V162260"][data4["data1.V162260"]<0]<-NA
data4["data1.V162262"][data4["data1.V162262"]<0]<-NA



trade1<-data.frame(data4$data1.V162175,data4$data1.V162176,data4$data1.V162152c)



voting<-data4$data1.V162034a
party<-data4$data1.V161158x
econ<-data4$data1.V161140+data4$data1.V161235
data4$data1.V162152c<-as.integer(data4$data1.V162152c)
trade<-data4$data1.V162175+data4$data1.V162176+data4$data1.V162152c
social<-data4$data1.V161178+data4$data1.V161189+data4$data1.V162148
moral<-data4$data1.V161232+data4$data1.V161231
immigration<-data4$data1.V162158+data4$data1.V161192+data4$data1.V162157
black<-data4$data1.V162211+data4$data1.V162212+data4$data1.V162213+data4$data1.V162214
woman<-data4$data1.V161507+data4$data1.V161508+data4$data1.V161509+data4$data1.V161510
strong<-data4$data1.V162170+data4$data1.V162263
trust<-data4$data1.V162260+data4$data1.V162261+data4$data1.V162262

data5<-data.frame(voting,party,econ,trade,
                  social,moral,immigration,black,woman,strong,trust)
m1<-glm(voting~party+econ+social+trade+moral+immigration+black+woman+strong+trust,data=data5,family=binomial)
#m1<-glm(voting~econ,data=data5,family=binomial)
summary(m1)
effect_plot(model=m1, pred=partY,x.label = "Economy",y.label="Party Identification")

table(party)
table(voting)
table(econ)
table(trade)
table(social)
table(moral)
table(immigration)
table(black)
table(woman)
table(strong)
table(trust)





repub <- data.frame(data1$V161165, data1$V161164, data1$V161167,  data1$V161166)
democ <- data.frame(data1$V161160, data1$V161159, data1$V161162, data1$V161161)
repub<-subset(repub, data1.V161165>0)
repub<-subset(repub, data1.V161164>0)
repub<-subset(repub, data1.V161167>0)
repub<-subset(repub, data1.V161166>0)
democ<-subset(democ, data1.V161160>0)
democ<-subset(democ, data1.V161159>0)
democ<-subset(democ, data1.V161162>0)
democ<-subset(democ, data1.V161161>0)
mean1 <- repub$data1.V161165 + repub$data1.V161164 + repub$data1.V161167 + repub$data1.V161166
mean2 <- democ$data1.V161160 + democ$data1.V161159 + democ$data1.V161162 + democ$data1.V161161
mean1 <- mean1/4
mean2 <- mean2/4
mean11 <- mean(mean1)
mean22 <- mean(mean2)
mean12 <- (mean11 - mean22)
barplot(table(mean1),main="Republican mean Distribution",xlab="Mean",ylab="Frequency")
barplot(table(mean2),main="Democrat mean Distribution",xlab="Mean",ylab="Frequency")
