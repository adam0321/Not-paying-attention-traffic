library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)

register_google(key = "AIzaSyC7JRppSZtZuzpy9SdsVbpK5Nhf_oh2xl0")
a<- read_csv("C:/Users/USER/Desktop/traffic108_c.csv")
a<-filter(a,肇因研判子類別名稱_主要=="未注意車前狀態")

a$年齡分組<-ifelse(
  a$當事者事故發生時年齡<=20, "小於等於20",
  ifelse(
    a$當事者事故發生時年齡<=40, "20以上，小於等於40",
    ifelse(
      a$當事者事故發生時年齡<=60, "40以上，小於等於60",
      ifelse(
        a$當事者事故發生時年齡<=80, "60以上，小於等於80", "80以上"
      )
    )
  )
)
a<-group_by(a,年齡分組,當事者事故發生時年齡)%>%
  summarise(個數=n())

ggplot(data = a) +
  aes(x = 當事者事故發生時年齡, y = 個數, color = 年齡分組) +
  geom_point() +
  theme_minimal()


TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW"
)



library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)

register_google(key = "AIzaSyC7JRppSZtZuzpy9SdsVbpK5Nhf_oh2xl0")
a<- read_csv("C:/Users/USER/Desktop/traffic108_c.csv")





a<-filter(X107,肇因研判子類別名稱_主要=="未注意車前狀態")
a<-select(a,肇因研判子類別名稱_主要,當事者事故發生時年齡,GPS經度,GPS緯度)
a$年齡分組<-ifelse(
  a$當事者事故發生時年齡<=20, "小於等於20",
  ifelse(
    a$當事者事故發生時年齡<=40, "20以上，小於等於40",
    ifelse(
      a$當事者事故發生時年齡<=60, "40以上，小於等於60",
      ifelse(
        a$當事者事故發生時年齡<=80, "60以上，小於等於80", "80以上"
      )
    )
  )
)
atable <- table(a$肇因研判子類別名稱_主要, a$年齡分組)
atable




TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW"
)

TMapO <-ggmap(TMap) + geom_point(data=a,
                   aes(x=GPS經度,y=GPS緯度, color = 年齡分組))
                   

TMapO



View(b)
a<-group_by(a,肇因研判子類別名稱_主要,當事者事故發生時年齡)%>%
summarise(個數=n())
aaov<-aov(a$當事者事故發生時年齡~as.character(a$肇因研判子類別名稱_主要))
summary(aaov)
leveneTest(a$當事者事故發生時年齡,as.character(a$肇因研判子類別名稱_主要))
table2<-table(a$當事者事故發生時年齡,a$肇因研判子類別名稱_主要)
View(table2)
c<-prop.table(table2,margin=1) 
View
fisher.test(table2)







library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
a<- read_csv("C:/Users/USER/Desktop/traffic108_c.csv")
select1<-filter(a,肇因研判子類別名稱_主要=="未注意車前狀態")
select1<-data.frame(年齡=a$當事者事故發生時年齡)
a<-group_by(select1,年齡)%>%
summarise(個數=n())
cor(a$年齡,a$個數)
cor.test(a$年齡,a$個數)




library(readr)
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
X107 <- read_csv("http://data.tycg.gov.tw/api/v1/dump/datastore/1a1227d9-5199-4ded-99bc-733997224fb2?format=csv")
register_google(key = "AIzaSyC7JRppSZtZuzpy9SdsVbpK5Nhf_oh2xl0")
a<-group_by(x107,年齡分組,X135)%>%
  summarise(個數=n())

ggplot(data = a) +
  aes(x = 當事者事故發生時年齡, y = 個數, color = 年齡分組) +
  geom_point() +
  theme_minimal()


TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW"
)

TMapO <-ggmap(TMap) + geom_point(data=a,
                                 aes(x=GPS經度,y=GPS緯度, color = 年齡分組))


TMapO