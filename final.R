library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)

register_google(key = "AIzaSyC7JRppSZtZuzpy9SdsVbpK5Nhf_oh2xl0")
a<- read_csv("C:/Users/USER/Desktop/traffic108_c.csv")
a<-filter(a,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")

a$�~�֤���<-ifelse(
  a$���ƪ̨ƬG�o�ͮɦ~��<=20, "�p�󵥩�20",
  ifelse(
    a$���ƪ̨ƬG�o�ͮɦ~��<=40, "20�H�W�A�p�󵥩�40",
    ifelse(
      a$���ƪ̨ƬG�o�ͮɦ~��<=60, "40�H�W�A�p�󵥩�60",
      ifelse(
        a$���ƪ̨ƬG�o�ͮɦ~��<=80, "60�H�W�A�p�󵥩�80", "80�H�W"
      )
    )
  )
)
a<-group_by(a,�~�֤���,���ƪ̨ƬG�o�ͮɦ~��)%>%
  summarise(�Ӽ�=n())

ggplot(data = a) +
  aes(x = ���ƪ̨ƬG�o�ͮɦ~��, y = �Ӽ�, color = �~�֤���) +
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





a<-filter(X107,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
a<-select(a,�F�]��P�l���O�W��_�D�n,���ƪ̨ƬG�o�ͮɦ~��,GPS�g��,GPS�n��)
a$�~�֤���<-ifelse(
  a$���ƪ̨ƬG�o�ͮɦ~��<=20, "�p�󵥩�20",
  ifelse(
    a$���ƪ̨ƬG�o�ͮɦ~��<=40, "20�H�W�A�p�󵥩�40",
    ifelse(
      a$���ƪ̨ƬG�o�ͮɦ~��<=60, "40�H�W�A�p�󵥩�60",
      ifelse(
        a$���ƪ̨ƬG�o�ͮɦ~��<=80, "60�H�W�A�p�󵥩�80", "80�H�W"
      )
    )
  )
)
atable <- table(a$�F�]��P�l���O�W��_�D�n, a$�~�֤���)
atable




TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW"
)

TMapO <-ggmap(TMap) + geom_point(data=a,
                   aes(x=GPS�g��,y=GPS�n��, color = �~�֤���))
                   

TMapO



View(b)
a<-group_by(a,�F�]��P�l���O�W��_�D�n,���ƪ̨ƬG�o�ͮɦ~��)%>%
summarise(�Ӽ�=n())
aaov<-aov(a$���ƪ̨ƬG�o�ͮɦ~��~as.character(a$�F�]��P�l���O�W��_�D�n))
summary(aaov)
leveneTest(a$���ƪ̨ƬG�o�ͮɦ~��,as.character(a$�F�]��P�l���O�W��_�D�n))
table2<-table(a$���ƪ̨ƬG�o�ͮɦ~��,a$�F�]��P�l���O�W��_�D�n)
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
select1<-filter(a,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
select1<-data.frame(�~��=a$���ƪ̨ƬG�o�ͮɦ~��)
a<-group_by(select1,�~��)%>%
summarise(�Ӽ�=n())
cor(a$�~��,a$�Ӽ�)
cor.test(a$�~��,a$�Ӽ�)




library(readr)
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
X107 <- read_csv("http://data.tycg.gov.tw/api/v1/dump/datastore/1a1227d9-5199-4ded-99bc-733997224fb2?format=csv")
register_google(key = "AIzaSyC7JRppSZtZuzpy9SdsVbpK5Nhf_oh2xl0")
a<-group_by(x107,�~�֤���,X135)%>%
  summarise(�Ӽ�=n())

ggplot(data = a) +
  aes(x = ���ƪ̨ƬG�o�ͮɦ~��, y = �Ӽ�, color = �~�֤���) +
  geom_point() +
  theme_minimal()


TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW"
)

TMapO <-ggmap(TMap) + geom_point(data=a,
                                 aes(x=GPS�g��,y=GPS�n��, color = �~�֤���))


TMapO