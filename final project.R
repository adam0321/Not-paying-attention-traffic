#找出甚麼原因發生車禍最多
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
a<-data.frame(肇因=X$肇因研判子類別名稱_主要,受傷人數=X$受傷人數)
ggplot(a)+geom_bar(data=a,
                   aes(x=肇因,y=受傷人數),
                   stat = "identity")+theme_classic() + coord_flip()



#依據年齡做分層，畫出google地圖，熱度圖
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
                 trim_ws = FALSE)
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
register_google(key = "AIzaSyDVhy1jnJEDSy8cZDcmJv1kKq7t41-PpVI")
a<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
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

TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW", maptype = "hybrid"
)
TMapO <-ggmap(TMap) + geom_point(data=a,
                                 aes(x=GPS經度,y=GPS緯度, color = 年齡分組))
TMapO

TMapO <-ggmap(TMap) + stat_density2d(data = a, aes(x = GPS經度, y=GPS緯度,fill = ..level.., alpha = ..level..)
                 ,size = 0.1, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red")+scale_alpha(range = c(0, 0.3))
TMapO




#依據年齡和發生"未注意車前狀態"做相關係數分析
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
select1<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
select1<-data.frame(年齡=a$當事者事故發生時年齡)
a<-group_by(select1,年齡)%>%
  summarise(個數=n())
cor(a$年齡,a$個數)
cor.test(a$年齡,a$個數)




library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
a<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
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
ggplot(a, aes(x =肇因研判子類別名稱_主要 , fill = 年齡分組)) + geom_bar(position = "dodge") +
  theme_classic()




#?
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)

X $年齡分組<-ifelse(
  X $當事者事故發生時年齡<=20, "小於等於20",
  ifelse(
    X $當事者事故發生時年齡<=40, "20以上，小於等於40",
    ifelse(
      X $當事者事故發生時年齡<=60, "40以上，小於等於60",
      ifelse(
        X $當事者事故發生時年齡<=80, "60以上，小於等於80", "80以上"
      )
    )
  )
)
ggplot(X, aes(x =肇因研判子類別名稱_主要 , fill = 年齡分組)) + geom_bar(position = "dodge") + coord_flip()+
  theme_classic()



#H0:"未注意車前狀態"平均年齡等於所有肇因發生車禍的平均年齡
#H1:"未注意車前狀態"平均年齡不等於所有肇因發生車禍的平均年齡
a<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
t.test(a$當事者事故發生時年齡,mu=mean(X$當事者事故發生時年齡))
#結論:年齡會影響肇事原因







library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
X<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
X $年齡分組<-ifelse(
  X $當事者事故發生時年齡<=20, "小於等於20",
  ifelse(
    X $當事者事故發生時年齡<=40, "20以上，小於等於40",
    ifelse(
      X $當事者事故發生時年齡<=60, "40以上，小於等於60",
      ifelse(
        X $當事者事故發生時年齡<=80,"60以上，小於等於80", "80以上"
      )
    )
  )
)

library(MASS)
clean<- na.omit(subset(X, select = c(肇因研判子類別名稱_主要, 年齡分組)))
head(clean)
table1 <- table(clean$肇因研判子類別名稱_主要, clean$年齡分組)
table1
chisq.test(table1,simulate.p.value=TRUE)
prop.table(table1,margin=1)
fisher.test(table1,conf.level = 0.95,simulate.p.value=TRUE)
?fisher.test()
ggplot(X, aes(當事者事故發生時年齡)) + geom_density(colour="steelblue")+theme_classic()

library(zoo)
brks.cdf <- pgamma(table1, shape = 10, scale = 3)
null.probs <- rollapply(brks.cdf, 2, function(x) x[2]-x[1])
chisq.test(table1, p = null.probs, rescale.p = TRUE, simulate.p.value = TRUE)
fisher.test(table1,conf.level = 0.95,simulate.p.value=TRUE)
ggplot(X, aes(x = 發生市區鄉鎮名稱, fill = 年齡分組)) + geom_bar(position = "dodge")+theme_classic()





library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
a<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
a<- na.omit(subset(a, select = c(當事者事故發生時年齡)))
a<-group_by(a,當事者事故發生時年齡)%>%
  summarise(個數=n())
d<-head(a[order(a$個數,decreasing = T),],n=5)
ggplot(d)+geom_bar(data=d,
                   aes(x=當事者事故發生時年齡,y=個數,fill = 個數),
                   stat = "identity")+theme_classic()
d
esquisse::esquisser()










#年齡受傷人數之圖表
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
a<-filter(X,肇因研判子類別名稱_主要=="未注意車前狀態")
ggplot(data = a) +
  aes(x = 當事者事故發生時年齡, fill = 當事者屬性別名稱) +
  geom_histogram(bins = 30) +
  theme_classic() +
  theme(legend.position = 'left')

#年齡因"未注意車前狀態"受傷人數之圖表
ggplot(data = a) +
  aes(x = 當事者事故發生時年齡, fill = factor(年齡分組)) +
  geom_histogram(bins = 30) +
  theme_classic()







ggplot(data = a) +
  aes(x = 當事者事故發生時年齡) +
  geom_histogram(bins = 30, fill = "#377eb8") +
  theme_base()




lm.model <- lm(個數~年齡,a)

plot(x=a$年齡,
     y=a$個數,
     xlab="年齡",
     ylab="個數",
     pch=16
)
abline(lm.model,                          
        lwd=2)











#費雪校正
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)

X $年齡分組<-ifelse(
  X $當事者事故發生時年齡<=20, "小於等於20",
  ifelse(
    X $當事者事故發生時年齡<=40, "20以上，小於等於40",
    ifelse(
      X $當事者事故發生時年齡<=60, "40以上，小於等於60",
      ifelse(
        X $當事者事故發生時年齡<=80, "60以上，小於等於80", "80以上"
      )
    )
  )
)

X$肇因<-ifelse(
  X$肇因研判子類別名稱_主要=="未注意車前狀態","未注意車前狀態","其他不為未注意車前狀態")

library(MASS)
clean<- na.omit(subset(X, select = c(肇因, 年齡分組)))
head(clean)
table1 <- table(clean$肇因, clean$年齡分組)
table1
chisq.test(table1)
prop.table(table1,margin=1)
View(table1)
fisher.test(table1,conf.level = 0.95,simulate.p.value=TRUE)








library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)

X $年齡分組<-ifelse(
  X $當事者事故發生時年齡<=20, "小於等於20",
  ifelse(
    X $當事者事故發生時年齡<=40, "20以上，小於等於40",
    ifelse(
      X $當事者事故發生時年齡<=60, "40以上，小於等於60",
      ifelse(
        X $當事者事故發生時年齡<=80, "60以上，小於等於80", "80以上"
      )
    )
  )
)
c<-data.frame(table1)
View(c)
c<-subset(c,肇因=="未注意車前狀態")
c<-group_by(c,年齡分組)%>%
  summarise(個數=n())
c$比率<-(c$Freq/sum(c$Freq)*100)
ggplot(data = c) +
  aes(x = 年齡分組, fill = 年齡分組, weight = 比率) +
  geom_bar(position = "dodge") +
  theme_classic()

