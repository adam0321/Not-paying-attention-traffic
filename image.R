#年齡比率
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
  X $當事者事故發生時年齡<=20, "0-20",
  ifelse(
    X $當事者事故發生時年齡<=40, "21-40",
    ifelse(
      X $當事者事故發生時年齡<=60, "41-60",
      ifelse(
        X $當事者事故發生時年齡<=80, "61-80", "80以上"
      )
    )
  )
)
X$肇因 <- ifelse(X$肇因研判子類別名稱_主要 == "未注意車前狀態", "未注意車前狀態", "其他不為未注意車前狀態")
c <- group_by(X, 肇因, 年齡分組) %>%
  summarise(個數 = n())
c$比率   <-
  ifelse(
    c$年齡分組   == "21-40" & c$肇因   == "未注意車前狀態",
    2733 / (2733 + 10538),
    ifelse(
      c$年齡分組   == "21-40" &
        c$肇因   == "其他不為未注意車前狀態",
      10538 / (2733 + 10538),
      ifelse(
        c$年齡分組   == "41-60" & c$肇因   == "未注意車前狀態",
        1508 / (1508 + 8212),
        ifelse(
          c$年齡分組   == "41-60" &
            c$肇因   == "其他不為未注意車前狀態",
          8212 / (1508 + 8212),
          ifelse(
            c$年齡分組   == "61-80" & c$肇因   == "未注意車前狀態",
            511 / (511 + 3280),
            ifelse(
              c$年齡分組   == "61-80" & c$肇因   == "其他不為未注意車前狀態",
              3280 / (511 + 3280),
              ifelse(
                c$年齡分組 == "80以上" & c$肇因   == "未注意車前狀態",
                13 / (13 + 206),
                ifelse(
                c$年齡分組 == "80以上" & c$肇因   == "其他不為未注意車前狀態",
                206 / (13 + 206),
                ifelse(
                  c$年齡分組   == "0-20" & c$肇因   == "未注意車前狀態",
                  927 / (927 + 2882),
                  ifelse(c$年齡分組   == "0-20" &
                           c$肇因   == "其他不為未注意車前狀態", 2882 / (927 + 2882), 0)
                )
              )
            )
          )
        )
      )
    )
  )
)

ggplot(data = c) +
  aes(x = 年齡分組, fill = 肇因, weight = 比率) +
  geom_bar() +
  labs(x = "年齡分組",
       y = "比例") +
  theme_classic()


#0-20、21-40卡方
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
  X $當事者事故發生時年齡<=20, "0-20",
  ifelse(
    X $當事者事故發生時年齡<=40, "21-40",
    ifelse(
      X $當事者事故發生時年齡<=60, "41-60",
      ifelse(
        X $當事者事故發生時年齡<=80, "61-80", "80以上"
      )
    )
  )
)
X$肇因<-ifelse(
  X$肇因研判子類別名稱_主要=="未注意車前狀態","未注意車前狀態","其他不為未注意車前狀態")
library(MASS)
clean<- na.omit(subset(X, select = c(肇因, 年齡分組)))
head(clean)



esquisse::esquisser()



#比例圖
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
  X $當事者事故發生時年齡<=17, "0-17",
  ifelse(
    X $當事者事故發生時年齡<=24, "18-24",
    ifelse(
      X $當事者事故發生時年齡<=30, "24-30","30以上"
      
    )
  )
)
X$肇因<-ifelse(
  X$肇因研判子類別名稱_主要=="未注意車前狀態","未注意車前狀態", "其他不為未注意車前狀態")
library(MASS)
clean<- na.omit(subset(X, select = c(肇因, 年齡分組)))
head(clean)


clean2<-filter(clean,年齡分組%in% c("18-24","24-30","30以上"))
table2 <- table(肇因=clean2$肇因, 年齡分組=clean2$年齡分組)
chisq.test(table2,correct=FALSE)
summary(table2)
library(vcd)
clean1<-filter(clean,年齡分組%in% c("41-60","61-80"))
table1 <- table(肇因=clean1$肇因, 年齡分組=clean1$年齡分組)
summary(table1)
chisq.test(table1,correct=FALSE)
View(table1)

clean3<-filter(clean,年齡分組%in% c("0-20","41-60"))
table3 <- table(肇因=clean3$肇因, 年齡分組=clean3$年齡分組)
summary(table3)
chisq.test(table3,correct=FALSE)








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
  X $當事者事故發生時年齡<=20, "0-20",
  ifelse(
    X $當事者事故發生時年齡<=40, "21-40",
    ifelse(
      X $當事者事故發生時年齡<=60, "41-60",
      ifelse(
        X $當事者事故發生時年齡<=80, "61-80", "80以上"
      )
    )
  )
)

X$肇因<-ifelse(
  X$肇因研判子類別名稱_主要=="未注意車前狀態","未注意車前狀態", "其他不為未注意車前狀態")
clean<- na.omit(subset(X, select = c(肇因, 年齡分組)))
head(clean)
table1 <- table(肇因=clean$肇因, 年齡分組=clean$年齡分組)
c<-data.frame(table1)
c<-subset(c,肇因=="未注意車前狀態")
c$比率<-(c$Freq/sum(c$Freq)*100)

esquisse::esquisser()


ggplot(data = c) +
  aes(x = 年齡分組, fill = 年齡分組, weight = 比率) +
  geom_bar() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(x = "年齡",
       y = "比例") +
  theme_classic() +
  coord_flip()
?guides()








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
d<-data.frame(d)
ggplot(data=d,aes(x = reorder(當事者事故發生時年齡,當事者事故發生時年齡,function(x)-length(x)),y=個數,fill=個數)) +
  geom_bar(stat="identity")+theme_classic()+guides(fill=FALSE)
d
esquisse::esquisser()








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
  X $當事者事故發生時年齡<=20, "0-20",
  ifelse(
    X $當事者事故發生時年齡<=40, "21-40",
    ifelse(
      X $當事者事故發生時年齡<=60, "41-60",
      ifelse(
        X $當事者事故發生時年齡<=80, "61-80", "80以上"
      )
    )
  )
)
a<- na.omit(subset(X, select = c(年齡分組,肇因研判子類別名稱_主要)))
a<-group_by(a,年齡分組,肇因研判子類別名稱_主要)%>%
  summarise(個數=n())
ggplot(data = a) +
  aes(x = 肇因研判子類別名稱_主要, fill = 肇因研判子類別名稱_主要, weight = 個數) +
  geom_bar() +
  scale_fill_viridis_d(option  = "magma") +
  theme_classic() +
  coord_flip()+guides(fill=FALSE)+
  theme(text = element_text(family = "BL"))
windowsFonts(BL = windowsFont("微軟正黑體"))
