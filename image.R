#�~�֤�v
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)

X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "0-20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "21-40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "41-60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "61-80", "80�H�W"
      )
    )
  )
)
X$�F�] <- ifelse(X$�F�]��P�l���O�W��_�D�n == "���`�N���e���A", "���`�N���e���A", "��L�������`�N���e���A")
c <- group_by(X, �F�], �~�֤���) %>%
  summarise(�Ӽ� = n())
c$��v   <-
  ifelse(
    c$�~�֤���   == "21-40" & c$�F�]   == "���`�N���e���A",
    2733 / (2733 + 10538),
    ifelse(
      c$�~�֤���   == "21-40" &
        c$�F�]   == "��L�������`�N���e���A",
      10538 / (2733 + 10538),
      ifelse(
        c$�~�֤���   == "41-60" & c$�F�]   == "���`�N���e���A",
        1508 / (1508 + 8212),
        ifelse(
          c$�~�֤���   == "41-60" &
            c$�F�]   == "��L�������`�N���e���A",
          8212 / (1508 + 8212),
          ifelse(
            c$�~�֤���   == "61-80" & c$�F�]   == "���`�N���e���A",
            511 / (511 + 3280),
            ifelse(
              c$�~�֤���   == "61-80" & c$�F�]   == "��L�������`�N���e���A",
              3280 / (511 + 3280),
              ifelse(
                c$�~�֤��� == "80�H�W" & c$�F�]   == "���`�N���e���A",
                13 / (13 + 206),
                ifelse(
                c$�~�֤��� == "80�H�W" & c$�F�]   == "��L�������`�N���e���A",
                206 / (13 + 206),
                ifelse(
                  c$�~�֤���   == "0-20" & c$�F�]   == "���`�N���e���A",
                  927 / (927 + 2882),
                  ifelse(c$�~�֤���   == "0-20" &
                           c$�F�]   == "��L�������`�N���e���A", 2882 / (927 + 2882), 0)
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
  aes(x = �~�֤���, fill = �F�], weight = ��v) +
  geom_bar() +
  labs(x = "�~�֤���",
       y = "���") +
  theme_classic()


#0-20�B21-40�d��
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "0-20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "21-40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "41-60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "61-80", "80�H�W"
      )
    )
  )
)
X$�F�]<-ifelse(
  X$�F�]��P�l���O�W��_�D�n=="���`�N���e���A","���`�N���e���A","��L�������`�N���e���A")
library(MASS)
clean<- na.omit(subset(X, select = c(�F�], �~�֤���)))
head(clean)



esquisse::esquisser()



#��ҹ�
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)

X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=17, "0-17",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=24, "18-24",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=30, "24-30","30�H�W"
      
    )
  )
)
X$�F�]<-ifelse(
  X$�F�]��P�l���O�W��_�D�n=="���`�N���e���A","���`�N���e���A", "��L�������`�N���e���A")
library(MASS)
clean<- na.omit(subset(X, select = c(�F�], �~�֤���)))
head(clean)


clean2<-filter(clean,�~�֤���%in% c("18-24","24-30","30�H�W"))
table2 <- table(�F�]=clean2$�F�], �~�֤���=clean2$�~�֤���)
chisq.test(table2,correct=FALSE)
summary(table2)
library(vcd)
clean1<-filter(clean,�~�֤���%in% c("41-60","61-80"))
table1 <- table(�F�]=clean1$�F�], �~�֤���=clean1$�~�֤���)
summary(table1)
chisq.test(table1,correct=FALSE)
View(table1)

clean3<-filter(clean,�~�֤���%in% c("0-20","41-60"))
table3 <- table(�F�]=clean3$�F�], �~�֤���=clean3$�~�֤���)
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

X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "0-20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "21-40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "41-60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "61-80", "80�H�W"
      )
    )
  )
)

X$�F�]<-ifelse(
  X$�F�]��P�l���O�W��_�D�n=="���`�N���e���A","���`�N���e���A", "��L�������`�N���e���A")
clean<- na.omit(subset(X, select = c(�F�], �~�֤���)))
head(clean)
table1 <- table(�F�]=clean$�F�], �~�֤���=clean$�~�֤���)
c<-data.frame(table1)
c<-subset(c,�F�]=="���`�N���e���A")
c$��v<-(c$Freq/sum(c$Freq)*100)

esquisse::esquisser()


ggplot(data = c) +
  aes(x = �~�֤���, fill = �~�֤���, weight = ��v) +
  geom_bar() +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(x = "�~��",
       y = "���") +
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
a<-filter(X,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
a<- na.omit(subset(a, select = c(���ƪ̨ƬG�o�ͮɦ~��)))
a<-group_by(a,���ƪ̨ƬG�o�ͮɦ~��)%>%
  summarise(�Ӽ�=n())
d<-head(a[order(a$�Ӽ�,decreasing = T),],n=5)
d<-data.frame(d)
ggplot(data=d,aes(x = reorder(���ƪ̨ƬG�o�ͮɦ~��,���ƪ̨ƬG�o�ͮɦ~��,function(x)-length(x)),y=�Ӽ�,fill=�Ӽ�)) +
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
X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "0-20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "21-40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "41-60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "61-80", "80�H�W"
      )
    )
  )
)
a<- na.omit(subset(X, select = c(�~�֤���,�F�]��P�l���O�W��_�D�n)))
a<-group_by(a,�~�֤���,�F�]��P�l���O�W��_�D�n)%>%
  summarise(�Ӽ�=n())
ggplot(data = a) +
  aes(x = �F�]��P�l���O�W��_�D�n, fill = �F�]��P�l���O�W��_�D�n, weight = �Ӽ�) +
  geom_bar() +
  scale_fill_viridis_d(option  = "magma") +
  theme_classic() +
  coord_flip()+guides(fill=FALSE)+
  theme(text = element_text(family = "BL"))
windowsFonts(BL = windowsFont("�L�n������"))