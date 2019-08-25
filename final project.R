#��X�ƻ��]�o�ͨ��׳̦h
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
a<-data.frame(�F�]=X$�F�]��P�l���O�W��_�D�n,���ˤH��=X$���ˤH��)
ggplot(a)+geom_bar(data=a,
                   aes(x=�F�],y=���ˤH��),
                   stat = "identity")+theme_classic() + coord_flip()



#�̾ڦ~�ְ����h�A�e�Xgoogle�a�ϡA���׹�
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
                 trim_ws = FALSE)
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
register_google(key = "AIzaSyDVhy1jnJEDSy8cZDcmJv1kKq7t41-PpVI")
a<-filter(X,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
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

TMap <- get_googlemap(
  center  = c(lon = 121.20, lat = 25.00),
  zoom = 11,
  language = "zh-TW", maptype = "hybrid"
)
TMapO <-ggmap(TMap) + geom_point(data=a,
                                 aes(x=GPS�g��,y=GPS�n��, color = �~�֤���))
TMapO

TMapO <-ggmap(TMap) + stat_density2d(data = a, aes(x = GPS�g��, y=GPS�n��,fill = ..level.., alpha = ..level..)
                 ,size = 0.1, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red")+scale_alpha(range = c(0, 0.3))
TMapO




#�̾ڦ~�֩M�o��"���`�N���e���A"�������Y�Ƥ��R
library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
select1<-filter(X,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
select1<-data.frame(�~��=a$���ƪ̨ƬG�o�ͮɦ~��)
a<-group_by(select1,�~��)%>%
  summarise(�Ӽ�=n())
cor(a$�~��,a$�Ӽ�)
cor.test(a$�~��,a$�Ӽ�)




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
ggplot(a, aes(x =�F�]��P�l���O�W��_�D�n , fill = �~�֤���)) + geom_bar(position = "dodge") +
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

X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "�p�󵥩�20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "20�H�W�A�p�󵥩�40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "40�H�W�A�p�󵥩�60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "60�H�W�A�p�󵥩�80", "80�H�W"
      )
    )
  )
)
ggplot(X, aes(x =�F�]��P�l���O�W��_�D�n , fill = �~�֤���)) + geom_bar(position = "dodge") + coord_flip()+
  theme_classic()



#H0:"���`�N���e���A"�����~�ֵ���Ҧ��F�]�o�ͨ��ת������~��
#H1:"���`�N���e���A"�����~�֤�����Ҧ��F�]�o�ͨ��ת������~��
a<-filter(X,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
t.test(a$���ƪ̨ƬG�o�ͮɦ~��,mu=mean(X$���ƪ̨ƬG�o�ͮɦ~��))
#����:�~�ַ|�v�T�F�ƭ�]







library(readr)
library(dplyr)
library(ggmap)
library(ggplot2)
library(reshape2)
library(car)
library(readr)
X <- read_csv("C:/Users/USER/Desktop/107.csv", 
              trim_ws = FALSE)
X<-filter(X,�F�]��P�l���O�W��_�D�n=="���`�N���e���A")
X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "�p�󵥩�20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "20�H�W�A�p�󵥩�40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "40�H�W�A�p�󵥩�60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80,"60�H�W�A�p�󵥩�80", "80�H�W"
      )
    )
  )
)

library(MASS)
clean<- na.omit(subset(X, select = c(�F�]��P�l���O�W��_�D�n, �~�֤���)))
head(clean)
table1 <- table(clean$�F�]��P�l���O�W��_�D�n, clean$�~�֤���)
table1
chisq.test(table1,simulate.p.value=TRUE)
prop.table(table1,margin=1)
fisher.test(table1,conf.level = 0.95,simulate.p.value=TRUE)
?fisher.test()
ggplot(X, aes(���ƪ̨ƬG�o�ͮɦ~��)) + geom_density(colour="steelblue")+theme_classic()

library(zoo)
brks.cdf <- pgamma(table1, shape = 10, scale = 3)
null.probs <- rollapply(brks.cdf, 2, function(x) x[2]-x[1])
chisq.test(table1, p = null.probs, rescale.p = TRUE, simulate.p.value = TRUE)
fisher.test(table1,conf.level = 0.95,simulate.p.value=TRUE)
ggplot(X, aes(x = �o�ͥ��϶m���W��, fill = �~�֤���)) + geom_bar(position = "dodge")+theme_classic()





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
ggplot(d)+geom_bar(data=d,
                   aes(x=���ƪ̨ƬG�o�ͮɦ~��,y=�Ӽ�,fill = �Ӽ�),
                   stat = "identity")+theme_classic()
d
esquisse::esquisser()










#�~�֨��ˤH�Ƥ��Ϫ�
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
ggplot(data = a) +
  aes(x = ���ƪ̨ƬG�o�ͮɦ~��, fill = ���ƪ��ݩʧO�W��) +
  geom_histogram(bins = 30) +
  theme_classic() +
  theme(legend.position = 'left')

#�~�֦]"���`�N���e���A"���ˤH�Ƥ��Ϫ�
ggplot(data = a) +
  aes(x = ���ƪ̨ƬG�o�ͮɦ~��, fill = factor(�~�֤���)) +
  geom_histogram(bins = 30) +
  theme_classic()







ggplot(data = a) +
  aes(x = ���ƪ̨ƬG�o�ͮɦ~��) +
  geom_histogram(bins = 30, fill = "#377eb8") +
  theme_base()




lm.model <- lm(�Ӽ�~�~��,a)

plot(x=a$�~��,
     y=a$�Ӽ�,
     xlab="�~��",
     ylab="�Ӽ�",
     pch=16
)
abline(lm.model,                          
        lwd=2)











#�O���ե�
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
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "�p�󵥩�20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "20�H�W�A�p�󵥩�40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "40�H�W�A�p�󵥩�60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "60�H�W�A�p�󵥩�80", "80�H�W"
      )
    )
  )
)

X$�F�]<-ifelse(
  X$�F�]��P�l���O�W��_�D�n=="���`�N���e���A","���`�N���e���A","��L�������`�N���e���A")

library(MASS)
clean<- na.omit(subset(X, select = c(�F�], �~�֤���)))
head(clean)
table1 <- table(clean$�F�], clean$�~�֤���)
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

X $�~�֤���<-ifelse(
  X $���ƪ̨ƬG�o�ͮɦ~��<=20, "�p�󵥩�20",
  ifelse(
    X $���ƪ̨ƬG�o�ͮɦ~��<=40, "20�H�W�A�p�󵥩�40",
    ifelse(
      X $���ƪ̨ƬG�o�ͮɦ~��<=60, "40�H�W�A�p�󵥩�60",
      ifelse(
        X $���ƪ̨ƬG�o�ͮɦ~��<=80, "60�H�W�A�p�󵥩�80", "80�H�W"
      )
    )
  )
)
c<-data.frame(table1)
View(c)
c<-subset(c,�F�]=="���`�N���e���A")
c<-group_by(c,�~�֤���)%>%
  summarise(�Ӽ�=n())
c$��v<-(c$Freq/sum(c$Freq)*100)
ggplot(data = c) +
  aes(x = �~�֤���, fill = �~�֤���, weight = ��v) +
  geom_bar(position = "dodge") +
  theme_classic()
