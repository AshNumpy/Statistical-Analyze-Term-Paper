library(dplyr)  #verileri manipüle edebilmek için kullandık
library(readr)  #verilerimizi çekmek için kullandığımız paket
library(ggplot2)
dataset_ex <- read_delim("~/GitHub/Statistical-Analyze/datasets/dataset_ex.csv", 
                         delim = "\t", escape_double = FALSE, 
                         trim_ws = TRUE)


head(dataset_ex, 3) #kitlemiz
set.seed(300) #kodu her çalıştırdığımızda örneklemimizin değişmemesi için kullandığımız bir fonksiyon
data_sample <- dataset_ex[sample(nrow(dataset_ex),300),replace=F]
head(data_sample, 3)


#Çocuk var yok sütunu ekleme:
data_sample$Kid_Status <- ifelse(data_sample$Kidhome>0, 1, 0)
  

#9.3 tabulating:
head(table(data_sample$Income,data_sample$Education))
#Bir faktörü tablolamak veya birden fazla faktörden bir beklenmedik durum tablosu oluşturmak için kullanılan fonksiyon


#9.4 stats:
summary(data_sample)
summary(table(data_sample$MntWines,data_sample$MntFruits))
#verilen değişkenlerin bağımsız olup olmadığına ilişkin kullanılan ki kare testi fonksiyonudur.


#9.5 quantile:
#Bir f kesri verildiğinde, verilerinizin karşılık gelen niceliğini bilmek istersiniz. Yani, x gözlemini, x'in altındaki gözlemlerin kesri f olacak şekilde arıyorsunuz.
quantile(data_sample$Income, na.rm=T)


#9.9 Forming a Confidence Interval for a Mean:
#n<30 olabilmesi için 30 örneklem çekip ortalamaya göre güven aralığı bulduk
set.seed(30)
data_sample_30 <- data_sample[sample(nrow(data_sample),30),]
data_sample_30_dbl <- select(data_sample_30, -c(Education, Marital_Status, Dt_Customer))
data_sample_30_dbl
#T testini yapabilmek için 36. satırdaki 3 değişkeni yokettik.
t.test(data_sample_30_dbl)
#güven aralığı testi yaptık. Bunun sonucunda ortalamaya ilişkin güven aralığı %95 güven düzeyinde: [1502.769;3058.393]



#9.11 Testing a proportion:
# H0: Q = 50.000
# Hs: Q > 50.000 {geliri 50.000 den fazla olanları bulmak için kurduğumuz seçenek hipotezi}
prop.test(173,300,p=0.5,alternative="greater") #H0 reddedildi gelirin 50000 fazla olma olasılığı 0.5 ten fazladır.
prop.test(nrow(data_sample[data_sample$Income>50000,]),300,p=0.5,alternative="greater")



#9.16 Comparing the Locations of Two Samples Nonparametrically:
sample_meat <- as.data.frame(list(data_sample$MntMeatProducts))
colnames(sample_1) <- c("meat")
sample_fish <- as.data.frame(list(data_sample$MntFishProducts))
colnames(sample_2) <- c("fish")
shapiro.test(sample_meat$meat)#p<alpha=0.05 için normal dağılmıyor
shapiro.test(sample_fish$fish)#p<alpha=0.05 için normal dağılmıyor
#H0: balık satışı ile et satışı arasında fark yoktur. 
#Hs: balık satışı ile et satışı arasında fark vardır.
wilcox.test(sample_1$meat,sample_2$fish, alternative = "two.sided")
#p<0.05 için H0 reddedilir.




#9.17 Testing a Correlation for Significance:
sample_wine <- as.data.frame(list(data_sample$MntWines))
colnames(sample_wine) <- c("wine")

sample_income <- as.data.frame(list(data_sample$Income))
colnames(sample_income) <- c("income")

cor.test(sample_income$income,sample_wine$wine)
#Gelir düzeyi ile şarap alımı arasında %76 lık bir doğrusal ilişki vardır.





#9.18 Testing Groups for Equal Proportions
data_sample[data_sample$NumWebPurchases>3,] #0.543
data_sample[data_sample$NumStorePurchases>5,]#0.477
#H0: Aylık web sitesi ziyaretinin 3 ten fazla olma olasılığı ile aylık dükkan ziyaretinin 5 ten fazla olma olasılığı eştittir.
#Hs: P~web~ < P~store~
prop.test(163,300,p=0.5)
prop.test(143,300,p=0.5)





#9.19 Performing Pairwise Comparisons Between Group Means
#H0: M1 = M2
#Hs: M1 != M2
sample_fruit <- as.data.frame(list(data_sample$MntFruits))
colnames(sample_fruit) <- c("fruit")
sample_sweet <- as.data.frame(list(data_sample$MntSweetProducts))
colnames(sample_sweet) <- c("sweet")

pairwise.t.test(data_sample$MntMeatProducts,data_sample$MntFishProducts, alternative = "two.sided")





#9.20 Testing Two Samples for the Same Distribution
#H0: Dağılımlar aynıdır.
#Hs: Dağılımlar aynı değildir.
shapiro.test(data_sample$MntFruits) #normal dağılmıyor
shapiro.test(data_sample$MntFishProducts) #normal dağılmıyor
ks.test(data_sample$MntFishProducts,data_sample$MntFruits)
#p<0.05 için H0 reddedilir. dağılımlar farklıdır.



# 10.4:
attach(data_sample)
ggplot(data_sample, aes(Education, Income, color=Education)) +
  geom_point() +
  theme_bw() + 
  labs(title = "Income change by education level ",
       x = "Education",
       y = "Incomes")
detach(data_sample)



#10.11:
ggplot(data_sample, aes(data_sample$Education,data_sample$Income)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=min(data_sample$Income,na.rm=T), ymax=max(data_sample$Income,na.rm=T)), width=.2)



#10.12:
ggplot(data_sample, aes(data_sample$Education, data_sample$Income, fill = data_sample$Education)) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin=min(data_sample$Income,na.rm=T), ymax=max(data_sample$Income,na.rm=T)), width=.2)



#10.13:
attach(data_sample)
ggplot(data_sample, aes(Income, MntMeatProducts)) +
  geom_line()


#10.15:
dat1<-data_sample %>%
  summarize(Income=Income, pur=NumStorePurchases)
dat2 <- data_sample %>%
  summarize(Income=Income, pur=NumWebPurchases)
ggplot() +
  geom_line(data = dat1, aes(dat1$Income, dat1$pur, color="red")) +
  geom_line(data = dat2, aes(dat1$Income, dat2$pur, color="blue"))



#10.16:
ggplot(data_sample, aes(data_sample$Income, data_sample$MntMeatProducts)) + geom_point() + 
geom_vline(
  xintercept = mean(data_sample$Income , na.rm=T),
  color = "red",
  linetype = "dashed",
  size = 1.5
) +
  geom_hline(yintercept = mean(data_sample$MntMeatProducts, na.rm=T), color = "blue")




#10.17: 
ggplot(data_sample) +
aes(y = Income, fill = "red") +
  geom_boxplot() + theme_dark()




#10.18:
ggplot(data_sample) +
  aes(x=Marital_Status, y = Income, color="red") +
  geom_boxplot() + theme_bw()



#10.20: ????






