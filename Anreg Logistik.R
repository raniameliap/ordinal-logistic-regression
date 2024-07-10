library(readxl)
Data_Minipoject<- read_excel("C:/Users/Lenovo/OneDrive/Documents/Mini Project/Data2.xlsx")
View(Data_Minipoject)
## three way cross tabs (xtabs) and fl
#ubah numerik menjadi ordered category(termasuk labeling)
Data_Minipoject$Kat_Usia2<-factor(Data_Minipoject$Kat_Usia,
                                  levels =c("Remaja","Dewasa","Lansia")) 
Data_Minipoject$Rating_Produk2<-ordered(Data_Minipoject$Rating_Produk,
                                        levels=c(1,2,3,4,5),
                                        labels=c("Sangat buruk","Buruk","Cukup baik","Baik","Sangat baik"))
Data_Minipoject$Jenis_Pakaian2<-factor(Data_Minipoject$Jenis_Pakaian,
                                       
                                       levels=c("Bottoms","Tops","Dresses")) 
#tabel kontingensi
ftable(xtabs(Jumlah ~ Kat_Usia2 + Jenis_Pakaian2 + Rating_Produk2 , 
             data = Data_Minipoject))
str(Data_Minipoject)
library(MASS)
reglog <- polr(Rating_Produk2 ~ Kat_Usia2 + Jenis_Pakaian2, data = 
                 Data_Minipoject, weights = Jumlah, Hess=TRUE,method = "logistic")
summary(reglog)
#multikolinieritas
library(car)
vif(reglog)
#uji simultan
library(pscl)

qchisq(0.99, 3)
options("scipen"=100, digits = 5)
## store table
(ctable <- coef(summary(reglog)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))
#CI
ci<-confint(reglog)
## odds ratios``
exp(coef(reglog))
exp(cbind(OR = coef(reglog),ci))

pR2(reglog)


