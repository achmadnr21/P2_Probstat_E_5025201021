# Soal 1
# a
# Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas

# Data sebelum aktivitas
before <- c(78, 75, 67, 77, 70, 72, 28, 74, 77)

# Data setelah aktivitas
after <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)

# Cek data
my_data <- data.frame(
  group = rep(c("sebelum", "sesudah"), each = 9),
  saturation = c(before, after)
)

# Di print
print(my_data)

# Standar Devisiasi sebelum aktivitas
sd_before <- sd(before)
sd_before

# Standar Devisiasi setelah aktivitas
sd_after <- sd(after)
sd_after

# b
# carilah nilai t (p-value)

# Mencari nilai t(p-value)
t.test(before, after, alternative = "greater", var.equal = FALSE)

# c

var.test(before, after)

t.test(before, after, mu = 0, alternative = "two.sided", var.equal = TRUE)


# Soal 2
#install.packages("lattice")
#install.packages("BSDA")
library(BSDA)

# a
# Jawaban di ss

# b 
tsum.test(mean.x=23500, sd(3900), n.x=100)

# c
# Jawaban di ss

# Soal 3
# a
# H0 dan H1
# Jawaban di ss

# b 
# Hitung Sampel Statistik
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, 
          mean.y =2.79 , s.y = 1.32, n.y = 27, 
          alternative = "greater", var.equal = TRUE)

# c
# Uji Statistik (df =2)
install.packages("mosaic")
library(mosaic)
plotDist(dist='t', df=2, col="blue")

# d
# Nilai Kritikal
qchisq(p = 0.05, df = 2, lower.tail=FALSE)

# e
# Jawaban di ss

# f
# Jawaban di ss

# Soal 4
# a
# Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup
# 2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
# lihat apakah ada outlier utama dalam homogenitas varians.

myFile  <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"))
dim(myFile)
head(myFile)
attach(myFile)
#Buat myFile menjadi group
myFile$Group <- as.factor(myFile$Group)
myFile$Group = factor(myFile$Group,labels = c("Kucing Oren","Kucing Hitam","Kucing Putih"))

class(myFile$V1)

group1 <- subset(myFile, V1=="Kucing Oren")
group2 <- subset(myFile, V1=="Kucing Hitam")
group3 <- subset(myFile, V1=="Kucing Putih")

# b
# carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
# didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
bartlett.test(Length~V1, data=dataoneway)

# c
# Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
# Grup dan beri nama model tersebut model 1.
qqnorm(group1$Length)
qqline(group1$Length)

# d
# Jawaban di ss

# e
model1 <- lm(Length~Group, data=myFile)

anova(model1)

TukeyHSD(aov(model1))

# f
library(ggplot2)
ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + 
  scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")


# Soal 5
# a
install.packages("multcompView")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read_csv("GTL.csv")
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

# b
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

# c
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

# d
tukey <- TukeyHSD(anova)
print(tukey)

# e
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)

write.csv("GTL_summary.csv")

