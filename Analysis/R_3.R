library(readr)
library(ggplot2)

data <- read_csv("abalone.csv", col_names = TRUE)

df <- subset.data.frame(data, Sex == "M" | Sex == "F")

#Критерий Стьюдента
#Проверка равенства среднего кол-ва колец в раковине моллюска в разбиении по полу
t.test(df$Rings ~ df$Sex)

#Критерий Фишера
boxplot(df$Rings ~ df$Sex,
        data = df, main = "Rings by Sex", 
        xlab = "Sex", ylab = "Rings",
        col = "aquamarine")

shapiro.test(df$Rings[df$Sex == 'M'])
shapiro.test(df$Rings[df$Sex == 'F'])



male <- df$Rings[df$Sex == 'M'][1:length(df$Rings[df$Sex == 'F'])]
female <- df$Rings[df$Sex == 'F']

diff <- with(df, male - female)

shapiro.test(diff)

normal_sample <- rnorm(1307, mean = mean(diff), sd = sd(diff))

par(mfrow = c(1, 2))
hist(diff, freq = FALSE, col = 'aquamarine')
lines(density(diff), col = 'blue')
boxplot(diff, col = 'aquamarine')

par(mfrow = c(1, 1))
qqnorm(diff, col = 'aquamarine')
qqline(normal_sample, col = 'coral')

par(mfrow = c(1, 2))
hist(normal_sample, freq = FALSE, col = 'aquamarine')
lines(density(normal_sample), col = 'blue')
boxplot(normal_sample, col = 'aquamarine')

shapiro.test(normal_sample)


# Генерация нормальнораспределенных выборок
male_norm <- rnorm(1307, mean = mean(male), sd = sd(male))
female_norm <- rnorm(1307, mean = mean(male), sd = sd(male))

shapiro.test(male_norm)
shapiro.test(female_norm)

diff_norm <- with(df, male_norm - female_norm)

par(mfrow = c(1, 2))
hist(diff_norm, freq = FALSE, col = 'aquamarine')
lines(density(diff_norm), col = 'blue')
boxplot(diff_norm, col = 'aquamarine')




#p <- grubbs.test(diff)
#print(p$alternative)
#print(p$p.value)

#while (p$p.value< 0.05) {
#  for (i in 1:length(diff)) {if (diff[i]==max(diff) | diff[i] == min(diff)) { diff[i]=median(diff)}}
#  p <-  grubbs.test(diff)
#  print(p$p.value)
#  print(max(diff))
#  print(p$alternative)
#}

t.test(diff, mu = 0)
t.test(normal_sample, mu = 0)
t.test(diff_norm, mu = 0)


var.test(df$Rings[df$Sex == 'M'], df$Rings[df$Sex == 'F'])
var.test(male_norm, female_norm)

#Критерий Колмогорова-Смирнова
ks.test(male, female)
