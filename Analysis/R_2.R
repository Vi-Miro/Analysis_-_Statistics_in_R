library(outliers)
library(EnvStats)
library(readr)
library(ggplot2)


data <-c(3850, 3730, 3870, 4020, 3840, 3830, 3810, 3690, 3740, 3800)
grubbs.test(data)

for (i in 1:length(data)) {if (data[i]==max(data)) { data[i]=median(data)}}
grubbs.test(data)

data <- c(3850, 3730, 3870, 4020, 3840, 3830, 3810, 3690, 3740, 3800, 5000)
p <- grubbs.test(data)
print(p$alternative)
print(p$p.value)

while (p$p.value< 0.05) {
  for (i in 1:length(data)) {if (data[i]==max(data)) { data[i]=median(data)}}
  p <-  grubbs.test(data)
  print(p$p.value)
  print(max(data))
  print(p$alternative)
}

data <- c(2000, 3850, 3730, 3870, 4020, 3840, 3830, 3810, 3690, 3740, 3800, 5000, 3800, 4000,
       3000, 2300, 3700,4900, 2000,2100,3100)
data <- data.frame(data)
rosnerTest(data$data, k=10, alpha = 0.05, warn = TRUE)



data <- read_csv("abalone.csv", col_names = TRUE)
df <- sqrt(data$Rings)

boxplot(df, col = 'aquamarine', main = 'Boxplot of sqrt(data$Rings)')

p <- grubbs.test(df)
cat('Критерий Граббса: ', p$alternative, ', p-value', p$p.value)
print(p$alternative)
print(p$p.value)

rosnerTest(df, k=9, alpha = 0.05, warn = TRUE)






# Функция для определения k в зависимости от n
n <- 4177

get_k <- function(n) {
  if (n >= 20 & n <= 55) return(3)
  if (n >= 56 & n <= 250) return(3.5)
  if (n >= 251 & n <= 1700) return(4)
  if (n >= 1701 & n <= 10000) return(4.5)
  return(NA)  # Если n не попадает в указанные диапазоны
}

# При неизвестной дисперсии
mean_unknown <- mean(df)
sd_unknown <- sd(df)
k_unknown <- get_k(n)

# Найдем выбросы по правилу |x̄ - xi| / s > k
outliers_unknown <- df[abs(mean_unknown - df) / sd_unknown > k_unknown]

# При известной дисперсии (используем генеральное стандартное отклонение)
sigma_known <- std_dev  # Генеральное СКО, рассчитанное ранее
k_known <- get_k(n)

# Найдем выбросы по правилу |x̄ - xi| / σ > k
outliers_known <- df[abs(mean_unknown - df) / sigma_known > k_known]


par(mfrow = c(1, 1))
# Ящичная диаграмма для неизвестной дисперсии
boxplot(df, col = "aquamarine")
points(outliers_unknown, col = "red", pch = 19)
abline(h = c(mean_unknown - k_unknown * sd_unknown, mean_unknown + k_unknown * sd_unknown), col = "blue", lty = 2)

cat('По правилу трех сигм: ', outliers_known)

