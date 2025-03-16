library(readr)
library(ggplot2)

data <- read_csv("abalone.csv", col_names = TRUE)

par(mfrow=c(3,3))
for(i in 2:9) {
  hist(data[[i]], main=names(data)[i], col = 'blue')
}

par(mfrow = c(1, 3))
hist(data$Rings, col = 'blue')
hist(sqrt(data$Rings), col = 'blue')

df <- sqrt(data$Rings)

boxplot(df, col = 'aquamarine', main = 'Boxplot of sqrt(data$Rings)')
shapiro.test(df)

# Среднее и стандартное отклонение
mean_value <- mean(df, na.rm = TRUE)
std_dev <- sd(df, na.rm = TRUE)

# Определим границы для выбросов.
low_bound <- mean_value - 3 * std_dev
up_bound <- mean_value + 3 * std_dev

#Найдем выбросы.
out <- df[df < low_bound | df > up_bound]

print("Выбросы")
print(out)

par(mfrow = c(1, 1))
boxplot(df, main = "Правило 3-х сигм" , col = 'aquamarine')
points(rep(1, length(out)), out, col = 'red', pch = 19)


# Проверка на выбросы с использованием 10% выборки.
sample_size <- round(0.1 * length(df))
sample_data <- sample(df, sample_size)

#Вычислим среднее значение и стандартное отклонение для выборки.
sample_mean <- mean(sample_data, na.rm = TRUE)
sample_std_dev <- sd(sample_data, na.rm = TRUE)

#Определим границы для выбросов в выборке.
sample_lower_bound <- sample_mean - 3 * sample_std_dev
sample_upper_bound <- sample_mean + 3 * sample_std_dev

#Найдем выбросы в выборке.
sample_outliers <- sample_data[sample_data < sample_lower_bound | sample_data > sample_upper_bound]

#Выведем выбросы в выборке.
print("Выбросы в 10%")
print(sample_outliers)

boxplot(sample_data, main = "10% выборка" , col = 'aquamarine')
points(rep(1, length(sample_outliers)), sample_outliers, col = 'red', pch = 19)


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


par(mfrow = c(1, 2))
# Ящичная диаграмма для неизвестной дисперсии
boxplot(df, main = "Неизвестная дисперсия")
points(outliers_unknown, col = "red", pch = 19)
abline(h = c(mean_unknown - k_unknown * sd_unknown, mean_unknown + k_unknown * sd_unknown), col = "blue", lty = 2)

# Ящичная диаграмма для известной дисперсии
boxplot(df, main = "Известная дисперсия")
points(outliers_known, col = "red", pch = 19)
abline(h = c(mean_unknown - k_known * sigma_known, mean_unknown + k_known * sigma_known), col = "blue", lty = 2)


outliers_known
