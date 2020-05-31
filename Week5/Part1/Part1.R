# 1. Загрузите данные из файла VILLA.xls

library(readxl)
villa <- read_excel("C:/Users/anna3/Desktop/computing/Computing/Week5/Part1/villa.xls")
View(villa)


# 2. Определите тип данных, с которыми Вы работаете.

str(villa)


# 3. Рассчитайте и проинтерпретируйте описательные статистики по каждой переменной, включая фиктивную переменную.

summary(villa)
library(dplyr)

# уберем лишние значения
villa <- filter(villa, Eco != 2)
View(villa)

# 4. Проанализируйте исходную выборку на наличие статистических выбросов, используя анализ ящичковых диаграмм. Сделайте выводы.
library(ggplot2)
library(reshape)
villaValues <- select(villa, -c("N"))
villaMelt <- melt(as.data.frame(villaValues))

ggplot(villaMelt,aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = variable)) +
  facet_wrap( ~ variable, scales = "free") +
  theme(legend.position = "none")


# 5. Проверьте однородность всех  переменных с помощью коэффициента вариации по каждой переменной. Сделайте выводы. 

v <- function(x){
  return(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100)
}

apply(villaValues, 2, v)


# 6. Проверьте нормальность распределения переменной Price с помощью:

# a.	гистограммы

hist(villaValues$Price, breaks = 20, freq = F, main = "Гистограмма цены", col = "tomato")
curve(dnorm(x, mean(villaValues$Price), sd(villaValues$Price)), 
      add = T)


# b.	коэффициентов асимметрии и эксцесса

library("moments")

kurtosis(villa$Price)
skewness(villa$Price)

# c.	графика Q-Qplot

qqnorm(villa$Price, main = "Q-Q Plot Цены")
qqline(villa$Price)

# d.	проверки гипотезы о нормально распределении ( на уровне значимости 0,05) с помощью критериев: 
# Колмогорова-Смирнова
library(nortest)

ks.test(villa$Price, "pnorm", mean = mean(villa$Price, na.rm = T), 
        sd = sd(villa$Price, na.rm = T))

# Шапиро-Уилка
shapiro.test(villa$Price)

# Лиллифорса
lillie.test(villa$Price)

# Крамера-фон Мизеса и Андерсона-Дарлинга
cvm.test(villa$Price)
ad.test(villa$Price)

# Шапиро-Франсиа
sf.test(villa$Price)

# хи-квадрат Пирсона 
pearson.test(villa$Price)
