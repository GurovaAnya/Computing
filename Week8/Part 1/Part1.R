# TASK1

# 1.	Загрузите данные из файла VILLA2.csv 

library(readr)
villa2 <- read_delim("C:/Users/anna3/Desktop/computing/Computing/Week8/Part 1/villa2.csv",
";", escape_double = FALSE, trim_ws = TRUE)
View(villa2)
villa2$Dist <- sub(',', '.', villa2$Dist)
villa2$Dist <- as.numeric(villa2$Dist)
View(villa2)


# 2.	Посмотрите на данные. Если нужно, то удалите из базы выделяющиеся и аномальные наблюдения. 

library(ggplot2)
library(reshape)
library(dplyr)
villaValues <- select(villa2, -c("N"))
villaMelt <- melt(as.data.frame(villaValues))

ggplot(villaMelt,aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = variable)) +
  facet_wrap( ~ variable, scales = "free") +
  theme(legend.position = "none")
# Вывод: выбросы есть у переменных Price, area

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

villa2$Price <- remove_outliers(villa2$Price)
villa2$area <- remove_outliers(villa2$area)



# 3.	Постройте красивые  графики, характеризующие зависимость между переменными.  

ggplot(villa2, aes(x = villa2$house, y =  villa2$Price)) + geom_jitter(aes())
ggplot(villa2, aes(x = villa2$area, y =  villa2$Price)) + geom_jitter(aes())
ggplot(villa2, aes(x = villa2$Dist, y =  villa2$Price)) + geom_jitter(aes())
ggplot(villa2, aes(x = villa2$Eco, y =  villa2$Price)) + geom_jitter(aes())

# 4.	Постройте корреляционную матрицу (коэффициентов Пирсона). 
# Визуализируйте ее. Сделайте выводы о том, какая взаимосвязь между переменными.  

cor(villa2)

library(car)
library(corrgram)  # пакет с коррелограммами
library(gplots)
library(ggm) 
library(corrplot)

corrplot(cor(villa2))


# 5.	На уровне значимости  0,05 проверить гипотезу о значимости парных коэффициентов корреляции.

library(Hmisc)
rcorr(as.matrix(villa2))


# 6.	Найдите частные коэффициенты корреляции. Сделайте выводы. 

matrix <- matrix(nrow = 6, ncol = 6, data = 1)

for (i in 1:6)
  for (j in 1:6)
  if(i!=j)
      matrix[i,j]<-(pcor(c(i,j), cor(villa2)))
matrix

# TASK 2


d <- array(c(12, 12, 6, 1, 15, 10, 4, 0, 10, 20, 25, 15, 5, 25, 30, 20, 0, 5, 10, 15), dim = c(4, 5))

mosaicplot(d, shade = TRUE)

chisq.test(d)