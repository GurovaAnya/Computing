# TASK2

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

villa2 <- subset(villa2, !is.na(villa2$Price) & !is.na(villa2$area))

# 3.	Построить поле корреляции. 

df <- data.frame(villa2$Price, villa2$Dist, villa2$house, villa2$area, villa2$Eco)

pairs(df)


# 4.	Оценить модель с помощью метода наименьших квадратов. 
# Найдите характеристики модели  как t-статистики и коэффициент детерминации R2  
# и скорректированный коэффициент детерминации, критерии Шварца и Акайке. 


lw <- lm(formula= Price~house+area, data=villa2)
summary(lw)

AIC(lw)
BIC(lw)

# 5.  Проверить модель на мультиколлинеарность  

vif(lw)
# 6

# Проанализируем предложенные варианты
lw1 <- lm(formula = Price~log(house)+log(area)+Eco, data = villa2)
summary(lw1)
AIC(lw1)
BIC(lw1)
vif(lw1)

lw2 <- lm(formula = log(Price)~log(house)+area+log(Dist)+Eco, data = villa2)
summary(lw2)
AIC(lw2)
BIC(lw2)
vif(lw2)

lw3 <- lm(formula = log(Price)~log(house)+log(area)+log(Dist)+Eco, data = villa2)
summary(lw3)
AIC(lw3)
BIC(lw3)
vif(lw3)

lw4 <- lm(formula = Price~house+I(area*Dist)+Eco, data = villa2)
summary(lw4)
AIC(lw4)
BIC(lw4)
vif(lw4)

lw5 <- lm(formula = Price~log(house)+log(area), data = villa2)
summary(lw5)
AIC(lw5)
BIC(lw5)
vif(lw5)

lw6 <- lm(formula = log(Price)~log(Dist)+log(area), data = villa2)
summary(lw6)
AIC(lw6)
BIC(lw6)
vif(lw6)
