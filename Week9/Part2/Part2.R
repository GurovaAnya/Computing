# TASK1

# 1.	Загрузите данные из файла VILLA2.csv 

library(readr)
villa2 <- read_delim("C:/Users/anna3/Desktop/computing/Computing/Week8/Part 1/villa2.csv",
                     ";", escape_double = FALSE, trim_ws = TRUE)
View(villa2)
villa2$Dist <- sub(',', '.', villa2$Dist)
villa2$Dist <- as.numeric(villa2$Dist)


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

villa2 <-na.omit(villa2)


ggplot(villaMelt,aes(x = variable, y = value)) +
  geom_boxplot(aes(fill = variable)) +
  facet_wrap( ~ variable, scales = "free") +
  theme(legend.position = "none")
View(villa2)

villa2$Price <- remove_outliers(villa2$Price)
villa2$area <- remove_outliers(villa2$area)

villa2 <-na.omit(villa2)

# 3.	Выберите оптимальное число кластеров, начертите соответствующие графики и проверьте гипотезу.

# Нужные столбцы
library(dplyr)
numeric_villa <- villa2[-1]

rownames(numeric_villa) <- villa2$N
rownames(numeric_villa)



# Обязательно приводим к одной шкале
Mdist <- dist(scale(numeric_villa))

hc <- hclust(Mdist, method = "ward.D2") 
plot(hc, cex = 0.6)
rect.hclust(hc, k = 4, border="red") 


# метод согнутого колена
library(factoextra)
fviz_nbclust(numeric_villa, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

fviz_nbclust(numeric_villa, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

groups4 <- cutree(hc, k = 4) 
d1 <- numeric_villa %>% mutate(groups4 = factor(groups4), number = rownames(numeric_villa))


# Проверка
kruskal.test(d1$Price ~ d1$groups4)
kruskal.test(d1$Dist ~ d1$groups4)
kruskal.test(d1$house ~ d1$groups4)
kruskal.test(d1$area ~ d1$groups4)
kruskal.test(d1$Eco ~ d1$groups4)


# 4.	Проведите кластеризацию методом К-средних. 

cl <- kmeans(numeric_villa, 4)
cl

# 5.	Визуализируйте результаты кластеризации. 
fviz_cluster(cl, data = numeric_villa, ellipse.type = 'convex')