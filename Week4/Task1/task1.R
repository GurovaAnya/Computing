# Задание 1

# 1. Импортируйте набор данных с именем «AppleStore» в R.
install.packages("xlsx")
library(xlsx)

AppleStore <- read.xlsx("AppleStore.xlsx", sheetName = "AppleStore")
View(AppleStore)


# 2. Создайте новый фрейм данных, который будет содержать все переменные, 
# кроме «id» и «currency». Назовите этот новый фрейм данных как df2.

library(dplyr)
df2 <- select(AppleStore, -c(id, currency))
View(df2)


# 3. Изучите структуру нового набора данных (df2) и предоставьте анализ 
# общей информации об этом наборе данных (что такое единица наблюдения, 
# сколько переменных и наблюдений, какие переменные находятся в наборе данных 
# и какие они типы).

str(df2)


# 4. Анализ суммарной статистики переменных «цена», «user_rating» 
# и «lang_num», «size_bytes».

install.packages("dplyr")
library(dplyr)

summary(df2[, c("price", "user_rating", "lang_num", "size_bytes")])



# 5. Какое приложение имеет наибольшее количество языков?

biggest <- AppleStore[which.max(df2$lang_num), "name"]
print(biggest)

# 6. Определите квантили переменных «цена», «user_rating» и «lang_num».
quantile(df2$price)
quantile(df2$user_rating)
quantile(df2$lang_num)

# 7. Для всех количественных переменных рассчитать коэффициенты эксцесса 
# и асимметрии и коэффициент вариации. Сделать выводы.

install.packages("moments")
library("moments")

v <- function(x){
  return(sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)*100)
}

num <- select(AppleStore, size_bytes, price, rating_count_tot, user_rating, lang_num)
apply(num, 2, kurtosis)
apply(num, 2, skewness)
apply(num, 2, v)

# 8. Для всех количественных переменных построить  Boxplot.
# Обязательно сделать подписи на графике. 
# Сделать выводы о наличии выбросов. 

boxplot(df2$size_bytes, ylab = "Bytes")
boxplot(df2$price, ylab = "Ценв")
boxplot(df2$rating_count_tot, ylab = "Raitings")
boxplot(df2$user_rating, ylab = "Stars")
boxplot(df2$lang_num, ylab = "Languages")


# 9. Для всех качественных данных построить круговые диаграммы. 

pie(table(df2$prime_genre), main = "Жанры", xlab = "Вид жанра", radius = -1)


# 10. Для всех количественных переменных построить  
# гистограммы с плотностью нормального распределения.
# Сделать выводы.

hist(df2$price, main = "Плотность распределения цены", xlab = "Цена", ylab = "Плотность", freq = FALSE)
curve(dnorm(x, mean(df2$price), sd = sd(df2$price)), add = TRUE)

hist(df2$size_bytes, main = "Плотность распределения размера", xlab = "размер", ylab = "Плотность", freq = FALSE)
curve(dnorm(x, mean(df2$size_bytes), sd = sd(df2$size_bytes)), add = TRUE)

hist(df2$rating_count_tot, main = "Плотность распределения количества рейтингов", xlab = "Количество рейтингов", ylab = "Плотность", freq = FALSE)
curve(dnorm(x, mean(df2$rating_count_tot), sd = sd(df2$rating_count_tot)), add = TRUE)

hist(df2$user_rating, main = "Плотность распределения рейтингов", xlab = "Рейтинг", ylab = "Плотность", freq = FALSE)
curve(dnorm(x, mean(df2$user_rating), sd = sd(df2$user_rating)), add = TRUE)

hist(df2$lang_num, main = "Плотность распределения количесва языков", xlab = "Количество языков", ylab = "Плотность", freq = FALSE)
curve(dnorm(x, mean(df2$lang_num), sd = sd(df2$lang_num)), add = TRUE)


# 11. Какой жанр наиболее распространен? 

tempGenres <- summary(df2$prime_genre)
biggestGenre = tempGenres[tempGenres == max(tempGenres)]
print(biggestGenre)

# 12. Создайте новый фрейм данных из существующего фрейма данных df2, 
# чтобы новый фрейм данных содержал только приложения, соответствующие 
# наиболее распространенному жанру. Рассчитайте сводную статистику переменных, 
# которые вы проанализировали в (4) для нового фрейма данных, и сравните их 
# с результатами в (4). Что вы можете сказать о цене, рейтинге пользователей и 
# количестве языков приложений, относящихся к наиболее распространенному жанру, 
# по сравнению со всей выборкой?

nameOfGenre <- attr(biggestGenre, "name")
new_df <- filter(df2, df2$prime_genre == nameOfGenre)
View(new_df)
summary(new_df$prime_genre)
summary(new_df[, c("price", "user_rating", "lang_num", "size_bytes")])


# 13 Проверьте, используя критерий Колмогорова-Смирнова, гипотезу о нормальности распределения 
# показателя «цена»  по группам. Проделайте то же самое, используя критерий Шапиро-Уилка. 
# Сделайте выводы.

gr1 <- filter(AppleStore, AppleStore$prime_genre == "Games")
gr2 <- filter(AppleStore, AppleStore$prime_genre != "Games")

# группа 1 - игры
mean_games <- mean(gr1$price)
sd_games <- sd(gr1$price)
ks.test(gr1$price, "pnorm", mean_games, sd_games)

# группа 2 - не игры
mean_nogames <- mean(gr2$price)
sd_nogames <- sd(gr2$price)
ks.test(gr2$price, "pnorm", mean_nogames, sd_nogames)

# критерий Шапиро-Уилка
# группа 1 - игры
shapiro.test(gr1$price)

# группа 2 - не игры
shapiro.test(gr2$price)


