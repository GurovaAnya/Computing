# Задание 2

# 1. 1.	Загрузите файл demography.csv. В нём содержатся данные по населению 
# Белгородской и Калужской областей за 2016 год (источник — Росстат).

install.packages("ggplot2")
library("ggplot2")
df <- read.csv("https://raw.githubusercontent.com/allatambov/R-programming-3/master/seminars/sem8-09-02/demography.csv", encoding = "UTF-8")

# 2.	Создайте переменную young_share — процент населения возраста, моложе трудоспособного.

young_share <- df$young_total/df$popul_total*100
df <- df %>% mutate(young_share = young_share)

# trud_share — процент населения трудоспособного возраста 
trud_share <- df$wa_total/df$popul_total*100
df <- df %>% mutate(trud_share = trud_share)

# old_share — процент населения возраста, старше трудоспособного
old_share = df$ret_total/df$popul_total*100
df <- df %>% mutate(old_share = old_share)

# 3. Постройте гистограмму для доли трудоспособного населения в процентах. 
# Измените цвет гистограммы, добавьте rugs. Добавьте вертикальную линию, 
# которая отчерчивает медианное значение доли трудоспособного населения в процентах
ggplot(data = df, aes(x = trud_share)) + geom_histogram(fill = "tomato", color = "black") 
+geom_vline(xintercept = median(df$trud_share), color = "blue") + geom_rug()


# 4. Постройте сглаженные графики плотности распределения для доли трудоспособного 
# населения в процентах по регионам (два графика в одной плоскости). 
# Настройте цвета и прозрачность заливки. По графикам плотности определите, 
# имеет ли смысл для визуализации распределения доли трудоспособного населения строить 
# скрипичные диаграммы (violin plot). Если да, постройте их (так же по группам). 
# Если нет, постройте ящики с усами.

ggplot(df, aes(x = trud_share, fill = factor(region))) + geom_density(alpha = 0.5)
ggplot(df, aes(y = trud_share, x = region)) + geom_violin(alpha = 0.5, trim=FALSE, aes(fill = region), show.legend = F) + geom_boxplot(width=0.1)


# 5.	Постройте диаграмму рассеяния для переменных young_share и old_share. Можно ли сказать, 
# что чем больше процент молодого населения (моложе трудоспособного населения), тем меньше процент 
# пожилых людей (старше трудоспособного возраста)? Поменяйте цвет и тип маркера для точек.

ggplot(df, aes(x = young_share, y = old_share)) + geom_jitter(color = "red", shape = "star")

# 6. Создайте переменную male_share — доля мужского населения в районе/городе (в процентах). 
# Создайте переменную male, которая принимает значение 1, если доля мужчин в муниципальном 
# районе/городе больше доли женщин, и значение 0 — во всех остальных случаях.

male_share <- (df$wa_male + df$ret_male + df$young_male)/df$popul_total*100
df <- df %>% mutate(male_share = male_share)
df <- df %>% mutate(male = male_share > 50)

# 7.	Постройте пузырьковую диаграмму (bubble plot) для переменных young_share и old_share, 
# учитывая информацию о доле мужчин в районе и о том, преобладают ли мужчины в районе или нет.

ggplot(df, aes(young_share, old_share, size = male_share, color = male)) + geom_count(alpha = 0.3)


# 8. Постройте столбиковую диаграмму (bar plot), которая показывала бы, сколько в базе данных районов 
# Белгородской области, а сколько — Калужской.

ggplot(df, aes(x=factor(region))) + geom_bar()


