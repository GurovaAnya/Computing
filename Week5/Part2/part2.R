# task 1

# 1. Загрузите данные из файла VILLA.xls

library(readxl)
villa <- read_excel("C:/Users/anna3/Desktop/computing/Computing/Week5/Part1/villa.xls")
villa <- subset(villa, Eco != 2)
View(villa)



# 2. Определите тип данных, с которыми Вы работаете.

str(villa)

# 3. Проверьте нормальность распределения переменной Price с помощью критерия Лиллифорса.

library(nortest)
lillie.test(villa$Price)


# 4. Проверьте гипотезу о равенстве дисперсий Цены коттеджей (Price) в двух совокупностях (рядом с озером и нет).

wilcox.test(Price ~ Eco, villa)


# 5. 


# task 2

psych_survey <- read.csv(file = "C:/Users/anna3/Desktop/computing/Computing/Week5/Part2/psych_survey.csv", sep = ";", dec = ",")
psych_survey <- subset(psych_survey, !is.na(subject) & !is.na(height))
View(psych_survey)


# Колмогорова-Смирнова
library(nortest)

ks.test(psych_survey$height, "pnorm", mean = mean(psych_survey$height, na.rm = T), 
        sd = sd(psych_survey$height, na.rm = T))
shapiro.test(psych_survey$height)


anova <- aov(height ~ subject, data = psych_survey)
summary(anova)




