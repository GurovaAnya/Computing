#task1
income <- 100000
log_income = log(income)
income_pre <-500000
now_bigger <- income > income_pre
now_bigger

#task2
x <- 2
y <- 4
z <- x
x <- y
y <- z
print(x)
print(y)

#task3
x <- 3.5
y <- "2,6"
z <- 1.78
h <- TRUE
typeof(x)
typeof(y)
typeof(z)
typeof(h)

#task4
q <- c(4, 7, -1, 21, 2, 0, 14)
q_sq <- q^2
q_log <- log(q)
#Область определения функции логарифма - (0; +беск)
print(q[q >= 0])
print(q[q %% 7 == 0])
print(q_log[q_log %% 2 == 0 && q_log > 5])

#task5
turnout <- c(100, 124, 121, 130, 150, 155, 144, 132, 189, 145, 125, 110, 118, 129, 127)
which(turnout %% 5 == 0)
w <- length(which(turnout %% 5 == 0))/length(turnout) * 100
round(w, 2)

#task6
z <- c(8, NA, 7, 10, NA, 15, NA, 0, NA, NA, 87)
print(which(is.na(z)))

#task7
s <- c("4,5", "6,8", "9,2", "1,75")
n <- as.numeric(gsub(",", ".", s))

#task8
b <- c(40,50)
A <- array(c(1, 50, 1, 75), dim=c(2,2))
AA <- t(A) %*% A
Ab <- t(A) %*% b
solve(AA, Ab)

A <- array(c(1,2,3,4,2,7,6,9,3,6,3,8,4,9,8,2), dim = c(4,4))
solve(A) #A-1
t(A) #At
diag(A) #главная диагональ
sum(diag(A)) #след
det(A) #определитель
A[-2,-3] #алгебраическое дополнение

