#part1

#task1
name <- readline("Enter your name： ")
print(paste0("Hello, ", name, "!"))

#task2

#with dots
num1 <- as.numeric(readline("Enter the 1st number: "))
num2 <- as.numeric(readline("Enter the 2nd number: "))
print(num1 + num2)

#with commas
num1 <- as.numeric(sub(",", ".", readline("Enter 1st number")))
num2 <- as.numeric(sub(",", ".", readline("Enter the 2nd number: ")))
print(num1 + num2)

#task3
kmph <- as.numeric(readline("Enter speed in kilometres per hour: "))
mps <- kmph/3.6
print(mps)

#part2

#task1
vec <- c(1, 0, 2, 3, 6, 8, 12, 15, 0, NA, NA, 9, 4, 16, 2, 0)
vec[1]
vec[length(vec)]
vec[3:5]
vec[vec == 2]
vec[vec > 4]
vec[vec %% 3 == 0]
vec[vec > 4 & vec %% 3 == 0]
vec[vec > 5 | vec < 1]
vec[vec == 0]
vec[!vec < 2 & !vec > 8]

#task2
test <- (1)
test[length(test)] = NA
test

#task3
which(is.na(vec))

#task4
length(which((is.na(vec))))

#task5
seq <- 1:100
seq

#task6
country <- rep(c("France", "Italy", "Spain"), each = 5)
country
year <- rep(2000:2004,3)
year

#task7
income <- c(10000, 32000, 28000, 150000, 65000, 1573)
average <- sum(income)/length(income)
income_class <- c(0)
income_class[income < average] <- 0
income_class[income >= average] <- 1
income_class

