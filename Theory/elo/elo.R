# source("D:\\dotnet\\Elovuon\\Theory\\elo\\elo.R")
mydata <- read.csv("D:\\dotnet\\Elovuon\\Theory\\elo\\elo.csv")
loss <- glm(loss ~ diff + sum, data = mydata, family = "binomial")
loss$coefficients
draw <- glm(draw ~ abs + sum, data = mydata, family = "binomial")
draw$coefficients
win <- glm(win ~ diff + sum, data = mydata, family = "binomial")
win$coefficients
