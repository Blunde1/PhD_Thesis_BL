library(tidyverse)
#library(gbtorch)
library(xgboost)
library(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# loss function
loss_mse <- function(y,y.hat){
    mean((y-y.hat)^2)
}
n <- 100
d <- 0.1
set.seed(1)
x <- as.matrix(runif(n,0,5))
y <- rnorm(n, x, 1)
x.test <- as.matrix(runif(n*100,0,5))
y.test <- rnorm(nrow(x.test), x.test, 1)
df <- data.frame(x,y)

p_dtr <- df %>%
    ggplot() + 
    geom_point(aes(x, y), shape=15, colour="#E69F00", size=1.5, alpha=1) + 
    theme_bw()
p_dtr

#xgb
K=1000
bst <- xgboost(data = x, label = y, max.depth = 3, eta = d, nthread = 2, nrounds = K, objective = "reg:squarederror")
pred.f.xgb <- function(x, K){
    predict(bst, as.matrix(x), ntreelimit=K)
}
p_dtr + stat_function(fun=pred.f.xgb, args=list(K=200), colour="blue", size=1)
l.train <- l.test <- l.asym <- numeric(K)
for(k in 1:K){
    # pred using first k values
    pred.train <- predict(bst, newdata = x, ntreelimit = k)
    pred.test <- predict(bst, newdata = x.test, ntreelimit = k)
    l.train[k] <- loss_mse(y, pred.train)
    l.test[k] <- loss_mse(y.test, pred.test)
    l.asym[k] <- l.train[k] + 0.5*(l.test[k] - l.train[k])
}
which.min(l.test)
p1 <- p_dtr + stat_function(fun=pred.f.xgb, args=list(K=1), colour="#56B4E9", size=0.8) + 
    ggtitle(paste0("First iteration: K=",1))
p2 <- p_dtr + stat_function(fun=pred.f.xgb, args=list(K=which.min(l.test)), colour="#56B4E9", size=0.8) + 
    ggtitle(paste0("Best iteration: K=",which.min(l.test)))
p3 <- p_dtr + stat_function(fun=pred.f.xgb, args=list(K=K), colour="#56B4E9", size=0.8) + 
    ggtitle(paste0("Last iteration: K=",K))



plot(1:K, l.train)
points(1:K, l.test)
points(1:K, l.asym)

theme_set(theme_bw())
cbp2 <- c("Training loss" = "#000000", 
          "Generalization loss" = "#E69F00", 
          "Asymptotic loss" = "#56B4E9", 
          "Training loss + 2 * Expected m-order distance" = "#009E73",
          "Training loss + 2 * Expected distance leaves only" = "#F0E442", 
          "#0072B2", "#D55E00", "#CC79A7")
df <- data.frame(iterations=1:K, training_loss=l.train, generalization_loss=l.test, asymptotic_loss=l.asym)
p <- df %>%
    ggplot() + 
    geom_line(aes(x=iterations, y=training_loss, colour="Training loss"), size=0.8) + 
    geom_line(aes(x=iterations, y=generalization_loss, colour="Generalization loss"), size=0.8) + 
    #geom_line(aes(x=iterations, y=asymptotic_loss, colour="Asymptotic loss"), size=2) + 
    scale_color_manual(name=NULL, values=cbp2) +
    scale_x_continuous(trans='log2', breaks=2^seq(0,10,2)) + 
    xlab("Boosting iterations") + 
    ylab("Loss") + 
    #ggtitle("Loss in tree-ensemble versus number of trees") + 
    theme(legend.position=c(.75, .75),legend.background=element_rect(colour='black'))
p

plot(grid.arrange(p, p1, p2, p3, layout_matrix=rbind(c(1,1,1), c(2,3,4))))

if(F)
{
    pdf(file="../Fig/gtb_complexity.pdf", width = 8,height = 5)
    plot(grid.arrange(p, p1, p2, p3, layout_matrix=rbind(c(1,1,1), c(2,3,4))))
    dev.off()
}
