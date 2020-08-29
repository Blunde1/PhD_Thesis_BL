library(gridExtra)
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# lm-boost
n <- 100
d <- 0.5
set.seed(7130)
x <- runif(n,0,5)
y <- rnorm(n, x, 1)
df <- data.frame(x,y)

p <- df %>%
    ggplot() + 
    geom_point(aes(x, y), shape=15, colour="#E69F00", size=1.5, alpha=1) + 
    theme_bw()
p

f0 <- mean(y)
f <- f0
pred <- rep(f, n)
pred.f <- function(x){
    f
}
p + stat_function(fun = pred.f, colour="#56B4E9", size=1.5) 

K <- 10
p.pred.list <- list()
p.res.list <- list()
pred.f.k <- list()
res.list <- list()
mod.coef <- list()
for(k in 1:K){
    #k=1
    res.list[[k]] <- (y-pred)
    
    p.res.list[[k]] <- data.frame(residuals=res.list[[k]], x) %>%
        ggplot() +     
        geom_point(aes(x, residuals), shape=16, colour="#56B4E9", size=1.5, alpha=1) + 
        theme_bw()
    
    
    modk <- lm(y~x, data=data.frame(y=res.list[[k]], x=x))
    mod.coef[[k]] <- modk$coefficients
    pred.f.k[[k]] <- function(x.obs){ predict(modk, newdata=data.frame(x=x.obs))}
    p.res.list[[k]] + stat_function(fun=pred.f.k[[1]], colour="#000000", size=1)
    
    pred <- pred + d * pred.f.k[[k]](x)
    # k=k+1
}

modk.f <- function(x,k){
    a <- as.numeric(mod.coef[[k]]["(Intercept)"])
    b <- as.numeric(mod.coef[[k]]["x"])
    return(a+b*x)
}
modk.f(2, 1)

#d=0.1
lm_boost <- function(x, K, f0){
    y.pred <- f0
    for(i in 1:K){
        #cat(pred.f.k[[i]](x))
        y.pred <- y.pred + d* modk.f(x,i) # pred.f.k[[i]](x)
    }
    return(y.pred)
}
lm_boost(0, 10, f0)

#par(mfrow=c(1,2))
p+     ggtitle("Observations and model ensemble") 
p11 <- p + stat_function(fun = pred.f, colour="#000000", size=0.8) + ggtitle("Update model") 
p12 <- p.res.list[[1]] + ggtitle("Compute residuals")
p13 <- p.res.list[[1]] + stat_function(fun=modk.f, args=list(k=1), colour="#000000", size=0.8)+
    ggtitle("Fit model to residuals")

p21 <- p + stat_function(fun = pred.f, colour="#009E73", size=0.8) +
    stat_function(fun= lm_boost, args=list(K=1, f0=f0), colour="#000000", size=0.8)
p22 <- p.res.list[[2]]
p23 <- p.res.list[[2]] + stat_function(fun=modk.f, args=list(k=2), colour="#000000", size=0.8)


p31 <- p + stat_function(fun = pred.f, colour="#009E73", size=0.8) +
    stat_function(fun= lm_boost, args=list(K=1, f0=f0), colour="#009E73", size=0.8) + 
    stat_function(fun= lm_boost, args=list(K=2, f0=f0), colour="#000000", size=0.8)
p32 <- p.res.list[[3]]
p33 <- p.res.list[[3]] + stat_function(fun=modk.f, args=list(k=3), colour="#000000", size=0.8)

p41 <- p + stat_function(fun = pred.f, colour="#009E73", size=0.8) +
    stat_function(fun= lm_boost, args=list(K=1, f0=f0), colour="#009E73", size=0.8) + 
    stat_function(fun= lm_boost, args=list(K=2, f0=f0), colour="#009E73", size=0.8) + 
    stat_function(fun= lm_boost, args=list(K=3, f0=f0), colour="#000000", size=0.8)
p42 <- p.res.list[[4]]
p43 <- p.res.list[[4]] + stat_function(fun=modk.f, args=list(k=4), colour="#000000", size=0.8)

p51 <- p + stat_function(fun = pred.f, colour="#009E73", size=0.8) +
    stat_function(fun= lm_boost, args=list(K=1, f0=f0), colour="#009E73", size=0.8) + 
    stat_function(fun= lm_boost, args=list(K=2, f0=f0), colour="#009E73", size=0.8) + 
    stat_function(fun= lm_boost, args=list(K=3, f0=f0), colour="#009E73", size=0.8) + 
    stat_function(fun= lm_boost, args=list(K=10, f0=f0), colour="#000000", size=0.8)
p52 <- p.res.list[[5]]
p53 <- p.res.list[[5]] + stat_function(fun=modk.f, args=list(k=5), colour="#000000", size=0.8)

grid.arrange(p11, p12, p13,
                        p21, p22, p23,
                        p31, p32, p33,
                        p41, p42, p43,
                        p51, p52, p53,
                        ncol=3)

if(F)
{
    my_plot2 <- ggpubr::ggarrange(p11, p12, p13,
                                  p21, p22, p23,
                                  p31, p32, p33,
                                  p41, p42, p43,
                                  p51, p52, p53,
                                  ncol=3, nrow=5)

    pdf(file="../Fig/lm_boost.pdf", width = 8,height = 9)
    my_plot2
    dev.off()
}