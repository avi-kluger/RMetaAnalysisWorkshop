rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

x <- read.csv("https://www.dropbox.com/s/z6g6ma44cfjkn5t/training.csv?dl=1")

# Why a three-level analysis is needed?
data.frame(table(x$Article))

# Define moderators
x$Experimental <- ifelse(x$design  == "Experiment", 1, 0)
x$Listener     <- ifelse(x$outcome == "Listener",   1, 0)

library(metafor)
x$M2 <- rep(0,nrow(x))     
x$SD <- rep(1,nrow(x))
x    <- x[order(-x$N),] 

x$vi <- escalc(
  measure= "SMD", 
               m1i = d	, sd1i = SD,  n1i = N/2,
               m2i = M2	, sd2i = SD,  n2i = N/2,
               data = x)$vi
x$id <- 1:nrow(x)

# Run the meta-analysis
res.mv <- rma.mv(d, vi, random =  ~  factor(id) | Article,
                 data = x)
print(res.mv, digits=2)

res.mv <- rma.mv(d, vi, 
                 mods = ~ design,
                 random =  ~  factor(id) | Article,
                 data = x)
print(res.mv, digits=2)

print(res.mv, digits=2)
profile(res.mv, sigma1=1)
profile(res.mv, sigma2=1)

confint(res.mv)


aggregate(Effect.r ~ Construct_2, 
          data = x[which(x$Construct_1 == "Leadership"),], length)

ES <- aggregate(Effect.r ~ Construct_2, 
                data = x[which(x$Construct_1 == "Leadership"),], 
      function(x) {
        c(k = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x))
        })
ES[, -1] <- round(ES[, -1], 2)
ES

res.mv <- rma.mv(Effect.r, vi, random =  ~   factor(id) | FullArticle, 
                 data = x[which(x$Construct_2 == "Wellbeing:Subjective wellbeing"), ])

res.mv <- rma.mv(Effect.r, vi, random =  ~   factor(id) | FullArticle, 
                 data = x, subset = which(x$Construct_2 == "Wellbeing:Subjective wellbeing"))
print(res.mv, digits=3)
profile(res.mv, sigma2=1)

#cluster automatically do to subsetting used in rma.mv
robust(res.mv, cluster = x$Article)

#sunset plot
library(metaviz)
viz_sunset(res.mv)

#print k
colSums(model.matrix(res.mv))

# forest plot for moderator levels
forest(coef(res.mv),
       diag(vcov(res.mv)),
       slab = names(coef(res.mv)),
       subset = order(coef(res.mv)))
	

