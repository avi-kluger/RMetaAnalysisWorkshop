rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

x <- read.csv("https://www.dropbox.com/s/z6g6ma44cfjkn5t/training.csv?dl=1")

# Define moderators
x$Experimental <- ifelse(x$design  == "Experiment", 1, 0)
x$Listener     <- ifelse(x$outcome == "Listener",   1, 0)

browseURL("http://www.metafor-project.org/doku.php/tips")
library(metafor)

# Calculate sampling variance
x$vi <- escalc(measure= "COR", ri  = x$r, ni = x$N)$vi
x$vi

# Prepare ID for metafor
x$id <- 1:nrow(x)

# Run the meta-analysis
res.uni <- rma.uni(r, vi, data = x)

# Write a report
reporter(res.uni, digits = 2)

# Prepare sampling variance for Cohen's d
x$M2 <- rep(0,nrow(x))     
x$SD <- rep(1,nrow(x))
x    <- x[order(-x$N),] 

x$vi <- escalc(
  measure= "SMD", 
               m1i = d	, sd1i = SD,  n1i = N/2,
               m2i = M2	, sd2i = SD,  n2i = N/2,
               data = x)$vi
x$id <- 1:nrow(x)

# Add multiple moderators
res.uni <- rma.uni(r, vi, mods = ~ design + outcome + N, data = x)

# Note the risk of second-order sampling error
res.uni

