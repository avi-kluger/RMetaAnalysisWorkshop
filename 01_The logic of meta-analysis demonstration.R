rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

##########################################################################
################### Simulate a population with Rho = .10 #################
##########################################################################

set.seed(111); x <- rnorm(100000, mean = 0, sd = 1)
set.seed(222); y <- rnorm(100000, mean = 0, sd = 1)

y      <- (.1 * x) + y
mydata <- as.data.frame(cbind(x, y))
round(cor(mydata), 2) [2, 1]

##########################################################################
############# Take k = 10 samples each with size n = 100 #################
##########################################################################

for (i in 1:10) {
  set.seed(i); assign(paste0("Study", i), mydata[sample(nrow(mydata), 100), ])
}

Studies <- mget(ls(pattern = "Study*"))

r           <- vector()
for (i in 1:10)   r[i] <- (cor(Studies[[i]])[2, 1])
n           <- rep (100, 10)
input       <- as.data.frame(cbind(r, n))
input$Study <- paste0("Study", 1:10)

rm(list=setdiff(ls(), "input"))

##########################################################################
######### Meta-analize the 10 studies assuming fixed model ###############
##########################################################################

if(!require("meta"))install.packages("meta"); suppressMessages(require("meta"))

##########################################################################
######## Choose the function from "meta" that suits your measure #########
######## Common options:                                         #########       
######## 1. metacor  for Pearson's r                             #########
######## 2. metacont for Cohen's d                               #########
######## 3. metabin  for binary data                             #########
##########################################################################

MyMeta  <- metacor(cor = r, 
                   n = n, 
                   studlab = Study,
                   data = input)
summary(MyMeta)
forest (MyMeta)

##########################################################################
######### Converting Effect Sizes to a single metric  ####################
##########################################################################

if(!require("compute.es")) install.packages("compute.es"); require("compute.es")

url <- ('https://cran.r-project.org/web/packages/compute.es/compute.es.pdf')
rstudioapi::sendToConsole("browseURL(url)")

res(r= .15,     n = 100)      # Convert a single correlational effect
res(r= input$r, n = input$n)  # Convert a dataframe

tes(t = 1.40, 30, 70)         # Convert a t test

##########################################################################
######### Converting Effect Sizes to a single metric  ONLINE #############
##########################################################################

browseURL('https://www.campbellcollaboration.org/research-resources/effect-size-calculator.html')

