rm(list = ls())                               #Clean the Global Environment
if (is.null(dev.list()) == FALSE) dev.off()   #Clean Plots
cat ("\014")                                  #Clean the R console

MAD <- read.csv("https://www.dropbox.com/s/z6g6ma44cfjkn5t/training.csv?dl=1")

#Trick metacont with a mean of zero for the control group
MAD$M2 <- rep(0,nrow(MAD))     

#Trick metacont with SD of one for both groups
MAD$SD <- rep(1,nrow(MAD))

#Arrange the data with descending order of sample size
MAD    <- MAD[order(-MAD$N),]  

#Meta analysis for Cohen's d with Design as a categorical moderator (subgroup analysis)
library(meta)
z <- metacont(N/2, d, SD, N/2, M2, SD,
        data = MAD,
        sm = "SMD",
        studlab = Source,
        byvar = as.character(MAD$design),
        bylab = "Design")

 #Inspect the results of the meta analysis
z         

z$TotalN <- z$n.e+z$n.c  #Calculate (restore) Total N.
N        <- prettyNum(sum(z$TotalN), big.mark = ",") 
I2       <- paste(round(z$I2, digits =3)*100, "%", sep="")
tau      <- round (z$tau, digits = 2)
Q        <- round (z$Q, digits = 2)
df       <- z$k-1
Qp       <- round(1-pchisq(Q, df=df), digits = 3)
d        <- round(z$TE.random, digits=3)
Qp       <- ifelse (Qp < .001, ".001", Qp)

library(grid); # help(pac=grid) 

### Fixing the png parameters by using high resultion, but also increasing w and h.

png("Training.png", width = 1900, height = 3800, res = 200, pointsize = 16)
forest(z                                             #Add paramaters to forest
       ,comb.fixed=FALSE                             #Supress fixed-effect summary
       ,rightlabs=c("d", "95% CI    ", "Weight" )    #Rename columns (e.g., SMD -> d)
       ,leftlabs=c("(First) Author", "N" )           #Rename new columns
       ,hetstat = FALSE                              #Supress heterogeneity tests from forest
       ,smlab = ""                                   #Supress lable on top of plot                   
       ,xlim= c(-1, 2)                               #User defined range of d values to show in plot
       ,col.square="aquamarine3"
       ,leftcols=c("studlab","TotalN" )              #Choose variables from z to show on plot
       ,prediction = TRUE)                           #Show prediction interval  
grid.text( bquote(.("Training")~~--~~italic(k)==.(z$k)~~italic(N)==.(N)), 
           .50, .90, gp = gpar(cex = 1.5)) #cex = Multiplier applied to fontsize.
grid.text( bquote(italic(I)^{2}==.(I2)~~tau==.(tau)~~chi[.(df)]^{2}==.(Q)~~italic(p)==.(Qp)), 
           .25, .10, gp=gpar(cex = 1))
dev.off()

funnel(z)                                            #Funnel plot of object class meta
funnel(z, xlab= paste("Estimated Cohen's d = ", d) ) #Adding lable to the funnel plot


trimfill(z)                                          #Trim and fill analysis
tf <- trimfill(z)                                    #creating a trim and fill object
#Funnel plot of the trim and fill with x labeled with the effect size
funnel(trimfill(z), xlab= paste("Estimated Cohen's d = ", round(tf$TE.random, 3)) )


metacum(z, pooled="random")                          #Cummulative meta analysis.

png("Training_Cummulative.png", width = 1900, height = 3800, res = 200, pointsize = 16)
forest(metacum(z, pooled="random"))                  #Forest plot for the cummulative meta-analysis
dev.off()


#-------- Equip the environment with libraries - installing the packages only when required.
if(!require(robumeta)) install.packages("robumeta"); require(robumeta)

# Small Sample Adjustments for Robust Variance Estimation With Meta-Regression
ModSm <- robu(  formula = z$TE ~ as.character(MAD$design),
                data = z,
                studynum = z$studlab,
                var.eff.size = 1 / z$w.random,
                small = TRUE)
print(ModSm) # Output results
levels(MAD$design)

       
# Meta regressions
z <- metacont(N/2, d, SD, N/2, M2, SD,
        data = MAD,
        sm = "SMD")
metareg(z, N)
bubble(metareg(z, N))

# Remove an outlier in N and repeat
z <- metacont(N/2, d, SD, N/2, M2, SD,
        data = MAD[which(MAD$N < 400), ],
        sm = "SMD")
metareg(z, N)
bubble(metareg(z, N))


