# Create a vector with names of studies
Study   <- c("Korsgaard et al., 1995", "Castro, 2015", 
             "Johnson, 1971", "Lloyd et al., 2015") 

# Embelish the vector with additional information, for demonstration only
Study   <- paste0("Study ", 1:4, ": ", Study)

# Create a vector with correlations
r       <- c(.43, .57, .19, .47)

# Create a vector with sample sizes
N       <- c(89, 60, 128, 100)        

# load the package for use, each time you start R, or after detaching it. 
library(meta)   
# Run function metacor from "meta" to meta analyze correlations
metacor(r, N, Study)  

# Run function metacor from "meta" to meta 
# analyze correlations and insert output 
# into an object named "fit" 
fit <- metacor(r, N, Study)

# Explore the class(es) of fit
class(fit)

# Explore what methods could be applied to an object from class "meta"  
methods(class= "meta")  
methods(class= "metacor")  

# Apply the various "meta" methods
as.data.frame(fit)                    
forest(fit)
funnel(fit)
metabias(fit)
print(fit)
summary(fit)
trimfill(fit)
update(fit)

################################################################################
# Power Analysis examples 
################################################################################

# For introduction and examples see http://www.statmethods.net/stats/power.html
# install.packages("yaml") 
if (!require('pwr')) {
  install.packages('pwr')
}
library('pwr')

pwr.t.test(d = 1, 
           sig.level = .05, 
           power = .80, 
           type = c("two.sample"), 
           alternative = c("greater"))

pwr.r.test(r = .2, 
           power = .80, 
           sig.level = .05, 
           alternative = "two.sided")

# Practice.

# 1. Run your own meta-analysis.

# 2. Use your meta-analysis result to calculate required sample size to 
# replicate the meta-analysis.

