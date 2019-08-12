#' While reading the paper quantile regression by Waldman (2018), he exemplified some key points with the use
#' of a data set of Body Mass Index of Dutch Boys and Malnutrition in India, the following script intends to replicate her findings
#' in order we can use a similar approach with out biomarker data
x <- c('openxlsx','plyr','ggplot2','gam','gamlss.data','gamlss','gamboostLSS','BayesX','mgcv') # List of packages here
packages <- as.data.frame(installed.packages())      # List of currently installed packages
for (i in 1:length(x)) {
  if(x[i] %in% packages$Package == TRUE){
    library(x[i],character.only = TRUE)  
  }else if(x[i] %in% packages$Package == FALSE){
    install.packages(as.character(x[i]))
    library(x[i],character.only = TRUE)  
  }
}

######################################
# # Working with BMI of Dutch Boys # #
######################################
mydata <- gamlss.data::dbbmi # the included variables are age and Body Mass Index

h0 <- hist(mydata$bmi, density = 20, breaks=40, freq = FALSE, #density=10
           xlab="BMI", ylim=c(0, 0.20), # 
           main="Normal curve over histogram of BMI")
so <- curve(dnorm(x, mean=mean(mydata$bmi), sd=sd(mydata$bmi)), 
      col="darkblue", lwd=2, add=TRUE, yaxt="n")

(h1 <- ggplot(mydata, aes(x=age,y=bmi)) +
    geom_point(alpha = 0.4) +
    stat_summary_bin(fun.y='mean', bins=20,
                     color='orange', size=2, geom='point'))

#' Finding the empirical distribution and the probability under the gaussian fit assumption
#' The fitted Gaussian distribution yields P(BMI>30) = 1.91 *10^-5
pnorm(30, mean = mean(mydata$bmi), sd = sd(mydata$bmi), lower.tail = FALSE) # 0.00001909675
#' while the empirical probability
emp_prob<-function(x,start,end){
  sum(end>=x & x>=start)/length(x)}
x <- mydata$bmi
start <- 30
end <- max(x)
emp_prob(x,start,end) # 0.002056485

# which value dos y have to have such that holds? P(BMI < y) = 95%
quantile(mydata$bmi)                     # first try, reports min, quantile, median, quantile and max.
quantile(mydata$bmi,probs=c(.025,.975))  # second try, report the value under 2.5% and 97.5
#The 95% quantile of the fitted normal distribution is 22.809, the 95% quantile of the data is 23.612.
quantile(rnorm(length(mydata), mean=mean(mydata$bmi), sd=sd(mydata$bmi)),probs=c(.95)) # P(BMI < y) = 95% fitted normal / still incorrect
quantile(mydata$bmi,probs=c(.95))        # P(BMI < y) = 95% data

# The quantile conditioned on age?
#' P-splines are a special way of modelling non-linear effects. For a detailed explanation see Fahrmeir et al. 2013
#' 
#' Modelling the BMI of Dutch boys
#' mean regression
#' A second visualization of the gact that the conditional distribution is skewed is the following Figure, in which we can see that the mean
#' is slightly above the median
(h2 <- ggplot(mydata, aes(x=age,y=bmi)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se=TRUE) +
    stat_summary_bin(fun.y='median', bins=20,
                     color='orange', size=1, geom='line')
)

#' Additive quantile regression
#' Data example II, Stunting in India
mydata2 <- gamboostLSS::india
# cbmi BMI of the child
# cage Age of the child in months
# mbmi BMI of the mother
# mage Age of the mother in years

# First trial, using child BMI to calculate z value
child_sd   <- sd(mydata2$cbmi)*sqrt((length(mydata2$cbmi)-1)/(length(mydata2$cbmi))) 
child_mean <- mean(mydata2$cbmi)
mydata2$z <- c(NA)
for (i in 1:nrow(mydata2)) {
  mydata2$z[i] <- (mydata2$cbmi[i] - child_mean)/child_sd
  
}
# Second trial, using child age to calculate z value
child_sd   <- sd(mydata2$cage)*sqrt((length(mydata2$cage)-1)/(length(mydata2$cage))) 
child_mean <- mean(mydata2$cage)
mydata2$z <- c(NA)
for (i in 1:nrow(mydata2)) {
  mydata2$z[i] <- (mydata2$cage[i] - child_mean)/child_sd
  
}

qr_boost <- gamboost(stunting ~ 
                       cage + cbmi + bmrf(mcdist, "markov", bnd =india.bnd, center = F), # not sure whether I am selecting the correct variables for the model
                     family = QuantReg(tau = 0.3), # quantile 0.3
                     data= india)

(h3 <- ggplot(mydata2, aes(y=z,x=cage)) + # This is the only way it looks like the figure in the paper, nonetheless I have to invert the z-score scale and dont know how to use the model coefficients or scores for the calculated 0.3 quantile
    geom_point(alpha = 0.3) +
    geom_smooth(method = "gam",formula = y ~ s(x, bs = "cs") , se=TRUE)+
    ylim(6,-6)
)
#' Particularly dont know how to perform instructions in page 10: In order to be able to plot the non-linear effects into one graph, each
#' quantile is shifted to the level of the predicted values for this quantile....

#########################
# # ADDITION TO PAPER # #
#########################
#' Although some approaches to compare data's means and standard deviations (e.g. Frank & Klar, 2016), as well as skewness and kurtosis (e.g. Cain et al 2017)
#' have been put forward and these conform the main stream in the literature, quantile based and GAMLSS models are in our view, the most optimal ways to get a depper 
#' insight on the data's distribution and particularly truth when the interest lays in targeting the subjects with extreme values within a data set either for identification and/or intervention purposes.
#' For example, we might have a data set whose fitted Gaussian distribution is not completely identical to its empirical distribution, especially in either end of the distribution
#' as perceived by the percentage below the 5% quantile or above the 95% quantile. In this analysis theoretical and empirical results might differ which indicates
#' that the data deviates from a Gaussian distribution (see Figure XX we can create a Figure with your data). 
#' We can then consider the data on ????????? and have as our variable of interest the _________ with a special interest on those whose _____ >= | =< X. In 
#' Figure A the fitted Gaussian distribution yields P (variable >= | =< X) = ???, while the empirical probability (i.e. the proportion of subjects with 
#' variable >= | =< X) is, which differs in magnitude. The above shows that the data deviates from a Gaussian distribution in some of the areas under the curve. If instead
#' we ask the question of which 'Variable of interest' values should be considered to be low/high? rephrasing, where on the scare are the 5% of subjects
#' with the lowest/highest 'Variable of interest'? such that P(variable >= | =< X) = 95%/5%. The 95%/5% quantile of the fitted normal distribution is___, instead
#' the 95%/5% quantile of the data is _______; both results indicate the threshold for the lowest/highest 5% of subjects in 'Variable of interest'. In this case
#' theorical and empirical results differ a lot, thus the distribution cannot be considered Gaussian.
#' Quantile regression allows evidencing the association between explanatory variables with the 
#' conditional quantile of other dependent variable independently of the shape of its conditional distribution or the compliance with statistical assumptions such as homoscedasticity,
#' by modeling the quantiles in a similar fashion as it is done in traditional regression approaches. As shown above, we can analyze the 'variable of interest' by itself, but we might
#' be neglecting the fact that its conditional distribution varies with age/other_Variable, as can be seen in Figure n1. The question asked then shold not be
#' restricted to the quantile level rather to the quantile conditioned on age/other_Variable...
#' 
#' 
#' 




