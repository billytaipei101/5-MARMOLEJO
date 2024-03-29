#' While reading the paper quantile regression by Waldman (2018), he exemplified some key points with the use
#' of a data set of Body Mass Index of Dutch Boys and Malnutrition in India, the following script intends to replicate her findings
#' in order we can use a similar approach with our biomarker data
##########################################
# # Setting up the working environment # #
##########################################
x <- c('openxlsx','plyr','ggplot2','gam','gamlss.data','gamlss','gamboostLSS',
       'BayesX','mgcv','grid','ggthemes','scales') # List of packages here
packages <- as.data.frame(installed.packages())      # List of currently installed packages
for (i in 1:length(x)) {
  if(x[i] %in% packages$Package == TRUE){
    library(x[i],character.only = TRUE)  
  }else if(x[i] %in% packages$Package == FALSE){
    install.packages(as.character(x[i]))
    library(x[i],character.only = TRUE)  
  }
}

#####################################
# # Some visualization adornments # #
#####################################
theme_Publication <- function(base_size=14,base_family="serif") { # optional to serif/helvetica
  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           axis.title.y = element_text(angle = 90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(),
           axis.text.x = element_text(angle = 0), # rotate in case long strings
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size= unit(0.2, "cm"),
           #legend.margin = unit(0, "cm"), # optionally comment
           legend.title = element_blank(), #optional == legend.title = element_text(face="italic"), element_blank()
           plot.margin=unit(c(10,5,5,5),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))
}

scale_fill_Publication <- function(...){
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")),...)
}

scale_colour_Publication <- function(...){
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")),...)
}
##########################################
# # Normal curve over histogram of BMI # #
##########################################
mydata1 <- gamlss.data::dbbmi # the Dutch boys body mass data
mycurve011 <- function(dataset,varname,varlabel,divisor){ # ,breaks_1
  #' The explanation of the fields are as follow
  #'  chr   dataset   <- a data frame with at least one variable, the one to be analyzed 
  #'  chr   varname   <- is the name of the variable of interest as given by str(dataset) 
  #'  chr   varlabel  <- is the name of the variable of interest to be displayed as the x axis in the figure
  #'  num   divisor   <- is a number to divide the x axis on the figure, if missing default is 1
  if(missing(divisor)){
    divisor <- 1   # change this value in case you want a more broad or small column size
  }
  mini <- floor(min(eval(parse(text = paste0(dataset,'$',varname)))))
  maxi <- ceiling(max(eval(parse(text = paste0(dataset,'$',varname)))))
  (mycurvi <- ggplot(data=eval(parse(text = dataset)), aes(eval(parse(text = paste0(dataset,'$',varname))))) + 
      geom_histogram(aes(y =..density..),
                     breaks = seq(mini,maxi, by = divisor),
                     col="#386cb0",
                     fill="#fdb462",
                     alpha=.2)+
      #geom_density(col=2)+ density function, instead of normal curve as default
      stat_function(fun = dnorm, args = list(mean = mean(eval(parse(text = paste0(dataset,'$',varname)))),
                                             sd = sd(eval(parse(text = paste0(dataset,'$',varname))))))+
      labs(title=paste0("Normal curve over histogram of ",varlabel),x=varlabel)
  ) 
  mycurvi + theme_Publication()+scale_fill_Publication()
}
# Figure 1
dataset  <- 'mydata1'
varname  <- 'bmi'
varlabel <- 'BMI'
mycurve01(dataset,varname,varlabel)

#########################################
# # Scatterplot between two variables # #
#########################################
myscatter011 <- function(dataset,varname,varname2,varlabel,varlabel2,pointsz){
  #' The explanation of the fields are as follow
  #'  chr   dataset   <- a data frame with at least one variable, the one to be analyzed 
  #'  chr   varname   <- is the name of the 1rs variable of interest as given by str(dataset) 
  #'  chr   varname2  <- is the name of the 2nd variable of interest as given by str(dataset) 
  #'  chr   varlabel  <- is the name of the variable of interest to be displayed as the y axis in the figure
  #'  chr   varlabel2 <- is the name of the variable of interest to be displayed as the x axis in the figure
  #'  num   pointsz   <- is a number indicating the size of points, if missing default is 1
  maxi <- ceiling(max(eval(parse(text = paste0(dataset,'$',varname2)))))
  #q10 <- seq(0.05, 0.95, by = 0.05)
  q10 <- c(0.01,0.1,0.2,0.3,0.4,0.6,0.7,0.8,0.9,0.99)
  if(missing(pointsz)){
    pointsz <- 0.5   # change this value in case you want a larger or smaller point size
  }
  (myscat <- ggplot(data=eval(parse(text = dataset)), aes(x=eval(parse(text = varname2)),
                                                          y=eval(parse(text = varname)))) + 
      geom_point(alpha = pointsz) +
      geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) + # method = gam uses R-package mgcv (Wood, 2014)
      #' This is a non-linear regression model estimating the association of varname2 with the expected value of varname, it was 
      #' calculated using the R-package mgcv (Wood, 2014)
      geom_quantile(quantiles = q10, formula = y ~ splines::bs(x, 6)) + # formula=y ~ poly(x, 3) | formula = y ~ splines::bs(x, 3) | method = "rqss", lambda = 0.8
      stat_summary_bin(fun.y='median', bins=maxi,    # optionally it can represent the median by changing fun.y to "median" or the mean with "mean"
                       color='orange', size=pointsz*4, geom='point') + # the orange dots represent the median, change geom='line' if desired
      labs(title=paste0("Scatterplot of ",varlabel," and ",varlabel2),x=varlabel2,y=varlabel)
  )
  myscat + theme_Publication()+scale_fill_Publication()
  #' When modelling the quantiles conditional on the data (i.e. Q(Y|X)), we get the model as displayed in the figure. Whether the distribution is
  #' assimetrical it can be capture by this approach by seeing the distance between the median regression and the curve for \tau = 0.6 should be
  #' the same as the difference between the median regression and the curve for \tau = 0.4. Likewise, the distance between \tau = 0.4 and \tau = 0.3
  #' should be the same as the differences between \tau = 0.6 and \tau = 0.7, etc.
  #' Does this assumption holds for the quantile curves? if not...
  #' The difference which is most obvious can be seen in the distance between the first and second (i.e., the \tau = 0.01 and \tau = 0.1) quantile
  #' and the distance between the second to last and the last (i.e., the \tau = 0.9 and the \tau = 0.99) quantile. The later reflects the positive
  #' skewness of the data. Additionally, the increasing difference between the \tau = 0.01 quantile and the \tau = 0.99 quantile shows how the variance
  #' in the print(varname) increases with increasing print(varname2)
  #' Is the mean line above or below the median? if so... the conditional distribution might be skewed.
  #' 
}

# Figure 2
mydata1 <- gamlss.data::dbbmi # the Dutch boys body mass data
dataset   <- 'mydata1'
varname   <- 'bmi'
varname2  <- 'age' 
varlabel  <- 'BMI'
varlabel2 <- 'Age'
myscatter011(dataset,varname,varname2,varlabel,varlabel2)
#' In this figure we can see that the mean as represented by the blue line is slightly above the median as represented by the orange dots


#' To find the empirical distribution and the probability under the normally distributed assumption see the following
#' For example, we would like to know the value of a distribution in the 95% quantile (theorical probability), in other words
#' What value does the fitted Gaussian distribution yields for a BMI value of 30 P(BMI>30) 

the_vs_emp_prob<-function(dataset,varname,value_of_int){
  #' Explanation of the fields below
  #' chr   dataset     <- is the name of the dataset that contains the vector of values of interest
  #' chr   varname     <- is the name of the variable of interest as given by str(dataset)
  #' num  value_of_int <- is a value of interest that we want to find both theorical and empirical probability 
  
  options(scipen = 99)                  # turn off scientific notation 
  round_fact <- 8
  x <- eval(parse(text = paste0(dataset,'$',varname)))
  end <- max(x)
  #' theorical probability
  theor <- round(pnorm(value_of_int, mean = mean(x),sd = sd(x), lower.tail = FALSE),round_fact)
  #' empirical probability
  empir <- round(sum(end>=x & x>=value_of_int)/length(x),round_fact)
  cat(paste0('For example, the theoretical probability of a ',varname,' value equal to ',value_of_int,' is ',theor,' while the empirical probability is ',empir,
             '\n the absolute difference between these values is ',round(abs(theor-empir),round_fact),'; below there is a report of some quantiles of interest\n'))
  cat('\n')
  print(quantile(x))                     # reports min, quantile, median, quantile and max.
  cat('\n')
  print(quantile(x,probs=c(.025,.975)))  # report the value under 2.5% and 97.5
  cat('\n')
  print(quantile(x,probs=c(.05,.95)))  # report the value under 2.5% and 97.5
  cat('\n')
  #The 95% quantile of the fitted normal distribution is 22.809, the 95% quantile of the data is 23.612.
  x1 <- round(quantile(rnorm(length(x), mean=mean(x), sd=sd(x)),probs=c(.95)),round_fact)
  x2 <- round(quantile(x,probs=c(.95)),round_fact)
  cat(paste0('Thus, following a theorical distribution, the 95% quantile of the fitted normal distribution is ',x1,',while the\n',
             '95% quantile of the data is ',x2,'. The absolute difference between these values is ',round(abs(x1-x2),round_fact),'.'))
}
# Results on Analyzing Quantiles on a extreme value of interest
dataset <- 'mydata1'
varname <- 'bmi'
value_of_int <- 30
the_vs_emp_prob(dataset,varname,value_of_int) 

####################################
# # ADDITIVE QUANTILE REGRESSION # #
#################################### aQUI!
#' Data example II, Stunting in India
mydata2 <- gamboostLSS::india
# cbmi BMI of the child
# cage Age of the child in months
# mbmi BMI of the mother
# mage Age of the mother in years

# First trial, using child BMI to calculate z value
# child_sd   <- sd(mydata2$cbmi)*sqrt((length(mydata2$cbmi)-1)/(length(mydata2$cbmi))) 
# child_mean <- mean(mydata2$cbmi)
# mydata2$z <- c(NA)
# for (i in 1:nrow(mydata2)) {
#   mydata2$z[i] <- (mydata2$cbmi[i] - child_mean)/child_sd
#   
# }
# # Second trial, using child age to calculate z value
# child_sd   <- sd(mydata2$cage)*sqrt((length(mydata2$cage)-1)/(length(mydata2$cage)))
# child_mean <- mean(mydata2$cage)
# mydata2$z <- c(NA)
# for (i in 1:nrow(mydata2)) {
#   mydata2$z[i] <- (mydata2$cage[i] - child_mean)/child_sd
# }
# Example provided by paper on how to use gamboost
qr_boost <- gamboost(stunting ~ 
                       cage + cbmi + bmrf(mcdist, "markov", bnd =india.bnd, center = F), # not sure whether I am selecting the correct variables for the model
                     family = QuantReg(tau = 0.3), # quantile 0.3
                     data= india)

mydata2$z <- qr_boost$response
# Figure 3
dataset   <- 'mydata2'
varname   <- 'z'
varname2  <- 'cage' 
varlabel  <- 'z-score'
varlabel2 <- 'Age'
myscatter011(dataset,varname,varname2,varlabel,varlabel2) # graph similar but not the same as Figure 5 in paper???

qr_boostm <- gamboost(stunting ~ 
                       mage + mbmi + bmrf(mcdist, "markov", bnd =india.bnd, center = F), # not sure whether I am selecting the correct variables for the model
                     family = QuantReg(tau = 0.3), # quantile 0.3
                     data= india)


mydata2$z_m <- qr_boostm$response # information from the mother
# Figure 4
dataset   <- 'mydata2'
varname   <- 'z_m'
varname2  <- 'mbmi' 
varlabel  <- 'z-score'
varlabel2 <- 'BMI of the mother'
myscatter011(dataset,varname,varname2,varlabel,varlabel2) # graph similar but not the same as Figure 7 in paper???

#' The section on implementation ond related models is quite dense

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
#' Writing in progress any update will be posted here
#'
#' https://www.gamlss.com 
#' 
#' 
#' ####################
#' STASINOPOULOS 2018 #
#' ####################
#' SECTION 2
#' General form of the GAMLSS model for any response variable distribution.
#' SECTION 3
#' Demonstrates GAMLSS analysis of a continuous response variable.

mydata3 <- gamlss.data::dbhh

#' SECTION 3.2 models height agains a single explanatory variable age in Dutch boys, it focus on obtainig centiles for the response variable.
#' 
#' Distributions
#' NO - normal
mNO  <- gamlss(head ~ 1, data = lice, family = NO, weights = freq, trace = FALSE)
#' BCCGo
BCCG()
#' BCPEo
BCPE()
#' BCTo
BCT()

#' SECTION 3.3 models head circumference against two explanatory variables height and age it focus on obtainig centiles for the response variable.
#' SECTION 4 
#' Demonstrates GAMLSS analsysis of a discrete response variable using different discrete distributions. 

s



