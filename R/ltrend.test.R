"ltrend.test" <- 
function(y, group, option=c("mean", "median", "trim.mean"), trim.alpha=1) 
 { 

   option<-match.arg(option)

if ((option=="trim.mean")&(trim.alpha==1)) 
{stop("trim.alpha value of 0 to 0.5 should be provided for the trim.mean option")}

   DNAME = deparse(substitute(y))


   group <- as.factor(group) # precautionary 


### User can choose the trend test for variances based on classical Levene's Test, 
### the trend test based on the robust Brown-Forsythe Levene-type test
### or the Levene-type test with the group trimmed means  


if(option=="mean")
{

############## trend test with mean (Levene's version) ######################## 
   means <- tapply(y, group, mean) 
   resp.mean <- abs(y - means[group])
   
   mu<-mean(resp.mean)
   
   n<-tapply(y, group, length)
   dbar<-sum(as.numeric(factor(levels(group)))*n)/sum(n)
   z<-as.vector(resp.mean-mu)
   d<-as.numeric(group)-dbar
 
   statistic=summary(lm(z~d))$coefficients[2,1]
   METHOD="ltrend test based on classical Levene's procedure using the group means"
   p.value=summary(lm(z~d))$coefficients[2,4]
 
}



else if(option=="median")
{

############### trend test with median (BL version)###################
   means <- tapply(y, group, median) 
   resp.mean <- abs(y - means[group])
   
   mu<-mean(resp.mean)
   
   n<-tapply(y, group, length)
   dbar<-sum(as.numeric(factor(levels(group)))*n)/sum(n)
   z<-as.vector(resp.mean-mu)
   d<-as.numeric(group)-dbar
 
   statistic=summary(lm(z~d))$coefficients[2,1]
   METHOD="ltrend test based on the modified Brown-Forsythe Levene-type procedure using the group medians"
   p.value=summary(lm(z~d))$coefficients[2,4]

}


else {option="trim.mean";

############## trend test with trimmed mean (Joseph's version)##############
   trimmed.mean<-function(y) mean(y, trim=trim.alpha)
   means <- tapply(y, group, trimmed.mean) 
   resp.mean <- abs(y - means[group])
   resp.mean <- abs(y - means[group])
   
   mu<-mean(resp.mean)
   
   n<-tapply(y, group, length)
   dbar<-sum(as.numeric(factor(levels(group)))*n)/sum(n)
   z<-as.vector(resp.mean-mu)
   d<-as.numeric(group)-dbar

   statistic=summary(lm(z~d))$coefficients[2,1]
   METHOD="ltrend test based on the modified Levene-type procedure using the group trimmed means"
   p.value=summary(lm(z~d))$coefficients[2,4]

} 
  
# Display Output #
STATISTIC=statistic
names(STATISTIC)="Test Statistic"

structure(list(statistic = STATISTIC, p.value=p.value, method = METHOD, data.name = DNAME), class = "htest")
   } 
