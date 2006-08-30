"levene.test" <- 
function(y, group, option=c("mean", "median", "trim.mean"), trim.alpha=1) 
 { 
   option<-match.arg(option)

if ((option=="trim.mean")&(trim.alpha==1)) 
{stop("trim.alpha value of 0 to 0.5 should be provided for the trim.mean option")}

   DNAME = deparse(substitute(y))

   group <- as.factor(group) # precautionary 

### User can choose classical Levene's Test, i.e. abs. deviations from the mean, or 
### modified robust Levene's tests, i.e. abs. deviations  the median or 
### the trimmed mean 

  
if(option=="mean")
{
means <- tapply(y, group, mean) 
resp.mean <- abs(y - means[group])
statistic=anova(lm(resp.mean ~ group))[1, 4]
METHOD="Classical Levene's test based on the absolute deviations from the mean"
p.value=anova(lm(resp.mean ~ group))[1, 5]
}
  

else if(option=="median")
{
meds <- tapply(y, group, median) 
resp.med <- abs(y - meds[group]) 
statistic=anova(lm(resp.med ~ group))[1, 4]
METHOD="Modified Robust Brown-Forsythe Levene-type test based on the absolute deviations from the median"
p.value=anova(lm(resp.med ~ group))[1,5] 
}

else {option="trim.mean";
trimmed.mean<-function(y) mean(y, trim=trim.alpha)
trim.means <- tapply(y, group, trimmed.mean) 
resp.trim.mean <- abs(y - trim.means[group]) 
statistic=anova(lm(resp.trim.mean ~ group))[1, 4]
METHOD="Modified Robust Levene-type test based on the absolute deviations from the trimmed mean"
p.value=anova(lm(resp.trim.mean ~ group))[1, 5]
}
 
# Display Output #
STATISTIC=statistic
names(STATISTIC)="Test Statistic"

structure(list(statistic = STATISTIC, p.value=p.value, method = METHOD, data.name = DNAME), class = "htest")
}



