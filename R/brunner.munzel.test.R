"brunner.munzel.test" <-
function(x,y, alternative=c("two.sided", "greater", "less"))
{

alternative<-match.arg(alternative)

### Calculate the overall rank, and Ranks and Means of the first and second sample ###

n1=length(x)
n2=length(y)
r1=rank(x)
r2=rank(y)
r=rank(c(x,y))
m1=mean(r[1:n1])
m2=mean(r[n1+1:n2])

### Calculate the Empirical Variance of the first and second sample ###

v1=sum((r[1:n1]-r1-m1+(n1+1)/2)^2)/(n1-1)
v2=sum((r[n1+1:n2]-r2-m2+(n2+1)/2)^2)/(n2-1)

### Calculate the T statistic and degree of freedom of the whole data set ###
statistic=n1*n2*(m2-m1)/(n1+n2)/sqrt(n1*v1+n2*v2)
dfbm=((n1*v1+n2*v2)^2)/(((n1*v1)^2)/(n1-1)+((n2*v2)^2)/(n2-1))


### Users can choose alternative hypothesis ###
### Must be one of "two.sided"(default), "greater" or "less" ###
### Users can specify just the initial letter ###

if((alternative=="greater")|(alternative=="g")){p.value=1-pt(abs(statistic), dfbm)}

else if((alternative=="less")|(alternative=="l")){p.value=pt(abs(statistic), dfbm)}

else {
	alternative="two.sided";
	p.value=2*min( pt(abs(statistic),dfbm),(1-pt(abs(statistic),dfbm)) ) }


### Display Output ###

STATISTIC=statistic
names(STATISTIC) = "Brunner-Munzel Test Statistic"
PARAMETER = dfbm
names(PARAMETER) = "df"
METHOD = "Brunner-Munzel Test"
DNAME = paste(deparse(substitute(x)),"and", deparse(substitute(y)))


structure(list(statistic = STATISTIC, parameter = PARAMETER, p.value=p.value, method = METHOD, data.name = DNAME), class = "htest")

}
