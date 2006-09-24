"symmetry.test" <-
function(x, option=c("mgg.test", "cabilio.masaro.test", "mira.test"))
{

option<-match.arg(option)
DNAME = deparse(substitute(x))

##Strip NAs
x<-na.omit(x)


x<-sort(x)
x<-(x-mean(x))/sd(x)
N=length(x)
m<-median(x)
g<-((mean(x)-2/N*(sum(x[which(x<=m)]))))


## tau2 for Normal Distribution ##
tau2<-0.5707963

J<-sqrt(pi/2)*mean(abs(x-m)) 
D<-N^(1/5)*(x[N/2+0.5*N^{4/5}]-x[N/2-0.5*N^{4/5}+1]) 
S<-4*var(x)+D^2-4*D*g


### User can choose Mira Test, Cabilio and Masaro Test, or Miao, Gel and Gaswirth Test ###

if(option=="mira.test"){
statistic=sqrt(N)*2*abs((mean(x)-m))/sqrt(S);
METHOD = "Test of Symmetry - Mira Test"}

else if(option=="cabilio.masaro.test"){
statistic=sqrt(N)*abs((mean(x)-m))/(sd(x)*sqrt(tau2));
METHOD = "Test of Symmetry - Cabilio-Masaro Test"}

else {option="mgg.test";
statistic=sqrt(N)*abs((mean(x)-m))/(J*sqrt(tau2));
METHOD = "Test of Symmetry - MGG Test"}


# Display Output #
p.value=2*(1-pnorm(statistic))

STATISTIC=statistic
names(STATISTIC)="Test Statistic"

structure(list(statistic = STATISTIC, p.value=p.value, method = METHOD, data.name = DNAME), class = "htest")
}