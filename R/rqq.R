"rqq" <-
function(y, plot.it=TRUE, square.it=TRUE, scale=c("MAD", "J", "classical"), 
             location=c("median", "mean"), line.it=FALSE, 
        line.type=c("45 degrees", "QQ"), col.line=1, lwd=1, ...)
{

##Strip NAs
y<-na.omit(y)


x=sort(y)

scale<-match.arg(scale)
location<-match.arg(location)
line.type<-match.arg(line.type)


### User select classical standard deviation, mean or median ###
if(location=="mean"){M=mean(x)}
else {M=median(x)}


### User can select Classical standard deviation, RQQ standardized by J, or RQQ standardized by MAD ###
if(scale=="classical"){qqstd = "QQ plot standardized by the classical std dev and";
                       y=(x-M)/sd(x)}

else if(scale=="MAD"){qqstd = "RQQ plot standardized by MAD and";
                       y=(x-M)/mad(x)}

else {scale="J";
	qqstd = "RQQ plot standardized by J and"; 
	j=sqrt(pi/2)*mean(abs(x-median(x)));
	y=(x-M)/j}

if((line.it=="TRUE")&(line.type=="QQ")){qql = ", QQ line"}
else if((line.it=="TRUE")&(line.type=="45 degrees")){qql = ", 45 degrees line"}


# User can select to plot QQ Line or 45 degrees Line #
# User can also select Square Plot or not#

if(line.it==TRUE)
{
  if(square.it==TRUE)
    {
    q<-qqnorm(y, xlim=c(min(y), max(y)), ylim=c(min(y), max(y)), main = paste(qqstd, location, qql), ...)
    }
    else {q<-qqnorm(y, main = paste(qqstd, location, qql), ...)}  

  if(line.type=="QQ")
    {
    qqline(y, datax = FALSE, col=col.line, lwd=lwd) 
    }
    else {abline(0, 1, col=col.line, lwd=lwd)}
}

else
{
  if(square.it==TRUE)
    {
    q<-qqnorm(y, xlim=c(min(y), max(y)), ylim=c(min(y), max(y)), main = paste(qqstd, location),...)
    }
    else {q<-qqnorm(y, main = paste(qqstd, location),...)}
}

}
