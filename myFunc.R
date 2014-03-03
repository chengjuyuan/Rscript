 
create.legend <- function(myData){
  m  <- mean(myData)
  sd <- sd(myData)
  lowSD <- m-sd
  highSD <- m+sd
  Fn1 <- ecdf(as.vector(myData)) # make CDF
  #perTile <-Fn1(last)
  low <- quantile(myData,.05,na.rm=TRUE)
  high <- quantile(myData,.95,na.rm=TRUE)
  med <- quantile(myData,.5,na.rm=TRUE)
  len <- length(myData)

  legTxt <-list()
  legTxt[1] <- paste( sprintf("%8s %7.2f", "-1sd:",lowSD) )
  legTxt[2] <- paste( sprintf("%8s %7.2f",  "avg:",m) )
  legTxt[3] <- paste( sprintf("%8s %7.2f", "+1sd:",highSD) )
  legTxt[4] <- paste( sprintf("%8s %7.2f", "5%-ile:",low) )
  legTxt[5] <- paste( sprintf("%8s %7.2f",  "median:",med) )
  legTxt[6] <- paste( sprintf("%8s %7.2f", "95%-ile:",high) )
  legTxt[7] <- paste( sprintf("%8s %7d", "num ticks:",len) )
  return(legTxt)
}

create.legend2 <- function(myData,cut){
  myData1 <- subset(myData,myData>0)
  posPer  <- length(myData1)/length(myData) *100
  posAvg  <- mean(myData1)
  posMed  <- median(myData1)

  myData2 <- subset(myData,myData<0)
  negPer  <- length(myData2)/length(myData) *100
  negAvg  <- mean(myData2)
  negMed  <- median(myData2)

  legTxt <-list()
  legTxt[1] <- paste( sprintf("%8s %7.2f%1s", "%(+):",posPer,"%") )
  legTxt[2] <- paste( sprintf("%8s +%7.2f",  "avg(+):",posAvg) )
  legTxt[3] <- paste( sprintf("%8s +%7.2f", "med(+):",posMed) )
  legTxt[4] <- paste( sprintf("%8s", "") )
  legTxt[5] <- paste( sprintf("%8s %7.2f%1s",  "%(-):",negPer,"%") )
  legTxt[6] <- paste( sprintf("%8s %7.2f", "avg(-):",negAvg) )
  legTxt[7] <- paste( sprintf("%8s %7.2f", "med(-):",negMed) )
  return(legTxt)
}


create.png <- function(isBatch,pgname,numheig,numwid,...){
   if ( isBatch == T ) {
      Cairo(file=pgname,type='png', width=800*numwid, height=600*numheig, res=100, bg='white',cex.main=3,...)
   } else {
      Cairo(width = 20, height = 20)
   }
}

plot.time.lpt <- function(data, title, ...) {
   #title <- paste( e, " :" , "CC Vol ratio")
   #volRat  <- axExp2$volCC / axExp1$volCC
   plot(data, type="l", pch=20, main = title,
	, major.tick="months", minor.ticks=FALSE, major.format="%Y\n%b", cex.main=3, ...)
   if(length(data)>0){
	lastP = tail(data,1)
   } else {
	lastP = 0
   } 
   points( lastP, col="red", pch=11, cex = 3)
   #text(time(tail(data,1)),tail(data,1), round(tail(data,1), 2), cex=1 , col="red")
   axis(4,at=lastP,labels=round(lastP,2), col.axis="red", las=2)
  
}

plot.time.avg <- function(data, title, ...) {
   #title <- paste( e, " :" , "CC Vol ratio")
   #volRat  <- axExp2$volCC / axExp1$volCC
   plot(data, type="l", pch=20, main = title,
	, major.tick="months", minor.ticks=FALSE, major.format="%Y\n%b",cex.main=3, ...)
   if(length(data)>0){
   avg<-mean(data,na.rm = TRUE, lty="dashed")
   abline(h=avg, col="blue")
   max<-max(data,na.rm = TRUE)
   abline(h=max, col="red", lty="dashed")
   min<-min(data,na.rm = TRUE)
   abline(h=min, col="red", lty="dashed")
   low <- quantile(data,.05,na.rm=TRUE)
   abline(h=low, col="green", lty="dashed")
   high <- quantile(data,.95,na.rm=TRUE)
   abline(h=high, col="green", lty="dashed")
axis(4,at=avg,labels=round(avg,2), col.axis="blue", las=2)
axis(4,at=max,labels=round(max,2), col.axis="red", las=2)
axis(4,at=min,labels=round(min,2), col.axis="red", las=2)
axis(4,at=low,labels=round(low,2), col.axis="green", las=2)
axis(4,at=high,labels=round(high,2), col.axis="green", las=2)
   }
}

plot.time.lpt.avg <- function(data, title, ...) {
   #title <- paste( e, " :" , "CC Vol ratio")
   #volRat  <- axExp2$volCC / axExp1$volCC
   plot(data, type="l", pch=20, main = title,
	, major.tick="months", minor.ticks=FALSE, major.format="%Y\n%b", cex.main=3,...)
   if(length(data)>0){
	lastP = tail(data,1)
   points(lastP , col="red", pch=11, cex = 3)
   axis(4,at=lastP ,labels=round(lastP ,2), col.axis="red", las=2)
   avg<-mean(data,na.rm = TRUE)
   abline(h=avg, col="blue")
   max<-max(data,na.rm = TRUE)
   abline(h=max, col="red", lty="dashed")
   min<-min(data,na.rm = TRUE)
   abline(h=min, col="red", lty="dashed")
   low <- quantile(data,.05,na.rm=TRUE)
   abline(h=low, col="green", lty="dashed")
   high <- quantile(data,.95,na.rm=TRUE)
   abline(h=high, col="green", lty="dashed")
axis(4,at=avg,labels=round(avg,2), col.axis="blue", las=2)
axis(4,at=max,labels=round(max,2), col.axis="red", las=2)
axis(4,at=min,labels=round(min,2), col.axis="red", las=2)
axis(4,at=low,labels=round(low,2), col.axis="green", las=2)
axis(4,at=high,labels=round(high,2), col.axis="green", las=2)
   } else {
	lastP = 0
   }
 
}

plot.lpt.avg <- function(data, title, ...) {
   #title <- paste( e, " :" , "CC Vol ratio")
   #volRat  <- axExp2$volCC / axExp1$volCC
   plot(data,  pch=20, main = title, cex.main=3, ...)
   if(length(data)>0){
	lastP = tail(data,1)
   points(lastP , col="red", pch=11, cex = 3)
 #  text(time(tail(data,1)),tail(data,1), round(tail(data,1), 2), cex=1 , col="red")
   axis(4,at=lastP ,labels=round(lastP ,2), col.axis="red", las=2)
   avg<-mean(data,na.rm = TRUE)
   abline(h=avg, col="blue")
   max<-max(data,na.rm = TRUE)
   abline(h=max, col="red", lty="dashed")
   min<-min(data,na.rm = TRUE)
   abline(h=min, col="red", lty="dashed")
   low <- quantile(data,.05,na.rm=TRUE)
   abline(h=low, col="green", lty="dashed")
   high <- quantile(data,.95,na.rm=TRUE)
   abline(h=high, col="green", lty="dashed")
axis(4,at=avg,labels=round(avg,2), col.axis="blue", las=2)
axis(4,at=max,labels=round(max,2), col.axis="red", las=2)
axis(4,at=min,labels=round(min,2), col.axis="red", las=2)
axis(4,at=low,labels=round(low,2), col.axis="green", las=2)
axis(4,at=high,labels=round(high,2), col.axis="green", las=2)
   } else {
	lastP = 0
   }

}


getValue <- function(inFile){
   a <- as.list(paste(as.list(t(read.table( inFile )))))
   return(a)
}


getVolData <- function(baseFile, version="current", pubDir="/data/public", region="HK" ) {
   #region<-"HK" 
   #pubDir <- "/data/public"
   #version <- "current"
   inFile <- paste(pubDir, "/Quant/system/", version,"/data/",region,"/vol/", baseFile,  sep="")

   a <- read.table( inFile , header=T, comment.char = "", row.names = NULL)
   return(a)
}


getCurveData <- function(inFile ) {
   #region<-"HK" 
   #pubDir <- "/data/public"
   #version <- "current"
   a <- read.table( inFile , header=T, comment.char = "", row.names = NULL)
   return(a)
}


getPairsData_Time <- function(a, name0, name1) {
 a0  <- subset(a,name==name0)
 ax0 <- xts(a0[,-c(1,2)], as.POSIXct(strptime(paste(a0$date,"01:00:00"),"%Y-%m-%d %s"))) 

 a1  <- subset(a,name==name1)
 ax1 <- xts(a1[,-c(1,2)], as.POSIXct(strptime(paste(a1$date,"01:00:00"),"%Y-%m-%d %s"))) 

 mym<-merge(ax0,ax1,join="inner")
 return(mym)
}


getPairsData<- function(a0,a1, name0, name1) {

 a00  <- subset(a0,name==name0)
 ax00 <- xts(a00[,-c(1,2)], as.POSIXct(strptime(paste(a00$date,"01:00:00"),"%Y-%m-%d %s"))) 

 a11  <- subset(a1,name==name1)
 ax11 <- xts(a11[,-c(1,2)], as.POSIXct(strptime(paste(a11$date,"01:00:00"),"%Y-%m-%d %s"))) 

 mym<-merge(ax00,ax11,join="inner")
 return(mym)
}

calWingVol <- function(pc, cc,  sc, vc,  usm, uc, dsm,  dc,  x) {
  # x = Math.log(1 + x);
  result <- -1;
  if (x <= 0 && x >= dc) {
    result = (vc + sc * x + pc * x * x)
  }
  else if (x < dc && x >= (1 + dsm) * dc) {
    result <- (vc - (1 + 1 / dsm) * pc * dc * dc - sc * dc / (2 * dsm) + (1 + 1 / dsm) * (2 * pc * dc + sc) * x - (pc / dsm + sc/ (2 * dc * dsm))* x * x)
  }
  else if (x < dc * (1 + dsm)) {
    result <- (vc + dc * (2 + dsm) * sc / 2 + (1 + dsm) * pc * dc * dc)
  }
  else if (x > 0 && x < uc) {
    result <- (vc + sc * x  + cc * x * x)
  }
  else if (x >= uc && x <= uc * (1 + usm)) {
    result <- (vc - (1 + 1 / usm) * cc * uc * uc - sc * uc / (2 * usm) + (1 + 1 / usm) * (2 * cc * uc + sc) * x - (cc / usm + sc/ (2 * uc * usm))* x * x)
  }
  else if (x > uc * (1 + usm)) {
    result <- (vc + uc * (2 + usm) * sc / 2 + (1 + usm) * cc * uc * uc)
  }
  return(result)
}




plotDensity <- function(myData, title, hxLim) {
  # get stats
  m  <- mean(myData)
  sd <- sd(myData)
  lowSD <- m-sd
  highSD <- m+sd
  last <- tail(myData,1)
  lastSD <- (last-m)/sd

  Fn1 <- ecdf(as.vector(myData)) # make CDF
  perTile <-Fn1(last)
  
  lowBd <- min(m-2*sd,hxLim[1])
  hiBd <- max(m+2*sd,hxLim[2])


  # plot density
  d <- density(myData)
  plot(d, xlab="ratio", main=title, xlim=c(lowBd,hiBd) ,cex.main=3)
  abline(v=m, col="green",lty=3)
  abline(v=lowSD, col="blue",lty=3)
  abline(v=highSD, col="blue",lty=3)
  abline(v=last, col="red",lty=1,lwd=3)

  # setup legend
  legTxt <-list()
  legTxt[1] <- paste( sprintf("%5s %.2f", "-1sd:",lowSD) )
  legTxt[2] <- paste( sprintf("%5s %.2f",  "avg:",m) )
  legTxt[3] <- paste( sprintf("%5s %.2f", "+1sd:",highSD) )
  legTxt[4] <- paste( sprintf("%5s %.2f", "last",last) )
  #legTxt[1] <- paste("-1sd:", sprintf("%.2f",lowSD) )
  #legTxt[2] <- paste(" avg:" , format(m,digits=3) )
  #legTxt[3] <- paste("+1sd" , format(highSD,digit=3) )
  #legTxt[4] <- paste("last:", format(last,digit=3))
  legend("topleft", legend=legTxt, cex=3)
  legTxt <-list()
  legTxt[1] <- paste(sprintf("%5s %4.1f", "%-ile",perTile*100) )
  legTxt[2] <- paste(sprintf("%5s %.2f", "SD"   ,lastSD) )
  legend("topright", legend=legTxt, cex=3)
}



varSwapSlopeApprox <- function(atmVol, slope, ttx) { 
   # ttx is in years
   # > varSwapSlopeApprox(.2,-0.19,2)
   # [1] 0.2280193
   return( sqrt( atmVol^2  -  slope*atmVol^3*ttx  + slope^2/4*(12*atmVol^2*ttx + 5*atmVol^4*ttx^2) ) )

}

calcForwardVol <- function(volNear, timeNear, volFar, timeFar, timeForw) { 
   volsqr = timeFar*volFar^2 - timeNear*volNear^2
   if(volsqr>=0){
      return(sqrt(volsqr/timeForw))
   }else{
      return("nan")
   }
}



getDelta <- function(F,K,ttx,atmVol,r=0,q=0,...){
	d1 = (log(F/K)+(r-q+atmVol^2/2)*ttx)/(atmVol*sqrt(ttx))
	return( pnorm(d1,mean=0,sd=1))
}


getStrike <- function(F,x,ttx,atmVol,r=0,q=0,...){
	d1 <- qnorm(x)
	#print(d1)
	return( F*exp(-(r-q+atmVol^2/2)*ttx-d1*atm*sqrt(ttx)))
}


dateToInt <-function(x){
	dateToIntTem <- paste(substr(x,0,4),substr(x,6,7),substr(x,9,10),sep="")
	return(dateToIntTem)
}


corrAvg<-function(x,w,v){
	corrvol<-x*x*w
	corrAvg<-(x*w)%*%(x*w)
	corr<-(corrvol-v*v)/(sum(corrAvg)-sum(diag(corrAvg)))
	return(corr)
}


minlength<-function(x,y){
	minlen<- min(length(na.omit(x)),length(na.omit(y)))
	return(minlen)
}

parseTime <-function(x){
	len<-nchar(x)
	if(len==8){ x <- paste("0",x,sep="")}
	parsetime <- sprintf("%02s:%02s:%02s",substr(x,1,2),substr(x,3,4),substr(x,5,6))
	return(parsetime)
}	



push <- function(l, x) {
	assign(l, append(eval(as.name(l)), x), envir=parent.frame())
}

math <-function(x){
	as.numeric(as.matrix(x))
}

strMatrix<-function(mat){
	strMat <- ""
	for (kk in 1:ncol(mat)){
		if (kk>1){
			strMat <- paste(strMat,",",sep="")
		}
		strMat <- paste(strMat,colnames(mat)[kk],sep="")
	}
	strMat <- paste(strMat,"\n")

	for (kk in 1:nrow(mat)){
		strMat <- paste(strMat,rownames(mat)[kk],sep="")
		for (jj in 1:ncol(mat)){
			strMat <- paste(strMat,",",mat[kk,jj],sep="")			
		}
		strMat <- paste(strMat,"\n")
	}
	return(strMat)
}
