## Utilities ##

library("RNiftyReg")

generate.ymdhm <- function(t){
	st <- as.character(t1970+t)
	yyyy <- substr(st,1,4)
	mm <- substr(st,6,7)
	dd <- substr(st,9,10)
	HH <- "00"
	MM <- "00"
	if(nchar(st)>10){
		HH <- substr(st,12,13)
		MM <- substr(st,15,16)
	}
	ymdhm <- sprintf("%s%s%s%s%s",yyyy,mm,dd,HH,MM)	
	return(ymdhm)
}

generate.title <- function(t,name.var){
	st <- as.character(t1970+t)
	yyyy <- substr(st,1,4)
	mm <- substr(st,6,7)
	dd <- substr(st,9,10)
	HH <- "00"
	MM <- "00"
	if(nchar(st)>10){
		HH <- substr(st,12,13)
		MM <- substr(st,15,16)
	}
	title <- sprintf("%s %s-%s-%s %s:%s",name.var,yyyy,mm,dd,HH,MM)	
	return(title)
}

generate.colbar <- function(Ncol){
	col.Rbar <- c("darkblue","blue3","blue1","dodgerblue","deepskyblue","cyan")
	col.Rbar <- c(col.Rbar,"yellow","gold","orange","red1","red3","darkred")
	col.Rbar <- colorRampPalette(col.Rbar)
	col.Rbar <- col.Rbar(Ncol)
	return(col.Rbar)
}

generate.colbar.wb <- function(Ncol){
	col.Rbar <- c("white","grey","black")
	col.Rbar <- colorRampPalette(col.Rbar)
	col.Rbar <- col.Rbar(Ncol)
	return(col.Rbar)
}

generate.tabC <- function(Z,minZ,maxZ,Ncol,col.Rbar){
	tabK <- 1+round((Ncol-1)*(Z-minZ)/(maxZ-minZ))
    tabK[tabK>Ncol] <- Ncol
    tabK[tabK<1] <- 1
    tabC <- col.Rbar[tabK]
	return(tabC)
}

get.prev6h <- function(t){

	## Returns the closest 6h period before t

	t1970 <- as.POSIXct("1970-01-01",tz="GMT")
	st  <- as.character(t1970+t)
	ymd <- substr(st,1,10)
	te  <- as.numeric(as.POSIXct(ymd,tz="GMT"))+24*3600
	seqT <- seq(te-30*3600,te,6*3600)
	I <- which(seqT<=t)
	new.t <- seqT[I[length(I)]]
	return(new.t)
}

get.prev1h <- function(t){

	## Returns the closest 1h period before t

	t1970 <- as.POSIXct("1970-01-01 00:00:00",tz="GMT")
	st  <- as.character(t1970+t)
	ymd <- substr(st,1,10)
	te  <- as.numeric(as.POSIXct(ymd,tz="GMT")) + 24*3600
	seqT <- seq(from=te-26*3600,to=te,by=3600)
	I <- which(seqT<=t)
	new.t <- seqT[I[length(I)]]
	return(new.t)
}

get.nearest1h <- function(t){

	## Returns the nearest 1h period to time t

	t1970 <- as.POSIXct("1970-01-01 00:00:00",tz="GMT")
	st  <- as.character(t1970+t)
	ymd <- substr(st,1,10)
	te  <- as.numeric(as.POSIXct(ymd,tz="GMT")) + 24*3600
	seqT <- seq(from=te-26*3600,to=te,by=3600)
	imin <- which.min(abs(seqT-t))
	return(seqT[imin])
}

plot.leg <- function(leg,hleg,wleg,ylim,atZ,labZ,minZ,maxZ,mai.leg,plot.path,colbar,Ncol=200,dpi=300,cx1=0.9,cx2=0.9){

	yll   <- ylim[2]-0.045*diff(ylim) 
	atY   <- ylim[1] + (yll-ylim[1])*(atZ-minZ)/(maxZ-minZ)
	dyy   <- (yll-ylim[1])/Ncol	
	NlabZ <- length(labZ)
	
	leg.eps  <- sprintf("%s/leg.eps",plot.path)
	leg.png  <- sprintf("%s/leg.png",plot.path)
	postscript(leg.eps,height=hleg,width=wleg,horizontal=FALSE,onefile=FALSE,paper="special",bg="white")
	par(mai=mai.leg)
	plot(0,0,"n",bty="n",xaxt="n",yaxt="n",xlim=c(0,1),ylim=ylim,xlab="",ylab="")
	rect(rep(0,Ncol),seq(ylim[1],yll-dyy,dyy),rep(0.2,Ncol),seq(ylim[1]+dyy,yll,dyy),col=colbar[1:Ncol],border="NA")
	for(i in 1:NlabZ){lines(c(0.15,0.25),rep(atY[i],2),lwd=2)}
	text(rep(0.3,NlabZ),atY,labZ,adj=0,cex=cx1)	
	text(-0.05,ylim[2],leg,adj=0,cex=cx2)		
	dev.off()
	system(sprintf("convert -quiet -density %i %s %s",dpi,leg.eps,leg.png))
	system(sprintf("rm %s",leg.eps))
	return(leg.png)
}
	
	
confusion.table <- function(Y,hatY){

	TABLE <- matrix(NA,nrow=2,ncol=2)	# rows for predictions, columns for observations
	rownames(TABLE) <- c("predicted1","predicted0")
	colnames(TABLE) <- c("observed1","observed0")
	
	TP <- length(which(hatY==1 & Y==1))
	TN <- length(which(hatY==0 & Y==0))
	FP <- length(which(hatY==1 & Y==0))
	FN <- length(which(hatY==0 & Y==1))	
			
	TABLE[1,] <- c(TP,FP)
	TABLE[2,] <- c(FN,TN)
	
	return(TABLE)		
}

qmap <- function(Z1,Z2){

	## Performs quantile matching of Z1 w.r.t. Z2
	
	## Inputs:
	## Z1 = vector of values for first image
	## Z2 = vector of values for second image (target)
	
	## Output:
	## newZ = new vector of values for Z1
	
	n1   <- length(Z1)
	n2   <- length(Z2)
	idw1 <- which(Z1>0)		
	idw2 <- which(Z2>0)
	Nw1  <- length(idw1)
	Nw2  <- length(idw2)
	if(Nw1==0 || Nw2==0){return(Z1)}
	
	newZ <- rep(0,n1)
	S <- sort(Z2[idw2])
	ranks <- rank(Z1[idw1])		# from 1 to Nw1
	id <- round(Nw2*ranks/Nw1)	# indexes
	id[id<1] <- 1				# lowest index is 1
	newZ[idw1] <- S[id]
	return(newZ)		
}

best.affine.transform <- function(Z1,Z2,xdim,ydim,qmap=FALSE){

	## Function to determine the best affine transformation from Z.initial to Z.final

	## Inputs:
	## Z1 = vector of initial values 
	## Z2 = vector of final values
	## x|y dim = dimensions of grid
	## qmap: if TRUE, apply quantile mapping between Z1 and Z2 before calculating affine transform
	
	## Output:
	## nreg = output of niftyreg 
	
	## Remarks:
	## - Requires library RNiftyReg
	
	if(xdim<=0 || ydim<=0){stop("grid dimensions must be positive")}
	if(any(is.na(Z1))){stop("NA values in Z1")}
	if(any(is.na(Z2))){stop("NA values in Z2")}
	n1 <- length(Z1)
	n2 <- length(Z2)
	if(n1!=n2){stop("Z1 and Z2 have different lengths!")}
	
	if(qmap==TRUE){
		idw1 <- which(Z1>0)
		idw2 <- which(Z2>0)
		Nw1  <- length(idw1)
		Nw2  <- length(idw2)
		if(Nw1>0 && Nw2>0){
			newZ <- rep(0,n1)
			S <- sort(Z2[idw2])			# Nw2 values
			ranks <- rank(Z1[idw1])		# from 1 to Nw1
			id <- round(Nw2*ranks/Nw1)	# indexes
			id[id<1] <- 1				# lower index is 1
			newZ[idw1] <- S[id]
			Z1 <- newZ
		}
	}
	M1 <- matrix(Z1,nrow=xdim,ncol=ydim,byrow=TRUE)
	M2 <- matrix(Z2,nrow=xdim,ncol=ydim,byrow=TRUE)	
	nreg <- niftyreg(source=M1,target=M2,scope="affine")	
	return(nreg)
}

apply.affine.transform <- function(Z,xdim,ydim,nreg){

	## Function to apply affine transformation to Z

	## Inputs:
	## Z = vector of values
	## x/y dim = dimensions of grid
	## reg = output of best.affine.transform()
	
	## Output:
	## newZ = vector of values after transformation (over same grid)
	
	## Remarks:
	## - Requires library RNiftyReg
	
	M <- matrix(Z,nrow=xdim,ncol=ydim,byrow=TRUE)
	fwd  <- forward(nreg)
	newM <- applyTransform(fwd,M)
	newZ <- as.vector(t(newM))		
	return(newZ)
}

compute.FSS <- function(Z1,Z2){
	FSS <- NA
	FBS <- mean((1*(Z1>0)-1*(Z2>0))^2)
	FBS.ref <- mean((1*(Z1>0))^2 + (1*(Z2>0))^2)
	if(FBS.ref!=0){FSS <- 1-FBS/FBS.ref}
	return(FSS)
}

compute.CC <- function(Z1,Z2){
	CC  <- NA
	lu1 <- length(unique(Z1))
	lu2 <- length(unique(Z2))
	if(lu1>3 && lu2>3){
		CC <- cor(Z1,Z2)
	}
	return(CC)
}

Z.after.shift <- function(data,shift,xy.data,xy.query){
	xy.query[,1] <- xy.query[,1]-shift[1]
	xy.query[,2] <- xy.query[,2]-shift[2]
	id.query <- knnx.index(data=xy.data,query=xy.query,k=1,algorithm="kd_tree")
	Z <- data[id.query]				
	return(Z)
}

compute.shift <- function(Z1,Z2,xdim,ydim){
	mat1 <- matrix(Z1,nrow=xdim,ncol=ydim)
	mat2 <- matrix(Z2,nrow=xdim,ncol=ydim)
	xc <- xcorr3d(mat1,mat2)
	shift <- xc$max.shift						
	return(shift)
}
