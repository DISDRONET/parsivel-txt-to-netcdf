##################################################################
######### Script to Generate Daily Parsivel NetCDF Files #########
#################### by M. Schleiss, TU Delft ####################
################### Last update April 01, 2021 ###################
##################################################################

########################
## Required Libraries ##
########################

source("/home/mschleis/Codes/R/lib_Parsivel.R")			# custom R-library for Parsivel data processing

######################
## Global variables ##
######################

Parsivel_number <- 3					# Parsivel identifier
site_name       <- "Green_Village"		# site identifier
NETDL           <- 6					# NETDL identifier
dt.Parsivel     <- 60					# sampling resolution

input.path      <- sprintf("/media/marc/Data/Parsivel")
output.path     <- "/home/mschleis/Test"

day.start <- "2021-03-20"
day.end   <- "2021-03-31"

tb  <- as.POSIXct(day.start,tz=tz)
te  <- as.POSIXct(day.end,tz=tz)
ntb <- as.numeric(tb)
nte <- as.numeric(te)

seqT  <- seq(ntb,nte,24*3600)
days  <- unique(substr(as.character(t1970+seqT),1,10))
Ndays <- length(days)

if(Parsivel_number==3){
	latitude <- 51.99617
	longitude <- 4.37868
	altitude <- 3
}
if(Parsivel_number!=3){
	stop("error. Latitude and longitude for other Parsivels not known yet.")
}

## To DO: put information about latitude/longitude, site names and NETDL logger numbers into a global table in lib_Parsivel.R

######################
## MAIN STARTS HERE ##
######################

for(itr.day in 1:Ndays){

	day <- days[itr.day]

	print.noquote("")
	print.noquote(day)
	
	out <- create.daily.NetCDFfile.Parsivel(day,input.path,output.path,site_name,Parsivel_number,latitude,longitude,altitude,NETDL,dt.Parsivel)	
		
}
