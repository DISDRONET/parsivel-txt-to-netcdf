##################################################################################
################# Custom R-Library for OTT Parsivel2 Disdrometer ################# 
############################ by M. Schleiss, TU Delft ############################
##################################################################################

## Last update on: March 4, 2021

########################
## Required Libraries ##
########################

library(RNetCDF)						# for reading NetCDF files
library(stringi)						# for easier manipulation of character strings

######################
## Global variables ##
######################

tz    <- "GMT"								# in R, GMT timezone = UTC
t1970 <- as.POSIXct("1970-01-01",tz=tz)		# unix reference time

########################
## Global data frames ##
########################

# info.Parsivel = general information about Parsivel fields, according to OTT Parsivel documentaton
# var.info.NetCDF.Parsivel = information about each variable to be saved in the NetCDF files

info.Parsivel <- data.frame(Field=character(),Name=character(),Digits=numeric(),Units=character(),Type=character(),stringsAsFactors=FALSE)
info.Parsivel[1,] <- c("01","Rain intensity 32 bit",8,"mm/h","single_number")
info.Parsivel[2,] <- c("02","Rain amount accumulated 32 bit",7,"mm","single_number")
info.Parsivel[3,] <- c("03","Weather code SYNOP Table 4680",2,"","single_number")
info.Parsivel[4,] <- c("04","Weather code SYNOP Table 4677",2,"","single_number")
info.Parsivel[5,] <- c("05","Weather code METAR Table 4678",5,"","character_string")
info.Parsivel[6,] <- c("06","Weather code NWS",4,"","character_string")
info.Parsivel[7,] <- c("07","Radar reflectivity 32 bit",6,"dBZ","single_number")
info.Parsivel[8,] <- c("08","MOR visibility in precipitation",5,"m","single_number")
info.Parsivel[9,] <- c("09","Sample interval",5,"s","single_number")
info.Parsivel[10,] <- c("10","Signal amplitude of laser",5,"","single_number")
info.Parsivel[11,] <- c("11","Number of particles detected and validated",5,"","single_number")
info.Parsivel[12,] <- c("12","Temperature in sensor housing",3,"degree_Celsius","single_number")
info.Parsivel[13,] <- c("13","Sensor serial number",6,"","character_string")
info.Parsivel[14,] <- c("14","Firmware IOP",6,"","character_string")
info.Parsivel[15,] <- c("15","Firmware DSP",6,"","character_string")
info.Parsivel[16,] <- c("16","Sensor head heating current",4,"A","single_number")
info.Parsivel[17,] <- c("17","Power supply voltage",4,"V","single_number")
info.Parsivel[18,] <- c("18","Sensor status",1,"","single_number")
info.Parsivel[19,] <- c("19","Date/time measuring start",19,"DD.MM.YYYY_hh:mm:ss","character_string")
info.Parsivel[20,] <- c("20","Sensor time",8,"hh:mm:ss","character_string")
info.Parsivel[21,] <- c("21","Sensor date",10,"DD.MM.YYYY","character_string")
info.Parsivel[22,] <- c("22","Station name",4,"","character_string")
info.Parsivel[23,] <- c("23","Station number",4,"","character_string")
info.Parsivel[24,] <- c("24","Rain amount absolute 32 bit",7,"mm","single_number")
info.Parsivel[25,] <- c("25","Error code",3,"","character_string")
info.Parsivel[26,] <- c("26","Temperature PCB",3,"degree_Celsius","single_number")
info.Parsivel[27,] <- c("27","Temperature in right sensor head",3,"degree_Celsius","single_number")
info.Parsivel[28,] <- c("28","Temperature in left sensor head",3,"degree_Celsius","single_number")
info.Parsivel[29,] <- c("30","Rain intensity 16 bit max 30 mm/h",6,"mm/h","single_number")
info.Parsivel[30,] <- c("31","Rain intensity 16 bit max 1200 mm/h",6,"mm/h","single_number")
info.Parsivel[31,] <- c("32","Rain amount accumulated 16 bit",7,"mm","single_number")
info.Parsivel[32,] <- c("33","Radar reflectivity 16 bit",5,"dBZ","single_number")
info.Parsivel[33,] <- c("34","Kinetic energy",7,"J/(m2*h)","single_number")
info.Parsivel[34,] <- c("35","Snowfall intensity",7,"mm/h","single_number")
info.Parsivel[35,] <- c("60","Number of all particles detected",8,"","single_number")
info.Parsivel[36,] <- c("61","List of all particles detected",13,"","list")
info.Parsivel[37,] <- c("90","FieldN",224,"","vector")
info.Parsivel[38,] <- c("91","FieldV",224,"","vector")
info.Parsivel[39,] <- c("93","Raw data",4096,"","matrix")
info.Parsivel[,"Digits"] <- as.numeric(info.Parsivel[,"Digits"])

var.info.NetCDF.Parsivel <- data.frame(field=character(),short_name=character(),standard_name=character(),long_name=character(),units=character(),type=character(),stringsAsFactors=FALSE)
var.info.NetCDF.Parsivel[1,] <- c("-","time","time","start time of observation period","","NC_STRING")
var.info.NetCDF.Parsivel[2,] <- c("-","lower_diameter","","lower diameter class limit","mm","NC_FLOAT")
var.info.NetCDF.Parsivel[3,] <- c("-","upper_diameter","","upper diameter class limit","mm","NC_FLOAT")
var.info.NetCDF.Parsivel[4,] <- c("-","lower_velocity","","lower velocity class limit","m/s","NC_FLOAT")
var.info.NetCDF.Parsivel[5,] <- c("-","upper_velocity","","upper velocity class limit","m/s","NC_FLOAT")
var.info.NetCDF.Parsivel[6,] <- c("-","latitude","latitude","latitude of measurement site","degrees_north","NC_FLOAT")
var.info.NetCDF.Parsivel[7,] <- c("-","longitude","longitude","longitude of measurement site","degrees_east","NC_FLOAT")
var.info.NetCDF.Parsivel[8,] <- c("-","altitude","altitude","height above mean sea level","m","NC_FLOAT")
var.info.NetCDF.Parsivel[9,] <- c("01","rain_rate","rainfall_rate","rainfall rate","mm/h","NC_FLOAT")
var.info.NetCDF.Parsivel[10,] <- c("02","acc_rain_amount","","accumulate rain amount","mm","NC_FLOAT")
var.info.NetCDF.Parsivel[11,] <- c("03","code_4680","","SYNOP weather code according to table 4680 of Parsivel documentation","","NC_INT")
var.info.NetCDF.Parsivel[12,] <- c("04","code_4677","","SYNOP weather code according to table 4677 of Parsivel documentation","","NC_INT")
var.info.NetCDF.Parsivel[13,] <- c("05","code_4678","","METAR/SPECI weather code according to table 4678 of Parsivel documentation","","NC_STRING")
var.info.NetCDF.Parsivel[14,] <- c("06","code_NWS","","NWS weather code according to Parsivel documentation","","NC_STRING")	
var.info.NetCDF.Parsivel[15,] <- c("07","reflectivity","","radar reflectivity","dBZ","NC_FLOAT")			
var.info.NetCDF.Parsivel[16,] <- c("08","MOR","","Meteorological Optical Range in precipitation","m","NC_FLOAT")
var.info.NetCDF.Parsivel[17,] <- c("10","amplitude","","signal amplitude of laser strip","","NC_FLOAT")	
var.info.NetCDF.Parsivel[18,] <- c("11","n_particles","","number of particles detected and validated","","NC_INT")	
var.info.NetCDF.Parsivel[19,] <- c("12","temperature_sensor","","temperature in sensor housing","degree_Celsius","NC_INT")	
var.info.NetCDF.Parsivel[20,] <- c("16","heating_current","","sensor head heating current","A","NC_FLOAT")	
var.info.NetCDF.Parsivel[21,] <- c("17","voltage","","power supply voltage","V","NC_FLOAT")
var.info.NetCDF.Parsivel[22,] <- c("18","sensor_status","","sensor status","","NC_FLOAT")	
var.info.NetCDF.Parsivel[23,] <- c("25","error_code","","error code","","NC_STRING")
var.info.NetCDF.Parsivel[24,] <- c("26","temperature_PCB","","temperature in printed circuit board","degree_Celsius","NC_INT")		
var.info.NetCDF.Parsivel[25,] <- c("27","temperature_right","","temperature in right sensor head","degree_Celsius","NC_INT")	
var.info.NetCDF.Parsivel[26,] <- c("28","temperature_left","","temperature in left sensor head","degree_Celsius","NC_INT")		
var.info.NetCDF.Parsivel[27,] <- c("34","kinetic_energy","","kinetic energy","J/(m2*h)","NC_FLOAT")		
var.info.NetCDF.Parsivel[28,] <- c("35","snowfall_intensity","lwe_snowfall_rate","volume equivalent snow depth intensity","mm/h","NC_FLOAT")	
var.info.NetCDF.Parsivel[29,] <- c("90","ND","","particle number concentrations per diameter class","1/(m3*mm)","NC_FLOAT")	
var.info.NetCDF.Parsivel[30,] <- c("91","VD","","average particle velocities for each diameter class","m/s","NC_FLOAT")	
var.info.NetCDF.Parsivel[31,] <- c("93","N","","drop counts per diameter and velocity class","","NC_INT")						
			
################################
####### USEFUL FUNCTIONS #######
################################

get.classD.Parsivel <- function(){

    ## Returns a 32x2 matrix containing the lower/upper diameter limits 
    ## of the Parsivel disdrometer (in mm).

    classD <- matrix(NA_real_,nrow=32,ncol=2)
    classD[1,] <- c(0,0.1245)
    classD[2,] <- c(0.1245,0.2495)
    classD[3,] <- c(0.2495,0.3745)
    classD[4,] <- c(0.3745,0.4995)
    classD[5,] <- c(0.4995,0.6245)
    classD[6,] <- c(0.6245,0.7495)
    classD[7,] <- c(0.7495,0.8745)
    classD[8,] <- c(0.8745,0.9995)
    classD[9,] <- c(0.9995,1.1245)
    classD[10,] <- c(1.1245,1.25)
    classD[11,] <- c(1.25,1.50)
    classD[12,] <- c(1.50,1.75)
    classD[13,] <- c(1.75,2.00)
    classD[14,] <- c(2.00,2.25)
    classD[15,] <- c(2.25,2.50)
    classD[16,] <- c(2.50,3.00)
    classD[17,] <- c(3.00,3.50)
    classD[18,] <- c(3.50,4.00)
    classD[19,] <- c(4.00,4.50)
    classD[20,] <- c(4.50,5.00)
    classD[21,] <- c(5.00,6.00)
    classD[22,] <- c(6.00,7.00)
    classD[23,] <- c(7.00,8.00)
    classD[24,] <- c(8.00,9.00)
    classD[25,] <- c(9.00,10.0)
    classD[26,] <- c(10.0,12.0)
    classD[27,] <- c(12.0,14.0)
    classD[28,] <- c(14.0,16.0)
    classD[29,] <- c(16.0,18.0)
    classD[30,] <- c(18.0,20.0)
    classD[31,] <- c(20.0,23.0)
    classD[32,] <- c(23.0,26.0)
    return(classD)
}

get.classV.Parsivel <- function(){

    ## Returns a 32x2 matrix containing the lower/upper velocity limits (in m/s)
    ## of the Parsivel disdrometer.

    classV <- matrix(NA_real_,nrow=32,ncol=2)
    classV[1,] <- c(0,0.1)
    classV[2,] <- c(0.1,0.2)
    classV[3,] <- c(0.2,0.3)
    classV[4,] <- c(0.3,0.4)
    classV[5,] <- c(0.4,0.5)
    classV[6,] <- c(0.5,0.6)
    classV[7,] <- c(0.6,0.7)
    classV[8,] <- c(0.7,0.8)
    classV[9,] <- c(0.8,0.9)
    classV[10,] <- c(0.9,1.0)
    classV[11,] <- c(1.0,1.2)
    classV[12,] <- c(1.2,1.4)
    classV[13,] <- c(1.4,1.6)
    classV[14,] <- c(1.6,1.8)
    classV[15,] <- c(1.8,2.0)
    classV[16,] <- c(2.0,2.4)
    classV[17,] <- c(2.4,2.8)
    classV[18,] <- c(2.8,3.2)
    classV[19,] <- c(3.2,3.6)
    classV[20,] <- c(3.6,4.0)
    classV[21,] <- c(4.0,4.8)
    classV[22,] <- c(4.8,5.6)
    classV[23,] <- c(5.6,6.4)
    classV[24,] <- c(6.4,7.2)
    classV[25,] <- c(7.2,8.0)
    classV[26,] <- c(8.0,9.6)
    classV[27,] <- c(9.6,11.2)
    classV[28,] <- c(11.2,12.8)
    classV[29,] <- c(12.8,14.4)
    classV[30,] <- c(14.4,16.0)
    classV[31,] <- c(16.0,19.2)
    classV[32,] <- c(19.2,22.4)
    return(classV)
}

create.file.name.Parsivel <- function(t,path,NETDL=6,ext.type=".txt"){

	## Create the file name (full path) for a given time of interest
	
	## Input:
	## t = the time, in seconds since 1970
	## path = the path to where the Parsivel data are stored (parent directory)
	## ext_type = the type of extension to be generated (.txt or .txt.txt)
	
	## Output:
	## file = the file corresponding to time t
	
	## Notes:
	## by convention, the time denotes the end of the measurement period

	old.str  <- sprintf("00NETDL_%1.2i",NETDL)
	str      <- sprintf("000NETDL%1.2i",NETDL)
	st   <- as.character(t1970+t)
	nst  <- nchar(st)
	yyyy <- substr(st,1,4)
	mm   <- substr(st,6,7)
	dd   <- substr(st,9,10)
	HHMM <- "0000"
	if(nst>15){HHMM <- sprintf("%s%s",substr(st,12,13),substr(st,15,16))}
	ymdhms <- sprintf("%s%s%s%s00",yyyy,mm,dd,HHMM) 
	folder <- sprintf("%s%s%s",yyyy,mm,dd)
	if(ext.type==".txt"){file <- sprintf("%s/%s/%s_%s%s",path,folder,str,ymdhms,ext.type)}	
	if(ext.type==".txt.txt"){file <- sprintf("%s/%s/%s_%s_%s%s",path,folder,str,str,ymdhms,ext.type)}		
	return(file)
}

read.single.Parsivel.file <- function(file,pars=info.Parsivel[["Field"]],verbose=FALSE){

	## Description:
	## This function can be used to read a single Parsivel file (in .txt or .txt.txt format)
	
	## Inputs:
	## file = file to be read (full path) 
	## pars = parameters to read (by default all fields in info.Parsivel)
	## verbose = display warnings for missing files
	
	## Output:
	## values = list of values (with names)

	npars <- length(pars)
	if(npars==0){stop("must provide at least one parameter")}	
	
	id.keep <- which(is.element(info.Parsivel[["Field"]],pars))
	
	## create empty output list
	out <- vector("list",length(info.Parsivel[["Field"]]))
	
	## prefill output list according to data types in info.Parsivel
	id1 <- which(info.Parsivel[["Type"]]=="single_number")
	id2 <- which(info.Parsivel[["Type"]]=="character_string")	
	id3 <- which(info.Parsivel[["Type"]]=="vector")
	id4 <- which(info.Parsivel[["Type"]]=="matrix")		
	for(i in id1){out[[i]] <- NA}
	for(i in id2){out[[i]] <- ""}
	for(i in id3){out[[i]] <- rep(NA,32)}		
	for(i in id4){out[[i]] <- matrix(NA,nrow=32,ncol=32)}	
	names(out) <- info.Parsivel[["Field"]]
	
	## check if file exists
	if(!file.exists(file)){
		if(verbose==TRUE){print.noquote(sprintf("warning: file %s not found",file))}
 		out <- out[id.keep]
 		names(out) <- pars
		return(out)											# if the file does not exist, return empty pre-filled list
	}	
	
	## read the .txt file
	data <- scan(file=file,what="list",sep="\n",skip=1,quiet=TRUE,skipNul=FALSE,nlines=45)
	
	## go through each line, extract values
	n <- length(data)
	for(i in 1:n){
		s <- data[i]										# data on line i (as character string)
		ns <- nchar(s)										# number of characters in s
		if(ns<4){next}										# skip empty lines
		field.name <- substr(s,1,2)							# first two characters are reserved for the field name
		s <- substr(s,4,ns)									# remove first three characters
		if(!is.element(field.name,pars)){next}				# skip if current field name is not among the list of desired parameters
		id <- which(info.Parsivel[["Field"]]==field.name)	# check which field name in the Parsivel.info table corresponds to current field name
		if(length(id)==0){next}								# skip if variable is unknown
		type <- info.Parsivel[["Type"]][id]					# variable type (single_number, character_string, vector, matrix or list)
		digits <- info.Parsivel[["Digits"]][id]				# number of expected digits/characters
		values <- stri_split_fixed(s,";")					# splits s into different character strings according to separation character ";"	
		ns <- nchar(s)										# number of characters in s (= actual number of digits received)
		
		if(type=="character_string" && ns<digits && ns==1){values <- ""}	
		if(ns!=digits && ns>1){	
			long_name <- info.Parsivel[id,"Name"]
			print(sprintf("error: incorrect number of digits for field %s (%s)",field.name,long_name))
			print(sprintf("expected: %i ; received %i",digits,ns))
			stop()
		}			
		if(type!="character_string"){
			values <- unlist(values)						# unlist the character strings 
			values <- as.numeric(values)					# transform the values into numberic format
			nv <- length(values)							# defines the number of values
			if(nv>1){values <- values[-nv]}					# if there is more than 1 value, remove the last (necessary due to the way R handles lists)
			if(type=="matrix"){
				values  <- matrix(values,nrow=32,ncol=32)	# essentially only applies to field 93.
			}	
			if(field.name=="90" && sum(!is.na(values)>0)){	# field 90 needs special processing
				id.zero <- which(values<=(-9.999))			# indexes which need to be forced to zero after transformation
				values <- 10^values							# transform from log scale to linear scale
				values[id.zero] <- 0						# force indexes id.zero to zero
			}			
		}
		out[[id]] <- values									# put the extracted values into the output list (at the right index id)
	}		
	out <- out[id.keep]										# only keep the elements that were queried
	names(out) <- pars										# assign names to each element in the output list	
	return(out)												# return the output list
}

read.Parsivel.data <- function(ntb,nte,path,pars=info.Parsivel[["Field"]],dt.Parsivel=60,NETDL=6,ext.type=".txt"){

	## Description: 
	## This function can be used to read all Parsivel data between time ntb and time nte

	## Input:
	## ntb/nte beginning and ending time of interval
	## path = path where the Parsivel data is stored (parent directory)
	## pars = parameters to be read (by default all)
	## dt.Parsivel = one file every ... seconds (by default, 60 seconds)
	## ext.type = extention type (either .txt or .txt.txt. By default .txt).
	
	## Output:
	## out = list of parameter values
	
	verbose <- TRUE
	if(ext.type==".txt.txt"){verbose <- FALSE}
	
	## create timetable
	tabT  <- seq(from=ntb,to=nte,by=dt.Parsivel)
	NtabT <- length(tabT)	

	## Read the data separately for each time step
	list.data <- vector("list",NtabT)
	for(itt in 1:NtabT){
		t <- tabT[itt]
		file <- create.file.name.Parsivel(t,path,NETDL=NETDL,ext.type=ext.type)	
		list.data[[itt]] <- read.single.Parsivel.file(file,pars=pars,verbose=verbose)
	}
	
	## go through each parameter and append values along the time dimension
	npars <- length(pars)
	out <- vector("list",npars)	
	for(itp in 1:npars){
		par <- pars[itp]
		idp <- which(info.Parsivel[["Field"]]==par)
		if(length(idp)==0){
			out[[itp]] <- c()
			next
		}
		type <- info.Parsivel[["Type"]][idp]
		if(type=="single_number"){
			values <- rep(NA,NtabT)								# data structure becomes a vector of numbers
			for(itt in 1:NtabT){
				if(length(list.data[[itt]][[itp]])==0){next}
				values[itt] <- list.data[[itt]][[itp]]
			}
		}
		if(type=="character_string"){
			values <- rep("",NtabT)								# data structure becomes a vector of character strings
			for(itt in 1:NtabT){
				if(length(list.data[[itt]][[itp]])==0){next}			
				values[itt] <- unlist(list.data[[itt]][[itp]])
			}
		}
		if(type=="vector"){
			values <- matrix(NA,nrow=32,ncol=NtabT)				# data structure becomes a matrix, with time along columns
			for(itt in 1:NtabT){
				if(length(list.data[[itt]][[itp]])==0){next}			
				values[,itt] <- list.data[[itt]][[itp]]
			}
		}
		if(type=="matrix"){
			values <- array(NA,dim=c(32,32,NtabT))
			for(itt in 1:NtabT){
				values[,,itt] <- list.data[[itt]][[itp]]
			}	
		}
		if(type=="list"){
			values <- vector("list",NtabT)
			for(itt in 1:NtabT){
				if(length(list.data[[itt]][[itp]])==0){next}			
				values[[itt]] <- list.data[[itt]][[itp]]
			}		
		}	
		out[[itp]] <- values
	}
	names(out) <- pars
	return(out)	
}

combo.read.Parsivel.data <- function(ntb,nte,path,pars=info.Parsivel[["Field"]],dt.Parsivel=60,NETDL=6){

	## Description:
	## This function reads and combines all Parsivel data between ntb and nte
	
	## Inputs:
	## ntb/nte = starting and ending time (numeric format)
	## path = path where Parsivel data are stored (parent directory)
	## pars = parameters to be read (by default all)
	## dt.Parsivel = sampling resolution (by default, 60 seconds)

	## Basic tests:
	if(ntb>nte){stop("ntb>nte")}
	
	## Read all data (from both .txt and .txt.txt files)
	data1 <- read.Parsivel.data(ntb,nte,path,dt.Parsivel=dt.Parsivel,NETDL=NETDL,ext.type=".txt")
	data2 <- read.Parsivel.data(ntb,nte,path,dt.Parsivel=dt.Parsivel,NETDL=NETDL,ext.type=".txt.txt")	
	
	## Create timetable
	tabT <- seq(ntb,nte,dt.Parsivel)
	NtabT <- length(tabT)
	
	## go through each parameter, combine data
	npars <- length(pars)
	out <- vector("list",npars)
	for(itp in 1:npars){
		values1 <- data1[[itp]]	# either a vector of values (for numbers and character strings) or a list
		values2 <- data2[[itp]]
		par <- pars[itp]
		idp <- which(info.Parsivel[["Field"]]==par)
		type <- info.Parsivel[["Type"]][idp]
		if(type=="single_number"){
			values <- rep(NA,NtabT)
			id1 <- which(!is.na(values1) & is.na(values2))
			id2 <- which(is.na(values1) & !is.na(values2))
			values[id1] <- values1[id1]
			values[id2] <- values2[id2]
		}
		if(type=="character_string"){
			values <- rep("",NtabT)
			id1 <- which(values1!="" & values2=="")
			id2 <- which(values1=="" & values2!="")
			values[id1] <- values1[id1]
			values[id2] <- values2[id2]			
		}		
		if(type=="vector"){
			# data structure becomes a matrix, with time as last dimension (i.e., one column per time step)
			values <- matrix(NA,nrow=32,ncol=NtabT)
			for(itt in 1:NtabT){
				v1 <- values1[,itt]
				v2 <- values2[,itt]
				id1 <- which(!is.na(v1) & is.na(v2))
				id2 <- which(is.na(v1) & !is.na(v2))
				values[id1,itt] <- v1[id1]
				values[id2,itt] <- v2[id2]			
			}		
		}		
		if(type=="matrix"){
			# data structure becomes an array, with time as last dimension
			values <- array(NA,dim=c(32,32,NtabT))
			for(itt in 1:NtabT){
				v1 <- as.vector(values1[,,itt])
				v2 <- as.vector(values2[,,itt])
				v  <- rep(NA,1024)
				id1 <- which(!is.na(v1) & is.na(v2))
				id2 <- which(is.na(v1) & !is.na(v2))
				v[id1] <- v1[id1]
				v[id2] <- v2[id2]
				values[,,itt] <- matrix(v,nrow=32,ncol=32)
			}				
		}
		if(type=="list"){
			values <- vector("list",NtabT)					# this part is not ready yet...
		}
		out[[itp]] <- values
	}
	names(out) <- pars
	return(out)
}


create.daily.NetCDFfile.Parsivel <- function(day,input.path,output.path,site_name,Parsivel_number,latitude,longitude,altitude,NETDL=6,dt.Parsivel=60){

	## Reads all Parsivel data between tb and te and creates a NetCDF file
	
	## Inputs:
	## day = character string of length 10 (yyyy-mm-dd) describing the day to be processed.
	## input.path = path to where the Parsivel data are stored (parent directoy)
	## output.path = output path to where the NetCDF file should be stored
	## dt.Parsivel = time resolution of Parsivel (by default, 60s)
	
	if(nchar(day)!=10){stop("date must be character string of length 8 (yyyymmdd)")}
	yyyy <- substr(day,1,4)
	mm   <- substr(day,6,7)
	dd   <- substr(day,9,10)
	tb   <- as.POSIXct(sprintf("%s-%s-%s",yyyy,mm,dd),tz=tz)
	ntb  <- as.numeric(tb)
	nte  <- ntb + 24*3600 - dt.Parsivel

	pars  <-  info.Parsivel[["Field"]]
	npars <- length(pars)
	
	sensor_name <- sprintf("PAR%1.3i_%s",Parsivel_number,site_name)
	
	## Create timetable
	tabT.Parsivel  <- seq(ntb,nte,dt.Parsivel)
	NtabT.Parsivel <- length(tabT.Parsivel)
	time.as.string <- as.character(t1970+tabT.Parsivel)
	nc <- nchar(time.as.string)
	id <- which(nc<19)
	if(length(id)>0){
		for(i in id){
			print(time.as.string[i])		
			time.as.string[i] <- paste(time.as.string[i]," 00:00:00")
			stop("check new time as string")
			print(time.as.string[i])
		}
	}
	
	## Read and combine all Parsivel data (using both .txt and .txt.txt files)
	print.noquote("calling combo read Parsivel")
	path <- sprintf("%s/%s",input.path,sensor_name)
	data.Parsivel <- combo.read.Parsivel.data(ntb,nte,path=path,NETDL=NETDL,dt.Parsivel=dt.Parsivel)

	## Change storage mode (if necessary)
	for(itp in 1:npars){
		par <- pars[itp]
		id <- which(var.info.NetCDF.Parsivel[["field"]]==par)
		if(length(id)==0){next}
		desired.type <- var.info.NetCDF.Parsivel[["type"]][id]
		if(desired.type=="NC_INT"){
			storage.mode(data.Parsivel[[itp]]) <- "integer"
		}
	}	
		
	## Create empty NetCDF file
	yyyymmdd <- paste(yyyy,mm,dd,sep="")
	output_file <- sprintf("%s/%s_%s.nc",output.path,sensor_name,yyyymmdd)		
	print(output_file)
	nc <- create.nc(output_file,format="netcdf4")
	
	## Define dimensions
	dim.def.nc(nc,"diameter",dimlength=32)
	dim.def.nc(nc,"velocity",dimlength=32)		
	dim.def.nc(nc,"time",dimlength=NtabT.Parsivel)
		
	## Define variables
	nvars <- dim(var.info.NetCDF.Parsivel)[1]
	for(i in 1:nvars){
		short_name <- var.info.NetCDF.Parsivel[["short_name"]][i]
		dimensions <- c("time")
		type <- var.info.NetCDF.Parsivel[["type"]][i]
		if(short_name=="lower_diameter" || short_name=="upper_diameter"){dimensions <- c("diameter")}
		if(short_name=="lower_velocity" || short_name=="upper_velocity"){dimensions <- c("diameter")}	
		if(short_name=="latitude" || short_name=="longitude" || short_name=="altitude"){dimensions <- NA}	
		if(short_name=="ND" || short_name=="VD"){dimensions <- c("diameter","time")}
		if(short_name=="N"){dimensions <- c("diameter","velocity","time")}
		var.def.nc(nc,short_name,vartype=type,dimensions=dimensions,chunking=TRUE,deflate=5)
	}
	
	## Insert values for each variable	
	var.put.nc(nc,variable="lower_diameter",data=get.classD.Parsivel()[,1])		
	var.put.nc(nc,variable="upper_diameter",data=get.classD.Parsivel()[,2])	
	var.put.nc(nc,variable="lower_velocity",data=get.classV.Parsivel()[,1])		
	var.put.nc(nc,variable="upper_velocity",data=get.classV.Parsivel()[,2])				
	var.put.nc(nc,variable="time",data=time.as.string)		
	var.put.nc(nc,variable="latitude",data=latitude)
	var.put.nc(nc,variable="longitude",data=longitude)	
	var.put.nc(nc,variable="altitude",data=altitude)		
	for(i in 1:nvars){
		short_name <- var.info.NetCDF.Parsivel[["short_name"]][i]
		field <- var.info.NetCDF.Parsivel[["field"]][i]
		type  <- var.info.NetCDF.Parsivel[["type"]][i]
		if(field=="-"){next}							# these special fields are covered in the code above
		id <- which(info.Parsivel[["Field"]]==field)
		if(length(id)==0){
			stop("length(id)==0")
			next
		}
		values <- data.Parsivel[[id]]	
		if(type=="NC_STRING"){
			is.NA <- which(values=="")
			values[is.NA] <- "-"
			att.put.nc(nc,short_name,"missing_value",type,"-")			
		}	
		if(type=="NC_INT"){
			is.NA <- which(is.na(values))
			values[is.NA] <- -9999		
		}					
		if(type=="NC_FLOAT"){
			is.NA <- which(is.na(values))
			values[is.NA] <- NaN	
		}
		var.put.nc(nc,variable=short_name,data=values)
	}
	
	## Set attributes for each variable
	for(i in 1:nvars){
		short_name <- var.info.NetCDF.Parsivel[["short_name"]][i]
		long_name <- var.info.NetCDF.Parsivel[["long_name"]][i]
		standard_name <- var.info.NetCDF.Parsivel[["standard_name"]][i]
		type <- var.info.NetCDF.Parsivel[["type"]][i]		
		units <- var.info.NetCDF.Parsivel[["units"]][i]				
		att.put.nc(nc,short_name,"long_name","NC_STRING",long_name)
		if(units!=""){att.put.nc(nc,short_name,"units","NC_STRING",units)}		
		if(standard_name!=""){att.put.nc(nc,short_name,"standard_name","NC_STRING",standard_name)}		
		if(i>=9){	# first 8 variables don't have missing value attribute.
			if(type=="NC_STRING"){att.put.nc(nc,short_name,"missing_value",type,"-")}
			if(type=="NC_INT"){att.put.nc(nc,short_name,"missing_value",type,-9999)}
			if(type=="NC_FLOAT"){att.put.nc(nc,short_name,"missing_value",type,NaN)}	
		}			
	}		
	att.put.nc(nc,"time","calendar","NC_STRING","standard")	
		
	## Insert global attributes
	serial_number <- unique(data.Parsivel[["13"]])
	keep <- which(serial_number!="")
	Nkeep <- length(keep)
	if(Nkeep==0){serial_number <- "-"}
	if(Nkeep==1){serial_number <- serial_number[keep]}
	if(Nkeep>1){stop("multiple serial numbers detected...")}
	
	firmware_IOP <- unique(data.Parsivel[["14"]])
	keep <- which(firmware_IOP!="")
	Nkeep <- length(keep)	
	if(Nkeep==0){firmware_IOP <- "-"}
	if(Nkeep==1){firmware_IOP <- firmware_IOP[keep]}
	if(Nkeep>1){stop("multiple firmware_IOP detected...")}		
	
	firmware_DSP <- unique(data.Parsivel[["15"]])
	keep <- which(firmware_DSP!="")
	Nkeep <- length(keep)
	if(Nkeep==0){firmware_DSP="-"}
	if(Nkeep==1){firmware_DSP <- firmware_DSP[keep]}
	if(Nkeep>1){stop("multiple firmware_DSP detected...")}				
	
	att.put.nc(nc,"NC_GLOBAL","title","NC_STRING","Parsivel2 disdrometer data")	
	att.put.nc(nc,"NC_GLOBAL","insitution","NC_STRING","Delft University of Technology")
	att.put.nc(nc,"NC_GLOBAL","source","NC_STRING","surface observation")
	att.put.nc(nc,"NC_GLOBAL","history","NC_STRING","version 1.1")			
	att.put.nc(nc,"NC_GLOBAL","Conventions","NC_STRING","CF-1.7")		
				
	att.put.nc(nc,"NC_GLOBAL","site_name","NC_STRING",site_name)
	att.put.nc(nc,"NC_GLOBAL","sensor_name","NC_STRING",sensor_name)	
	att.put.nc(nc,"NC_GLOBAL","project_name","NC_STRING","https://ruisdael-observatory.nl/")
	att.put.nc(nc,"NC_GLOBAL","contributors","NC_STRING","Marc Schleiss, Saverio Guzzo, Rob Mackenzie")	
	
	att.put.nc(nc,"NC_GLOBAL","sensor_type","NC_STRING","OTT_Hydromet_Parsivel2")			
	att.put.nc(nc,"NC_GLOBAL","sensor_serial_number","NC_STRING",serial_number)		
	att.put.nc(nc,"NC_GLOBAL","firmware_IOP","NC_STRING",firmware_IOP)		
	att.put.nc(nc,"NC_GLOBAL","firmware_DSP","NC_STRING",firmware_DSP)	
	
    close.nc(nc) 		
	return(output_file)
}
