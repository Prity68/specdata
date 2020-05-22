#All assignments R programming course
pollutantmean<-function(directory,pollutant,id){
  setwd(directory)
  files <- list.files(path = getwd(), pattern = "*.csv", full.names = T)
  pollutantDt<-numeric()
  for(i in id){
    dataPerMonitor<-na.omit(read.csv(files[i]))
    pollutantDt<-c(pollutantDt,dataPerMonitor[[pollutant]])
  }
  mean(pollutantDt)
  
}

complete<-function(directory,id){
  setwd(directory)
  files <- list.files(path = getwd(), pattern = "*.csv", full.names = T)
  finalDt<-data.frame()
  for(j in id){
    dataPerMonitor<-na.omit(read.csv(files[j]))
    temp<-data.frame(id=j,nobs=nrow(dataPerMonitor))
    finalDt<-rbind(finalDt,temp)
  }
  finalDt
}

corr<-function(directory,threshold){
  if(missing(threshold)){threshold<-0}
  correlation<-numeric()
  setwd(directory)
  files <- list.files(path = getwd(), pattern = "*.csv", full.names = T)
  for(k in 1:332){
    monitor<-na.omit(read.csv(files[k]))
    if(nrow(monitor)>threshold)
    {
      correlation<-c(correlation,cor(monitor[["sulfate"]],monitor[["nitrate"]]))
    }
    
  }
  if(length(correlation)==0){
    0
  }
  else
    {
      correlation
  }
  
}

makeCacheMatrix<-function(x=numeric())
{
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
get<-function(){x}
  setinverse<-function(inverse){
    i<<-inverse
  }
  getinverse<-function(){
    i
  }
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

cacheSolve<-function(x,...){
 i<-x$getinverse()
 if(!is.null(i)){
   message("getting cached inverse")
   return(i)
 }
 else{
   data<-x$get()
   i<-solve(data,...)
   x$setinverse(i)
   i
 }
}

outcome<-function(){
  outcomes<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  hist(as.numeric(outcomes[,11]))
}

best<-function(state,outcome){
  rw<-NULL
  outcomes<-read.csv("outcome-of-care-measures.csv",colClasses="character")
  outcomes <- outcomes[outcomes$State == state, ]
  if(nrow(outcomes)==0){
    stop("invalid state")
  }
  if(outcome=="heart attack")
  {
  mortalityRateForheartAttack<-na.omit(as.numeric(outcomes[[11]]))
  rw<-which(as.numeric(outcomes[[11]])==min(mortalityRateForheartAttack), arr.ind=TRUE)
  }
  else if(outcome=="heart failure"){
  mortalityRateForheartFailure<-na.omit(as.numeric(outcomes[[17]]))
  rw<-which(as.numeric(outcomes[[17]])==min(mortalityRateForheartFailure), arr.ind=TRUE)
  }
  else if(outcome=="pneumonia"){
  mortalityRateForpneumonia<-na.omit(as.numeric(outcomes[[23]]))
  rw<-which(as.numeric(outcomes[[23]])==min(mortalityRateForpneumonia), arr.ind=TRUE)
  }
  else{
    stop("invalid outcome")
  }
  outcomes[rw,2]          
}