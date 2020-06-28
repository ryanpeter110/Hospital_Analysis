hospital <- read.csv('hospital-data.csv')
outcome <- read.csv('outcome-of-care-measures.csv')

#head(outcome)
#data <- as.numeric(outcome[,11])
#hist(data)

findOutcome <- function(out){
        reasonNames <- colnames(outcome)[c(11,17,23)]
        
        if(out == 'heart attack'){
                return(reasonNames[1])
        }else if(out == 'heart failure'){
                return(reasonNames[2])
        }else if(out == 'pneumonia'){
                return(reasonNames[3])
        }else{
                return(-1)
        }
}

state.is.valid <- function(state){
        state2 <- outcome$State
        if(!(state %in% unique(state2))){
                return(-1)
        }else{
                return(state2)
        }
}


best <- function(state1,out){
     
     hname <- outcome$Hospital.Name

     state2 <- state.is.valid(state1)
     suppressWarnings(if(state2 == -1){return("Invalid State")})
     reason <- findOutcome(out)
     if(reason == -1){return('Invalid Outcome')}
     
     reason <- suppressWarnings(as.numeric(outcome[[reason]]))
     
     relaventDF <- as.data.frame(cbind(hname,state2,reason))
     relaventDF <- relaventDF[complete.cases(relaventDF),]
     relaventDF <- split(relaventDF, relaventDF$state2)
     relaventDF <- relaventDF[[state1]]
     relaventDF$reason <- as.numeric(relaventDF$reason)
     
     bb <- split(relaventDF,relaventDF$reason)
     minimum<-min(relaventDF$reason)
     return((bb[[as.character(minimum)]])$hname)

}

rankhospital <- function(state1,out,num='best'){
        

        hname <- outcome$Hospital.Name
        state2 <- state.is.valid(state1)
        suppressWarnings(if(state2 == -1){return("Invalid State")})
        reason <- findOutcome(out)
        if(reason == -1){return('Invalid Outcome')}
        reason <- suppressWarnings(as.numeric(outcome[[reason]]))
        
        relaventDF <- as.data.frame(cbind(hname,state2,reason))
        relaventDF <- relaventDF[complete.cases(relaventDF),]
        relaventDF <- split(relaventDF, relaventDF$state2)
        relaventDF <- relaventDF[[state1]]
        relaventDF$reason <- as.numeric(relaventDF$reason)
        
        sortedOrder <- order(relaventDF$reason)
        newDF<-relaventDF[sortedOrder,]
        
        if(num=='best'){
                num <- 1
        }else if(num=='worst'){
                num<-length(sortedOrder)
        }else if(as.numeric(num)>length(sortedOrder)){
                return(NA)
        }
        
        newDF[as.numesric(num),]
        
}


rankall <- function(out, num="best"){
        x <- c()
        state2 <- unique(outcome$State)
        for(i in state2){
                x <- rbind(x,rankhospital(i,out,num))
        }
        x<- as.data.frame(x)
        x
        
}


