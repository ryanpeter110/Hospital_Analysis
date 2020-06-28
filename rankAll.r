rankAll <- function(outcome, num = "best") {

    dataAll <- data.frame(hospital = character(), state = character())
  
## Read outcome data

    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
## Check that outcome and num are valid

    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        dataAll <- "invalid outcome"
    }
    else {
        keys <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        outcomeKey <- keys[outcome]

## For each state, find the hospital of the given rank

        dataPerState <- split(data, data$State)
        for (stat in names(dataPerState)) {
        dataOurState <- dataPerState[[stat]]
        dataOutcome <- suppressWarnings(as.numeric(dataOurState[, outcomeKey]))
        good <- complete.cases(dataOutcome)
        dataOutcome <- dataOutcome[good]
        dataOurState <- dataOurState[good,]
        dataOurState <- dataOurState[ order(dataOutcome, dataOurState["Hospital.Name"]), ]
        
        if (num == "best") {
            numState <- c(1)
        } else {
            if (num == "worst") {
                numState <- length(dataOutcome)
            } else {
                numState <- num
            }
        }
    
        dataPart <- data.frame(hospital = dataOurState[numState, "Hospital.Name"], state = stat, row.names = stat)
        dataAll <- rbind(dataAll, dataPart)
        }
    }

## Return a data frame with the hospital names and the (abbreviated) state name

    dataAll
}
