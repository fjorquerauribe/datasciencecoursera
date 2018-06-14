best <- function(state_name, outcome_name) {
    # Read outcome data
    out_dt <- read.csv("../data/ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    if( !(state_name %in% unique(out_dt[["State"]])) ) stop("Invalid State")
    
    if( !(outcome_name %in% c("heart attack", "heart failure", "pneumonia"))) stop("Invalid Outcome")
    
    out_dt <- out_dt[out_dt$State == state_name, ]
    
    if (outcome_name == "heart attack") {
        out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        out_dt <- out_dt[!is.na(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        out_dt <- out_dt[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") ]
    } else if(outcome_name == "heart failure"){
        out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        out_dt <- out_dt[!is.na(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        out_dt <- out_dt[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") ]
    } else {
        out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        out_dt <- out_dt[!is.na(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
        out_dt <- out_dt[, c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") ]
    }
    
    out_dt <- out_dt[order(out_dt$Hospital.Name),]
    
    idxMin <- which.min(out_dt[,2])
    
    return(out_dt[idxMin, "Hospital.Name"])
}