rankall <- function(outcome_name, num) {
    # Read outcome data
    out_dt <- read.csv("../data/ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE)
    
    #if( !(state_name %in% unique(out_dt[["State"]])) ) stop("Invalid State")
    
    if( !(outcome_name %in% c("heart attack", "heart failure", "pneumonia"))) stop("Invalid Outcome")
    
    #out_dt <- out_dt[out_dt$State == state_name, ]
    
    if (outcome_name == "heart attack") {
        out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        out_dt <- out_dt[!is.na(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
        out_dt <- out_dt[, c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack") ]
        out_dt <- out_dt[with(out_dt, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, Hospital.Name)),]
    } else if(outcome_name == "heart failure"){
        out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        out_dt <- out_dt[!is.na(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]
        out_dt <- out_dt[, c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure") ]
        out_dt <- out_dt[with(out_dt, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, Hospital.Name)),]
    } else {
        out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
        out_dt <- out_dt[!is.na(out_dt$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
        out_dt <- out_dt[, c("Hospital.Name", "State", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia") ]
        out_dt <- out_dt[with(out_dt, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, Hospital.Name)),]
    }
    
    states <- unique(out_dt)
    
    results <- data.frame(hospital = character(), state = character())
    
    
    
    if(num == "best"){
        return(out_dt[1, "Hospital.Name"])
    }
    
    if(num == "worst"){
        return(out_dt[nrow(out_dt), "Hospital.Name"])
    }
    
    return(out_dt[num, "Hospital.Name"])
}

findHospital <- function(num) {
    
}