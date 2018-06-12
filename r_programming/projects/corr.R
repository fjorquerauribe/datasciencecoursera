corr <- function(directory, threshold = 0){
    # create vector with filenames
    filenames <- paste0(directory, '/', formatC(1:332, width=3, flag="0"), ".csv" )
    
    # function to calculate correlation
    my_cor <- function(f){
        file_data <- read.csv(f)
        good <- complete.cases(file_data)
        if(sum(good) > threshold){
            return(cor(file_data[good,"sulfate"], file_data[good,"nitrate"]))
        }
    }
    
    # apply in each file data
    my_data <- lapply(filenames, my_cor)
    my_data[sapply(my_data, is.null)] <- NULL
    if(length(my_data) == 0){
        return(list())
    }
    unlist(my_data)
}