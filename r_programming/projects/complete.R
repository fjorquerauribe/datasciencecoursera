complete <- function(directory, id = 1:332){
    # create vector with filenames
    filenames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
    
    # calculate num of completely observed cases in each data file
    my_data <- mapply(function(f,i) data.frame(i, sum(complete.cases(read.csv(f)))), filenames, id, USE.NAMES = FALSE)
    my_data <- as.data.frame(t(my_data))
    names(my_data) <- c("id", "nobs")
    my_data
}