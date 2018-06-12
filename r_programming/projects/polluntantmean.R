pollutantmean <- function(directory, pollutant, id = 1:332){
    # create vector with filenames
    filenames <- paste0(directory, '/', formatC(id, width=3, flag="0"), ".csv" )
    
    # load files in data frame
    my_data <- do.call(rbind, lapply(filenames, function(x) read.csv(x, stringsAsFactors = FALSE)))
    
    # calculate mean
    mean(my_data[[pollutant]], na.rm = TRUE)
}