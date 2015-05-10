

generatePlot1 <- function(){
        
        
        df4 <- readFileIntoRMemory();
        generatePlot(df4);
}

readFileIntoRMemory<-function(){
        columnClass <- c( "character" , "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric");
        df3 <- read.table("./household_power_consumption.txt", sep=";", header = TRUE, colClasses = columnClass, na.strings = "?");
        df3[[1]] <- as.Date(df3[[1]], "%d/%m/%Y");
        df4 <- df3 [ (df3[[1]] =="2007-02-01") | (df3[[1]] =="2007-02-02"), ];
        return(df4);
}

generatePlot <- function(df4){
        par(mfrow = c(1,1));
        global_active_power <- df4[[3]];
        hist(global_active_power, col= "RED", xlab = "Global Active Power (kilowatts)", main = "Global Active Power");
        dev.copy(png, file="plot1.png");
        dev.off();
        dev.copy(png, file="plot1.png");
        png(filename = "plot1.png", width = 480, height = 480);
        dev.off();
}