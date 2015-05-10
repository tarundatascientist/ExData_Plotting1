

generatePlot3 <- function(){
        df4<-readFileIntoRMemory();
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
        
        subMetering1 <- df4[[7]];
        subMetering2 <- df4[[8]];
        subMetering3 <- df4[[9]];
        plot(dt, subMetering1, pch=".", type="l", xlab = "", ylab = "Energy sub metering", yaxt = "n");
        axis(2, at = seq(0, 30, by=10), las=1);
        lines(dt, subMetering2, col="red");
        lines(dt, subMetering3, col="blue");
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty= c(1,1,1) , col= c("black", "red", "blue") , cex = 0.4);
        dev.copy(png, file="plot3.png")
        dev.off()
        
        png(filename = "plot3.png", width = 480, height = 480);
        dev.off();
}