

generatePlot2 <- function(){
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
        global_active_power <- df4[[3]];
        #print(global_active_power);
        dt <- paste(df4[[1]], df4[[2]]);
        dt <- strptime(dt, format="%Y-%m-%d %H:%M:%S");
        #print(dt)
        plot(dt, global_active_power, pch = ".", ylab = "Global Active Power (killowatts)", xlab = "", type="l")
        dev.copy(png, file="plot2.png");
        dev.off();
        dev.copy(png, file="plot2.png");
        png(filename = "plot2.png", width = 480, height = 480);
        dev.off();
}