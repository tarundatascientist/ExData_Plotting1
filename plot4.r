

generatePlot4 <- function(){
        df4<-readFileIntoRMemory();
        
        par(mfrow = c(2,2));
        
        generateGlobalActivePower(df4);
        generateVoltagePlot(df4);
        generateSubmeterPlot(df4);
        generateGlobalReactivePowerPlot(df4);
        savePlot()
}

readFileIntoRMemory<-function(){
        columnClass <- c( "character" , "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric");
        df3 <- read.table("./household_power_consumption.txt", sep=";", header = TRUE, colClasses = columnClass, na.strings = "?");
        df3[[1]] <- as.Date(df3[[1]], "%d/%m/%Y");
        df4 <- df3 [ (df3[[1]] =="2007-02-01") | (df3[[1]] =="2007-02-02"), ];
        return(df4);
}

generateGlobalActivePower <- function(df4){
        global_active_power <- df4[[3]];
        #print(global_active_power);
        dt <- paste(df4[[1]], df4[[2]]);
        dt <- strptime(dt, format="%Y-%m-%d %H:%M:%S");
        #print(dt)
        plot(dt, global_active_power, pch = ".", ylab = "Global Active Power", xlab = "", type="l")
}

generateSubmeterPlot <- function(df4){
        subMetering1 <- df4[[7]];
        subMetering2 <- df4[[8]];
        subMetering3 <- df4[[9]];
        plot(dt, subMetering1, pch=".", type="l", xlab = "", ylab = "Energy sub metering", yaxt = "n");
        axis(2, at = seq(0, 30, by=10), las=1);
        lines(dt, subMetering2, col="red");
        lines(dt, subMetering3, col="blue");
        legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty= c(1,1,1) , col= c("black", "red", "blue") , cex = 0.4);
        
}

savePlot <- function(){
        dev.copy(png, file="plott3.png")
        dev.off()
        
        png(filename = "plott3.png", width = 480, height = 480);
        dev.off();
}

generateVoltagePlot<- function(df4){
        voltage <- df4[[5]]
        plot(dt, voltage, type="l", xlab = "datetime", ylab = "Voltage", yaxt = "n")
        axis(2, at = seq(234, 246, by=4), las=1);
}

generateGlobalReactivePowerPlot <- function(df4){
        Global_reactive_power <- df4[[4]]
        plot(dt, Global_reactive_power, type="l", xlab = "datetime", yaxt = "n")
        axis(2, at = seq(0.0,0.5, by=0.1), las=2)
}