library(lattice);
library(ggplot2);

# Load the dataset
load.data = function(){
  households = read.csv("household_power_consumption.txt",sep=";", colClasses="character");
  
  # Select Feb 1st
  hsubset1 = households[households$Date == "1/2/2007",];
  hsubset1$DayOfWeek = rep("01-Thur",length(hsubset1));
  
  # Select Feb 2nd
  hsubset2 = households[households$Date == "2/2/2007",];
  hsubset2$DayOfWeek = rep("02-Fri",length(hsubset2));
  
  # Join the two data sets
  hsub = rbind(hsubset1, hsubset2);
  
  # Convert DayOfWeek to Factor
  hsub$DayOfWeek = factor(hsub$DayOfWeek);
  
  # Convert Global Active Power to numeric
  hsub$gap = as.numeric(hsub$Global_active_power);
  
  # Convert time to minutes
  hsub$minutes = as.numeric(substr(hsub$Time,0,2))*60+as.numeric(substr(hsub$Time,4,5));
  
  # Calculate the Offsets
  hsub$offset =  hsub$minutes + ((as.numeric(hsub$DayOfWeek)-1) * 1440);
  
  # Convert Sub Metering to numeric
  hsub$Sub_metering_1 = as.numeric(hsub$Sub_metering_1);
  hsub$Sub_metering_2 = as.numeric(hsub$Sub_metering_2);
  hsub$Sub_metering_3 = as.numeric(hsub$Sub_metering_3);
  
  return(hsub);
}


plot1 <- function(hsub, filename=NULL, ...){  
  p=hist(hsub$gap, breaks=11, 
       main="Global Active Power", 
       col='red', 
       xlab="Global Active Power (kilowatts)");
  
  # Save to file is requested
  if(!is.null(filename)){
    png(filename=filename, ...);
    p;
    dev.off();
  }
  
  return(p);
}

plot2 <- function(hsub, filename=NULL, ...){
  tf = transform(hsub, DOW=factor(DayOfWeek))
  p = xyplot(gap ~ minutes | DOW, tf, type="s", xlab="Day",
         ylab="Global Active Power (kilowatts)")
  
  # Save to file is requested
  if(!is.null(filename)){
    png(filename=filename, ...);
    p;
    dev.off();
  }
}

plot3 <- function(hsub, filename=NULL, ...){
  p = ggplot(hsub, aes(offset, Sub_metering_1)) + 
    geom_step(color='brown') +
    geom_step(data=hsub, aes(offset, Sub_metering_2), color='red') + 
    geom_step(data=hsub, aes(offset, Sub_metering_3), color="blue") + 
    theme_classic() + labs(x="Time", y="Energy sub metering");  
  
  # Save to file is requested
  if(!is.null(filename)){
    png(filename=filename, ...);
    p;
    dev.off();
  }
}

plot4 <- function(hsub){
  with(hsub, {
    xyplot(gap ~ minutes | DOW, tf, type="s", xlab="Day", ylab="Global Active Power (kilowatts)")
    hist(hsub$gap, breaks=11, 
         main="Global Active Power", 
         col='red', 
         xlab="Global Active Power (kilowatts)")        
  });
}