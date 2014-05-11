# Generate the first plot
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