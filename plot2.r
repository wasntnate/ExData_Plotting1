# Generate the second plot
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