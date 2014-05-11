# Generate the fourth plot
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