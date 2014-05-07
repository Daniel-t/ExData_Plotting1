
# data from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# subseted with `egrep ^\[12\]/2/2007 household_power_consumption.txt  > subsetted_data.txt`
# to obtain only data from 1/2/2007 - 2/2/2007


loadData<-function(filename){
  data<-read.table(filename,sep=";",stringsAsFactors=F,header=T)
  dateTime<-paste(data$Date,data$Time)
  data$DateTime<-strptime(dateTime,format="%d/%m/%Y %H:%M:%S")
  
  data
}

save_plot3<-function(filename='data/subsetted_data.txt'){
  png(filename="plot3.png",width=480,height=480)
  data<-loadData(filename)
  l<-dim(data)[1]
  
  plot(data$Sub_metering_1,type="l",ylab="Energy sub metering",xaxp=c(1,l,2),labels=F,xlab="",col="black")
  points(data$Sub_metering_2,type="l",col="red")
  points(data$Sub_metering_3,type="l",col="blue")
  
  at<-c(1,as.integer(l/2),l)
  # create labels, +60 ensures we get the right rollover at the end.
  labels<-weekdays(data$DateTime[at]+60,abbreviate=T)
  axis(1, labels=labels,at=at)
  axis(2, labels=seq(0,30,10),at=seq(0,30,10))
  legend(legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),x="topright",lty=1,col=c("black","red","blue"))
  
  dev.off()
}