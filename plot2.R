
# data from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# subseted with `egrep ^\[12\]/2/2007 household_power_consumption.txt  > subsetted_data.txt`
# to obtain only data from 1/2/2007 - 2/2/2007


loadData<-function(filename){
  data<-read.table(filename,sep=";",stringsAsFactors=F,header=T)
  dateTime<-paste(data$Date,data$Time)
  data$DateTime<-strptime(dateTime,format="%d/%m/%Y %H:%M:%S")
  
  data
}

save_plot2<-function(filename='data/subsetted_data.txt'){
  png(filename="plot2.png",width=480,height=480)
  data<-loadData(filename)
  l<-dim(data)[1]
  
  plot(data$Global_active_power,type="l",ylab="Global Active Power (kilowatts)",xaxp=c(1,l,2),labels=F,xlab="")
  at<-c(1,as.integer(l/2),l)
  # create labels, +60 ensures we get the right rollover at the end.
  labels<-weekdays(data$DateTime[at]+60,abbreviate=T)
  axis(1, labels=labels,at=at)
  axis(2, labels=c(0,2,4,6),at=c(0,2,4,6))
  
  
  dev.off()
}