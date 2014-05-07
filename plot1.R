
# data from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
# subseted with `egrep ^\[12\]/2/2007 household_power_consumption.txt  > subsetted_data.txt`
# to obtain only data from 1/2/2007 - 2/2/2007


loadData<-function(filename){
  data<-read.table(filename,sep=";",stringsAsFactors=F,header=T)
  dateTime<-paste(data$Date,data$Time)
  data$DateTime<-strptime(dateTime,format="%d/%m/%Y %H:%M:%S")
  
  data
}

save_plot1<-function(filename='data/subsetted_data.txt'){
  data<-loadData(filename)
  png(filename="plot1.png",width=480,height=480)
  hist(data$Global_active_power,col="red",xlab="Global Active Power (kilowatts)",main="Global Active Power")
  dev.off()
}