setwd("/Users/sung/Desktop/UCD/STA250/HW1")

Delay = function(path){
  data = read.table(path)
  data = data[-c(nrow(data), nrow(data)-1),]
  # delete the NA row and the row with the characters "ArrivalDelay"
  data[,2] = as.numeric(as.character(data[,2]))
  data = data[order(data[,2]),]
  Delay_median = function(data)
  {
    x = 0; y = 1
    while(x < sum(data[,1])/2) {
      x = x + data[y,1]
      y = y + 1
    }
    if (x == sum(data[,1])/2)
      return ((data[y,2]+data[y+1,2])/2)
    else
      return (data[y,2])
  }
  
  sum(data[,1])
  # number of observations = 68535532
  mu = sum(data[,1]*data[,2])/sum(data[,1])
  # mean = 7.220496
  med = Delay_median(data)
  # median = 2
  std = sqrt(sum(data[,1]*(data[,2] - mu)^2) / sum(data[,1]))
  # 27.37275
  result = list(time = time, results = c(mean = mu, median = med, sd = std),
       system = Sys.info(),  session = sessionInfo())
  return (result)
}
Delay("./data/FreqTable.txt")

system.time(a<-Delay("./data/FreqTable.txt"))

