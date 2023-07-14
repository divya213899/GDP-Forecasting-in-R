mat_dat <- list() 

combinationutil <- function(arr,n,r,index,dat,i,mat){
  if(index>r){
    return(print(dat))
  }
  if(i>n){
    return()
  }
    dat[index]=arr[i]
    combinationutil(arr,n,r,index+1,dat,i+1,mat)
    combinationutil(arr,n,r,index,dat,i+1,mat)
  
}
dat <- numeric(length = 4)
mat <- list()
combinationutil(c(1:13),13,4,1,dat,1,mat)
combin <- printcomb(c(1:13),13,4)
mat <- matrix(data =NA,nrow = 715,ncol = 4,byrow = TRUE)

