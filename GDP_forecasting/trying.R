test_data <- read.csv("test_data.csv")

test_data <- t(test_data)
rownames(test_data) <- NULL
# making first row, column heading
colnames(test_data) <- test_data[1,]
test_data <- test_data[-1,]
test_data <- cbind(test_data,"c6" = test_data[,5])
test_data <- test_data[,-5]
test_data <- as.data.frame(test_data)
test_data <- round(test_data,3)
#test_data <- test_data[-32,]
mu_test <- matrix(rep(c(2.833818e+01 ,2.414240e+10, 2.984091e+01, 9.530591e+01 ,1.379318e+01,8.035000e+00),31),ncol = 6,nrow = 31,byrow = TRUE)

sd_test <- matrix(rep(c(1.911458e+00,1.694725e+10 ,8.246780e+00, 8.592076e+00 ,6.320614e+00 ,4.788851e-01 ),31),ncol = 6,nrow = 31,byrow = TRUE)
test_dt <- (test_data - mu_test)/sd_test
test_data <- cbind("x"= index[2:33],test_data)
colnames(test_data) <- c("x","c1","c2","c3","c4","c5","c6")

write.csv(test_data,"final_test.csv")
