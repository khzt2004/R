# https://www.r-bloggers.com/zalandos-images-classification-using-h2o-with-r-2/

library(h2o)

h2o.init(ip = "localhost",
         port = 54321,
         nthreads = -1,
         min_mem_size = "20g")

fmnist_train <- h2o.importFile(path = "data/fashion-mnist_train.csv", 
                               destination_frame = "fmnist_train",
                               col.types=c("factor", rep("int", 784)))

fmnist_test <- h2o.importFile(path = "data/fashion-mnist_test.csv",
                              destination_frame = "fmnist_test",
                              col.types=c("factor", rep("int", 784)))