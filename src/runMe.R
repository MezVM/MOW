install.packages("Rstem");

str.join <- function(a, b) {
  return(paste(a, b, sep=''));
}

generateSolutionList <- function(test.cls) {
  solution_list = c();
  col_names = colnames(test.cls);
  for (row in 1:nrow(test.cls)){
    for (col in 1:ncol(test.cls)){
      if (test.cls[row,col] > 0) {
        solution_list = c(solution_list,col_names[col]);
        break;
      }
    }
  }
  return(solution_list);  
}




data.in <- 'D:/MOW/tmp/data_preprocessed/';
data.out <- 'C:/MOW/tmp/df/';
data.df.name <- "df2.csv";
data.df.name.bin <- "df2.bin";
data.df.name.opt <- "df2opt.bin";
dir.create(data.out, recursive=TRUE);
file.create(str.join(data.out,data.df.name))

source("bag_of_words.R");
create.articles.data.frame(data.in,str.join(data.out,data.df.name))

myTable <- read.table(str.join(data.out,data.df.name),header = TRUE)
save(myTable, file=str.join(data.out,data.df.name.bin), compress=FALSE);

min.n=20;
min.w=20;
source("data_strip.R")
optimalised.df <- vectorize.data(df = myTable, min.n = min.n, min.w = min.w);


source("TFIDF.R")
test <- sample(c(0,1),nrow(optimalised.df$data),replace = TRUE,prob=c(0.9, 0.1));
test.data <- optimalised.df$data[test>0,];
test.cls  <- optimalised.df$org.classes[test>0,];
solution_list <- generateSolutionList(test.cls);
row.names(test.data) <- NULL;
learn.data <- optimalised.df$data[test==0,];
learn.cls  <- optimalised.df$org.classes[test==0,];
row.names(learn.data) <- NULL;
newClasses <- tfIDF(learn.data,learn.cls,test.data);
tfidf_table <- table(solution_list,newClasses);


source("knn2.R");
source("knn.R");
test <- sample(c(0,1),nrow(optimalised.df$data),replace = TRUE,prob=c(0.9, 0.1));
test.data <- optimalised.df$data[test>0,];
test.cls  <- optimalised.df$org.classes[test>0,];
solution_list <- generateSolutionList(test.cls);
row.names(test.data) <- NULL;
learn.data <- optimalised.df$data[test==0,];
learn.cls  <- optimalised.df$org.classes[test==0,];
learn.cls <- generateSolutionList(learn.cls);
row.names(learn.data) <- NULL;
metric <- metric.euclid;
result <- knn2(learn.data, as.factor(learn.cls), test.data, metric, k=10)
# algorytmy zwraca wektor kategorii
# dla danych z test.data
table(result, solution_list)


source("naive_bayes.R")
test <- sample(c(0,1),nrow(optimalised.df$data),replace = TRUE,prob=c(0.9, 0.1));
test.data <- optimalised.df$data[test>0,];
test.data <- make.df.binary(test.data)
test.cls  <- optimalised.df$org.classes[test>0,];
solution_list <- generateSolutionList(test.cls);
row.names(test.data) <- NULL;
learn.data <- optimalised.df$data[test==0,];
learn.data <- make.df.binary(learn.data)
learn.cls  <- optimalised.df$org.classes[test==0,];
learn.cls <- generateSolutionList(learn.cls);
row.names(learn.data) <- NULL;
# ramka powinna zawierac krotnosci wystapien
# slow
model <- train.naive.bayes(learn.data, as.factor(learn.cls), -1)
p <- predict.mcNb(model, test.data, as.factor(solution_list));
table(p, solution_list)



source("statistics.R")
correct(solution_list,newClasses);
stat.df <- getStatisticDF(solution_list,newClasses,colnames(optimalised.df$org.classes))
stat.df
