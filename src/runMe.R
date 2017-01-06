install.packages("Rstem");

str.join <- function(a, b) {
  return(paste(a, b, sep=''));
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
save(optimalised.df, file=str.join(data.out,data.df.name.opt), compress=FALSE);
optimalised.df$data



sample(c(0,1), 100, replace = TRUE)

test <- sample(c(0,1),nrow(optimalised.df$data),replace = TRUE,prob=c(0.9, 0.1))
test.data <- optimalised.df$data[test>0,]
test.cls  <- optimalised.df$org.classes[test>0,]

solution_list = c();
col_names = colnames(test.cls)
for (row in 1:nrow(test.cls)){
  for (col in 1:ncol(test.cls)){
    if (test.cls[row,col] > 0) {
      solution_list = c(solution_list,col_names[col])
    }
  }
}
solution_list


row.names(test.data) <- NULL
learn.data <- optimalised.df$data[test==0,]
learn.cls  <- optimalised.df$org.classes[test==0,]
row.names(learn.data) <- NULL

source("TFIDF.R")
newClasses <- tfIDF(learn.data,learn.cls,test.data);
table(solution_list,newClasses);
