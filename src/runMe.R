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

