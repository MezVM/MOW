install.packages("Rstem");

str.join <- function(a, b) {
  return(paste(a, b, sep=''));
}

data.in <- 'D:/MOW/tmp/data_preprocessed/';
data.out <- 'D:/MOW/tmp/df/';
dir.create(data.out, recursive=TRUE);
file.create(str.join(data.out,"df.csv"))

source("bag_of_words.R");
create.articles.data.frame(data.in,str.join(data.out,"df.csv"))
