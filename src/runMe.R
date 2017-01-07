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



# Tworzymy plik z oryginalna, nieoptymalizowana tabela danych
data.in <- 'D:/MOW/tmp/data_preprocessed/';
data.out <- 'C:/MOW/tmp/df/';
data.df.name <- "df.csv";
data.df.name.bin <- "df.bin";
data.df.name.opt <- "dfopt.bin";
dir.create(data.out, recursive=TRUE);
file.create(str.join(data.out,data.df.name))
source("bag_of_words.R");
create.articles.data.frame(data.in,str.join(data.out,data.df.name))

# Wczytuyjemy istniejacy plik do pamieci i przeprowadzamy jego optymalizacje
# zakłądamy ze klasa musi wystapic minimum 20x aby ja rozważać
# a słowo musi wystąpić w conajmniej 2% artykolow.
myTable <- read.table(str.join(data.out,data.df.name),header = TRUE)
min.n=20;
min.w=nrow(myTable)*0.02;
source("data_optimaliser.R")
optimalised.df <- vectorize.data(df = myTable, min.n = min.n, min.w = min.w);

# określa podział zbirów testujacych i trenujących. Z lewej procentowy rozmiar zbioru treujacego
# z prawej zbioru testujacego
probability = c(0.8, 0.2);
test <- sample(c(0,1),nrow(optimalised.df$data),replace = TRUE,prob=probability);
test.data <- optimalised.df$data[test>0,];
test.cls  <- optimalised.df$org.classes[test>0,];
oryginal_solution <- generateSolutionList(test.cls);
row.names(test.data) <- NULL;
learn.data <- optimalised.df$data[test==0,];
learn.cls  <- optimalised.df$org.classes[test==0,];
learn.cls.2 <- optimalised.df$org.classes[test==0,];
learn.cls <- generateSolutionList(learn.cls);
row.names(learn.data) <- NULL;


### Własne implementacje Algorytmów ###

# algorytm TF_IDF
source("TFIDF.R")
# proba algorytmu TF_IDF
tfidf_result <- tfIDF(learn.data,learn.cls.2,test.data);
tfidf_table <- table(oryginal_solution,tfidf_result);


# algorytm KNN
source("knn2.R");

# proba algorytmu KNN dla roznych parametrow
metric <- metric.euclid;
knn_result <- knn2(learn.data, as.factor(learn.cls), test.data, metric, k=10)
table(oryginal_solution,knn_result)

metric <- metric.euclid;
knn_result2 <- knn2(learn.data, as.factor(learn.cls), test.data, metric, k=100)
table(oryginal_solution,knn_result2)

metric <- metric.cos;
knn_result3 <- knn2(learn.data, as.factor(learn.cls), test.data, metric, k=10)
table(oryginal_solution,knn_result3)

# algorytm Naive Bayes
source("naive_bayes.R")
# proba algorytmu NB w zalezności od parametru normalizacji
model <- train.naive.bayes(learn.data, as.factor(learn.cls), -1)
nb_result <- predict.mcNb(model, test.data, as.factor(oryginal_solution));
table(oryginal_solution, nb_result);

model <- train.naive.bayes(learn.data, as.factor(learn.cls), 0.5)
nb_result2 <- predict.mcNb(model, test.data, as.factor(oryginal_solution));
table(oryginal_solution, nb_result2);

model <- train.naive.bayes(learn.data, as.factor(learn.cls), 1)
nb_result3 <- predict.mcNb(model, test.data, as.factor(oryginal_solution));
table(oryginal_solution, nb_result3);


###  Oryginalne Algorytmy ###

# algorytm Naive Bayes
install.packages("e1071", dep = TRUE);
library(e1071);
data <- learn.data;
tmp <- colnames(learn.data);
data$class <- as.factor(learn.cls);
formula.string <- paste(tmp, collapse='+');
formula.string <- paste('class~', formula.string);
f <- formula(formula.string);
nbClasif <- naiveBayes(f, data=data, laplace = 0)
nb.result = predict(nbClasif,test.data)
table(oryginal_solution,as.vector(nb.result));
correct(oryginal_solution,as.vector(nb.result));

# algorytm KNN
library("class")

knn.result <- knn(learn.data, test.data, as.factor(learn.cls), k=10, l=0)
table(oryginal_solution,as.vector(knn.result));



# Statystyki eksperymentow
source("statistics.R")
correct(oryginal_solution,tfidf_result);
stat.df <- getStatisticDF(oryginal_solution,tfidf_result,colnames(optimalised.df$org.classes))
stat.df

correct(oryginal_solution,knn_result);
stat.df <- getStatisticDF(oryginal_solution, knn_result, colnames(optimalised.df$org.classes))
stat.df
correct(oryginal_solution,knn_result2);
stat.df <- getStatisticDF(oryginal_solution, knn_result2, colnames(optimalised.df$org.classes))
stat.df
correct(oryginal_solution,knn_result3);
stat.df <- getStatisticDF(oryginal_solution, knn_result3, colnames(optimalised.df$org.classes))
stat.df

correct(oryginal_solution, nb_result);
stat.df <- getStatisticDF(oryginal_solution, nb_result, colnames(optimalised.df$org.classes))
stat.df
correct(oryginal_solution, nb_result2);
stat.df <- getStatisticDF(oryginal_solution, nb_result2, colnames(optimalised.df$org.classes))
stat.df
correct(oryginal_solution, nb_result3);
stat.df <- getStatisticDF(oryginal_solution, nb_result3, colnames(optimalised.df$org.classes))
stat.df


correct(oryginal_solution,as.vector(nb.result));
stat.df <- getStatisticDF(oryginal_solution, as.vector(nb.result), colnames(optimalised.df$org.classes))
stat.df
correct(oryginal_solution,as.vector(knn.result));
stat.df <- getStatisticDF(oryginal_solution, as.vector(knn.result), colnames(optimalised.df$org.classes))
stat.df
