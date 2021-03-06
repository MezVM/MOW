# Grupa funkcji określająca jakość klasyfikacji

# funkcja wyliczajaca ogolna poprawnosc klasyfikacji
correct <- function(test.data, predict.data) {
  return(mean(test.data == predict.data));
}

#funkcja wyliczajaca recall klasyfikacji dla danej klasy
recall <- function(test.data, predict.data, class_name) {
  tmp <- mean(test.data[test.data == class_name] == predict.data[test.data == class_name]);
  return(tmp);
}

# fukcja wyliczajaca precyzje klasyfikacji dla danej klasy
precision <- function(test.data, predict.data, class_name) {
  a<-length(predict.data[test.data == class_name & predict.data == class_name]);
  b<-length(test.data[test.data == class_name]);
  c<-length(predict.data[predict.data == class_name & test.data != class_name]);
  return(a/(b+c));
}

# generowuje zbiorczego data_frame z parametrami recall i precision
# dla wszystkich klas
getStatisticDF <- function(test.data, predict.data, classes_list) {
  new.df <- data.frame();
  for (class_name in classes_list){
    p <- precision(test.data, predict.data,class_name);
    r <- recall(test.data, predict.data,class_name);
    tmp.df<-data.frame(class_name,p,r);
    names(tmp.df)<-c("class","precision","recall");
    new.df <- rbind(new.df,tmp.df)
  }
  return(new.df)
}