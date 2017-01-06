

getClasses <- function(train_classes) {
  NR <- nrow(train_classes);
  classes <- colnames(train_classes)
  classes_list <- list();
  for (iterClass in classes) {
    cat(sprintf('\t KLASA: %s \n', iterClass));
    flush(stdout());
    class_positions = c();
    for (r in 1:NR) {
      if(train_classes[r,iterClass] == 1) {
        class_positions <- c(class_positions,r);
      }
    }
    classes_list <- append(classes_list,list(class_positions));
  }
  return(list(classes = classes, positions = classes_list));
}



tfIDF <- function(train_data,train_classes,test_data) {
  NC <- ncol(train_data);
  D <- nrow(train_data);
  classes_list <- getClasses(train_classes);
  NCl <- length(classes_list$classes);
  
  all_vectors = list();
  for (cl in 1:NCl) { # dla kazdej klasy
    wektor_klasy <- c();
    cat(sprintf('\nKLASA: %s \n', classes_list$classes[cl]));
    flush(stdout());
    for (c in 1:NC) { # dla kazdego slowa
      cat(sprintf('\tSlowo: %d / %d\r', c, NC));
      flush(stdout());
      DF <- length(train_data[train_data[,c]>0,c])
      IDF <- log10(D/DF);
      TF_class = sum(train_data[classes_list$positions[[cl]],c]);
      TFIDF <- TF_class*IDF;
      if(is.na(TFIDF)){
        wektor_klasy <- c(wektor_klasy,0);
      }
      else{
        wektor_klasy <- c(wektor_klasy,TFIDF);
      }
    } # wszystkkie slowa w klasie
    all_vectors <- append(all_vectors,list(wektor_klasy));
  }
  
  # faza Klasyfikacji danych testowych:
  
  clasification_list = c();
  cat(sprintf('\n\n'));
  flush(stdout());
  for(row_index in 1:nrow(test_data)) {
    cat(sprintf('\tKlasyfikacja: %d / %d\r', row_index, nrow(test_data)));
    flush(stdout());
    max_class_index = 0;
    max_class_similarity = 0;
    for(class_vector_index in 1:length(all_vectors)){
      v1 <- all_vectors[[class_vector_index]];
      v2 <- as.numeric(test_data[row_index,]);
      cosine.similarity <- (sum(v1 * v2)) / (norm(v1, type="2") * norm(v2, type="2"));
      if(cosine.similarity > max_class_similarity){
        max_class_similarity = cosine.similarity;
        max_class_index = class_vector_index;
      }
    }
    clasification_list <- c(clasification_list,max_class_index);
  }
  return(clasification_list)
}