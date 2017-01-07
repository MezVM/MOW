#
# Plik zawiera funkcje odpowiedzialne za 
# odrzucenie czesci kategorii, oraz wektoryzacje
# kategorii. 



is.column.name <- function(s) {
	return(length( grep('^c\\.', s) ) > 0);
}


is.invalid <- function(column.name, invalid.categories.hash) {
	return(exists(column.name, envir=invalid.categories.hash, inherits=FALSE));
}



# min.n - minimalna liczba wystapien danej kategorii
# invalid.cats.hash - nazwy kategori ktore powinny zostac
#  	odrzucone
#
# RETURN wektor logiczny l taki ze l[nr.kolumny] == T jezeli
# dana kategoria powinna wystepowac w ostatecznych danych
#
vectorizeCategories <- function(text.data.frame, min.n, invalid.cats.hash) {
	column.names <- colnames(text.data.frame);
	result       <- logical( ncol(text.data.frame) );

	for(i in 1:ncol(text.data.frame)) {
		column.name <- column.names[i];
		if(is.column.name(column.name) && !is.invalid(column.name, invalid.cats.hash)) {
			number.of.arts <- text.data.frame[1, i];
			if(number.of.arts >= min.n) {
				result[i] = TRUE;
			}
		}
	}

	return(result);
}


create.hash <- function(str.vector) {
	e <- new.env(hash=TRUE, parent=emptyenv());
	
	for(s in str.vector) {
		assign(s, TRUE, envir=e);
	}

	return(e);
}


# funkcja zwraca wektor indeksow kolumn nalezacych
# do klasy cs
# cs = 'c' dla kategori, cs = 'w' dla slow oraz
# cs = 'meta' dla informacji dodatkowych
#
getDfIndexes <- function(df, cs) {
	column.names  <- colnames(df);
	class.columns <- grep(paste("^",cs,"\\.",sep=''), column.names);

	return( (1:ncol(df)) [class.columns] );
}


# Funkcja optymalizuje ramke danych
#
# min.n - minimalna liczba wystapien danej kategorii
# invalid.cats.hash - nazwy kategori ktore powinny zostac odrzucone
# min.w - minimalna liczba wystapien danego slowa
# 
# RETURN lista zawierajaca
#   $data - dane
#   $rows.indexes - indexy poszczegolnych klas
#   $org.classes - df klas
#   $optimalized.df  - wspolna zoptymalizowana ramka klas i slow
#
# format ramki danych (nazwy kolumn)
# w. w. w. ... w. c. ... c. meta. ... meta.
#
vectorize.data <- function(df, min.n=500, invalid.cats.hash=emptyenv(), min.w=2) {
	cats.names    <- colnames(df);
	cats.indexes  <- getDfIndexes(df, 'c');
	allow.in.data <- vectorizeCategories(df, min.n, invalid.cats.hash);
	max.words.index <- min(cats.indexes) - 1;
	rows.vector <- integer(0);
	cats.vector <- character(0);
	N = nrow(df);
	for(i in 2:N){
		cat(sprintf('I Faza: Wiersz %d / %d\r', i, N));
		flush(stdout());
		for(ci in cats.indexes) {
			if(allow.in.data[ci] && df[i, ci] > 0) {
				rows.vector <- c(rows.vector, i);
				cats.vector <- c(cats.vector, cats.names[ci]);
			}
		}
	}

	# Usuwamy te slow ktore wystepuja w zbyt malej liczbie
	#
	words.count  <- integer(max.words.index);
	for(wi in 1:max.words.index) {
		cat(sprintf('II Faza: Kolumna %d / %d\t\t\t\r', wi, max.words.index));
		flush(stdout());

		words.count[wi] = sum(df[rows.vector,wi]);
	}
	words <- (1:max.words.index)[words.count >= min.w];
	cat('\n');

	used.columns <- which(allow.in.data == TRUE);

	return(list(data=df[rows.vector, words],
			  rows.indexes=rows.vector,
			  org.classes=df[rows.vector,used.columns],
			  optimalized.df=df[rows.vector,c(words,used.columns)]));
}


# funkcja zamienia ramke danych zawierajaca liczby
# na ramke zawierajaco 0 i 1 (dla liczb >= 0)
#
make.df.binary <- function(df) {
	for(i in 1:ncol(df)) {
		df[,i] <- sign(df[,i]);
	}
	return(df);
}





