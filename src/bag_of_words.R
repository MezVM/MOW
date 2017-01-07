# Funkcje tu zawarte odpowiadaja za utworzenie tabeli, wktora
# nastepnie jest zapisywana do wskazanego pliku
# tabela moze byc duza wiec prosi sie o cierpliwosc (~500mb)
# kolumny tabeli to slowa z newsow, kategorie i metadane statystyczne
# w.*   c.*   m.*


# zeby uchronic sie od wplywu jezyka na sposob sortowania napisow
# ustawiamy locale na C
Sys.setlocale(category="LC_ALL", locale="C");

library('Rstem');

# funkcja wczytuje i zwraca wszystkie linie
# z pliku tekstowego o podanej nazwie
#
read.all.lines <- function(file.name) {
	handle <- file(file.name, "rt");
	lines  <- readLines(handle);
	close(handle);

	return(lines);
}


# funkcja zwraca wektor kategorii
# na podstawie lancucha znakow postaci
# "class1, class2, class3"
#
get.article.categories <- function(category.line) {
	category.vector <- strsplit(category.line, "[ \t]*,[ \t]*");
	return( category.vector[[1]] );
}


# funkcja odpowiada za wczytanie wstepnie przetworzonego
# pliku newsa(opis przetworzonego pliku patrz: preprocess.r)
# news zwracany jest w postaci listy zawierajacej pola
# $abstract - cialo, oraz $classes - wektor klas.
# w przypadku bledu zwracana jest wartosc NA
#
read.preprocessed.article <- function(file.name) {
	lines <- read.all.lines(file.name);
	if( length(lines) != 3 ) {
		stop(paste('invalid article file:', file.name));
	}

	categories <- get.article.categories(lines[1]);
	abstract   <- lines[3];

	return(list(abstract=abstract, categories=categories));
}


# funkcja odpowiada za wykonanie opisanego w
# dokumentacji wstepenej przetwarzania tekstu
# na ktore sklada sie:
# * zamiana duzych liter na male 
# * usniecie znakow nie bedacych literami
# * dokonanie stemmingu algorytmem Portera
# funkcja zwraca wektor slow
# 
stem.abstract.text <- function(abstract.text) {
	abstract.text <- gsub("[^A-Za-z]+", " ", abstract.text);
	abstract.text <- tolower(abstract.text);

	# dzielimy tekst na poszczegolne slowa
	# wynik jest zwracany w postaci listy wektorow
	abstract.text <- strsplit(abstract.text, "[ \t]+");
	abstract.text <- abstract.text[[1]];
	
	# stemujemy poszczegolne slowa
	# wordStem obsluguje slowa do dlugosci 255
	# dluzsze usuwamy, dodatkowo podczas splitu
	# moga pojawic sie napis o 0 dlugosci
	MAX.WORD.LENGTH <- 255;
	abstract.text   <- abstract.text[
		nchar(abstract.text) < MAX.WORD.LENGTH &  # NIE MOZE BYC &&
		nchar(abstract.text) > 3
	];

	return(wordStem(abstract.text, language="english"));
}

# --------------------------------------------------
# ladowanie pliku do obslugi stopwords
# --------------------------------------------------
source('stopwords.R');

init.stop.words.dictionary();
# --------------------------------------------------


# funkcja usuwa z angielskiego tekstu znaki bedace
# tzw. stop words a wiec np. 'the', 'you'
#
# tekst jest reprezentowany jako wektor slow
# np. remove.stop.words(c("one", "two", "three"))
#
remove.stop.words <- function(text) {
	return( text[!is.stop.word(text)] );
}



# funkcja wczytuje artykul i zwraca 
# liste zawierajaca $abstract - wektor
# slow z ciala poddanych stemmingowi oraz
# usuwaniu stopwords, $categories - wektor napisow
# okreslajacych poszczegolne kategorie newsa
#
load.article <- function(file.name) {
	art <- read.preprocessed.article(file.name);

	abstract <- stem.abstract.text(art$abstract);
	abstract <- remove.stop.words(abstract);

	categories <- art$categories;

	return( list(abstract=abstract, categories=categories) );
}

# funkcja dodaje slowa z wektora words do slownika
# (environment) env
# przy okazji env[word] zawiera liczbe wystapien danego
# slowa w przetwarzanych teskstach
#
insertWords <- function(env, words) {
	for(w in words) {
		if(!exists(w, envir=env)) {
			assign(w, 1, envir=env);
		}
		else {
			assign(w, 1+get(w, envir=env), envir=env);
		}
	}
}





# funkcja przebiega po wszystkich plikach wstepnie 
# przetworzonych artykulow i buduje liste uzywanych 
# w artach slow oraz kategorii. podane listy zawieraja
# slowa i kategorie ze wszystkich przejrzanych plikow.
# funkcja zwraca liste z atrybutami $words - slownik uzywanych
# slow, $categories - slownik uzywanych kategorii
# $files.count - liczba przetworzonych plikow
# 
computeGlobalStats <- function(articles.directory) {
	words 	 <- new.env(hash=TRUE, parent=emptyenv());
	categories <- new.env(hash=TRUE, parent=emptyenv());
	count 	 <- 0;

	articles <- list.files(
		path=articles.directory, 
		pattern="*",
		full.names=TRUE,
		ignore.case=TRUE
		);

	for(a in articles) {
		cat(sprintf("[%s]\n", a));
		tmp <- load.article(a);

		insertWords(words, tmp$abstract);
		insertWords(categories, tmp$categories);
		count <- count + 1;
	}

	return( list(count=count, words=words, categories=categories) );
}

# META COLUMNS - dodatkowe kolumny ktore dodajemy do
# data frame'u
META.COLUMNS <- c('filename', 'wordcount');


# funkcja zapisuje do pliku df.file nazwy kolumn
# dataframe'u na podstawie art.stats 
#
addDfHeader <- function(df.file, art.stats) {
	word.col.names <- paste('w.', sort(ls(envir=art.stats$words)), sep='');
	cats.col.names <- paste('c.', sort(ls(envir=art.stats$categories)), sep='');

	meta.col.names <- paste('meta.', META.COLUMNS, sep='');

	cat( c(word.col.names, cats.col.names, meta.col.names), 
		file=df.file, sep='\t' );
	cat( '\n', file=df.file, sep='' );
}


writeEnvCols <- function(envir, file) {
	col.names <- sort(ls(envir=envir));

	for(c in col.names) {
		tmp <- get(c, envir=envir);
		cat(tmp, file=file, sep='');
		cat('\t', file=file, sep='');
	}
}


# funkcja dodaje pierwszy wiersz do data frame'u
# pierwszy wiersz zawiera statystyki dotyczace 
# calego zbioru artow np. ilosc wystapien danego slowa
# we wszystkich artach razem wzietych
#
addDfStats <- function(df.file, art.stats) {
	writeEnvCols(art.stats$words, df.file);
	writeEnvCols(art.stats$categories, df.file);

	for(  i in 1:(length(META.COLUMNS)-1)  ) {
		cat('na\t', file=df.file, sep='');
	}
	cat('na\n', file=df.file, sep='');
}

write.art.row.cols <- function(aenv, genv, file) {
	col.names <- sort(ls(envir=genv));

	for(c in col.names) {
		if( exists(c, envir=aenv, inherits=FALSE) ) {
			tmp <- get(c, envir=aenv);
			cat(tmp, file=file, sep='');
		}
		else {
			cat('0', file=file, sep='');
		}

		cat('\t', file=file, sep='');
	}
}


# funkcja odpowiada za dodanie wiersza danych odpowiadajacych
# pojedynczemu plikowi artykulu
#
addNewsRow <- function(art, stats, file) {
	art.env <- new.env(hash=TRUE, parent=emptyenv());
	insertWords(art.env, art$abstract);

	category.env <- new.env(hash=TRUE, parent=emptyenv());
	insertWords(category.env, art$categories);

	write.art.row.cols(art.env, stats$words, file);
	write.art.row.cols(category.env, stats$categories, file);

	# specjalne kolumny
	cat(sprintf('"%s"\t', art$file.name), file=file, sep='');
	cat(sprintf('%d\n', length(art$abstract)), file=file, sep='');
}


# funkcja odpowiada za dodanie wierszy reprezentujacych
# poszczegolne artykuly do dataframe'u
#
addNewsRows <- function(art.stats, file, articles.directory) {
	articles <- list.files(
		path=articles.directory, 
		pattern="*",
		full.names=TRUE,
		ignore.case=TRUE
		);

	for(a in articles) {
		tmp <- load.article(a);
		tmp$file.name <- a; # dodaj info o nazwie pliku
		cat(sprintf('Adding row: %s\n', a));

		addNewsRow(tmp, art.stats, file);
	}
}



# funkcja przeksztalca katalog zawierajcy przetworzone wstepnie
# artykuly na pojedynczy plik zgody z formatem *data frame* jezyka R
# drugi parametr okresla lokalizacje pliku ramki
# 
create.articles.data.frame <- function(articles.directory, output.df.filename) {
	art.stats <- computeGlobalStats(articles.directory);
	df.file   <- file(output.df.filename, "wt");
	
	addDfHeader(df.file, art.stats);
	addDfStats(df.file, art.stats);

	addNewsRows(art.stats, df.file, articles.directory);

	close(df.file);
}



