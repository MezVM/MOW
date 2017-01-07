library(stringi)


# Plik odpowiada za wstepne przetworzenie newsow
# postac:
#
#   1. Kategoria
#   2.
#   3. Tekst newsa

# lista rozpoznawanych klas
klasy = c("alt.atheism",
          "comp.graphics",
          "comp.os.ms-windows.misc",
          "comp.sys.ibm.pc.hardware",
          "comp.sys.mac.hardware",
          "comp.windows.x",
          "misc.forsale",
          "rec.autos",
          "rec.motorcycles",
          "rec.sport.baseball",
          "rec.sport.hockey",
          "sci.crypt",
          "sci.electronics",
          "sci.med",
          "sci.space",
          "soc.religion.christian",
          "talk.politics.guns",
          "talk.politics.mideast",
          "talk.politics.misc",
          "talk.religion.misc");




# funkcja dodaje na koncu sciezki dir.path
# \ jezeli nie zostal juz dodany
#
appendSlash <- function(dir.path) {
	if(length(grep("[\\/]$", dir.path)) > 0)
		return(dir.path);
	return(paste(dir.path, '/', sep=''));
}



# funkcja usuwa biale znaki z poczatku i konca
# oraz ze srodka linii.
#
trimLines <- function(lines) {
	lines <- gsub("[ \t\n\r]+", " ", lines);
	lines <- gsub("(^[ ]+)|([ ]+$)", "", lines);
	return(lines);
}


# funkcja laczy stringi w wektorze
# lines w jedna dluga linje
#
catLines <- function(lines, sep=' ') {
	result <- '';

	for(line in lines) {
		result <- paste(result, line, sep=sep);
	}
	return(result);
}



# wyodrebnia z pliku linie zwiazana z wlasciwa kategoria w ramach
# lini Newsgroups. Jezeli w lini Xref nic nie bylo, to bedzie tutaj
#
extractCategoryNewsgroup <- function(lines) {
  tmp <- grep("^Newsgroups[ \t]*:", lines);
  start <- tmp[1];
  
  stop  <- NA;
  for(i in (start+1):length(lines)) {
    if(length(grep(":", lines[i])) > 0) {
      stop <- i - 1;
      break;
    }
  }
  
  return( lines[start:stop] );
}


# funkcja wydobywa pole zastosowan z newsie
#
extractCategory <- function(lines,id) {
	
  source <- extractCategoryNewsgroup(lines);
  source <- gsub("^Newsgroups[ \t]*:[ \t]*", "", source);
  for (k in klasy) {
    resoult = stri_extract_last(source, regex = k); 
    if (!is.na(resoult)){
      return(resoult);
    }
  }
  return(NA);
}


# funkcja wydobywacialo z pliku newsa
# news jest dostarczane funkcji w postaci
# wektora liniji
#
extractAbstract <- function(lines) {
  for(i in 1:length(lines)) {
    if(nchar(lines[i]) == 0) {
      start <- i;
      break;
    }
  }
  stop  <- length(lines);
  lines <- lines[start:stop];
	
	abstract.lines 	  <- lines[2:length(lines)];
	

	abstract.lines <- catLines(abstract.lines);
	return(trimLines(abstract.lines));
}


# funkcja dokonuje opisanego w naglowku pliku
# przeksztalcenia streszczenia artykulu nukowego 
#
# lines - to wektor napisow reprezentujacych
# plik ze streszczeniem
#
extractInformation <- function(lines,id) {
	field.of.app <- extractCategory(lines,id);
	abstract <- extractAbstract(lines);

	cat(field.of.app);
	return(c(field.of.app, "", abstract));
}


# funkcja sprawdza czy podany artykul zawiera wymagane
# dane (tj. co najmniej jedna kategorie oraz tekst
# newsa dluzszy niz co najmniej N znakow)
#
# funkcja zostala wprowadzona poniewaz niektore
# artykuly byly zbyt krutkie i trudne do klasyfikacji
#
newsValidation <- function(lines, N=10) {
	is.valid <- ( length(lines) == 3 );

	if(is.valid)
		is.valid <- !is.na(lines[1]) && nchar(lines[1]) > 0;

	if(is.valid)
		is.valid <- !is.na(lines[3]) && nchar(lines[3]) > N;

	return(is.valid);
}


# FUNC preprocessNews(full.input.path, full.output.path)
#
preprocessNews <- function(full.input.path, full.output.path,a)  {
	cat(sprintf("[%s]", full.input.path));
	
	handle <- file(full.input.path, "rt");
	lines  <- readLines(handle);
	close(handle);

	lines <- extractInformation(lines,a);

	if(newsValidation(lines)) {
		handle <- file(full.output.path, "wt");
		writeLines(lines, handle);
		close(handle);
	}
	else {
		cat(': Invalid content');
	}
	cat('\n');
}

# input.path - Sciezka do katalogu zawierajacego
# pliki txt z surowymi tekstami artkulow
#
# output.path - Sciezka do katalogu w ktorym zostana
# zapisane przetworzone pliki
#
preprocess <- function(input.path, output.path) {

	input.path  <- appendSlash(input.path);
	output.path <- appendSlash(output.path);

	articles <- list.files(
		path=input.path, 
		pattern="*",
		full.names=FALSE,
		ignore.case=TRUE
		);

	counter = 0;
	for(a in articles) {
	  if (counter == 3){ 
  		full.input.path  <- paste(input.path, a, sep='');
  		full.output.path <- paste(output.path, a, sep='');
  		try(preprocessNews(full.input.path, full.output.path,a));
  		counter = 0;
	  }
	  counter = counter + 1;
	}

}
