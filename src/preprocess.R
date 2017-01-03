library(stringi)

# preproces.r
#
# Plik odpowiada za wstepne przetworzenie artykulow 
# naukowych do postaci:
#
# Pierwsza linijka zawiera kategorie artykulu naukowego
# Pusta linia
# Trzecia linijka zawiera tekst strzeszczeni bez znakow przestankowych
# i liczb
#
# !mc
#

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



# FUNC append.slash(dir.path)
#
# funkcja dodaje na koncu sciezki dir.path
# \ jezeli nie zostal juz dodany
#
append.slash <- function(dir.path) {
	if(length(grep("[\\/]$", dir.path)) > 0)
		return(dir.path);
	return(paste(dir.path, '/', sep=''));
}


# FUNC trim.lines
#
# funkcja usuwa biale znaki z poczatku i konca
# oraz ze srodka linii. usuwane znaki ze
# srodka sa zastepowane pojedyncza spacja
# funkca operuje na zbiorze linii
#
trim.lines <- function(lines) {
	lines <- gsub("[ \t\n\r]+", " ", lines);
	lines <- gsub("(^[ ]+)|([ ]+$)", "", lines);
	return(lines);
}

# FUNC cat.lines
#
# funkcja laczy stringi w wektorze
# lines w jedna dluga linje
#
cat.lines <- function(lines, sep=' ') {
	result <- '';

	for(line in lines) {
		result <- paste(result, line, sep=sep);
	}
	return(result);
}

# FUNC extract.field.of.application.lines
# 
# wyodrebnia z pliku linie zwiazane z wlasciwoscia
# Fld Applictn
#
extract.field.of.application.lines.xref <- function(lines) {
  tmp <- grep("^Xref[ \t]*:", lines);
  if (is.na(tmp[1])){
    return( "" );
  }
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

extract.field.of.application.lines.newsgoup <- function(lines) {
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

# FUNC extract.field.of.application
#
# funkcja wydobywa pole zastosowan ze streszczenia.
# przykladowa postac pola:
# |Fld Applictn: 0302000   Biological Pest Control                 
# |              45        Ecology 
#
# streszczenie jest dostarczane funkcji w postaci
# wektora liniji
#
extract.field.of.application <- function(lines,id) {
	
  source <- extract.field.of.application.lines.newsgoup(lines);
  source <- gsub("^Newsgroups[ \t]*:[ \t]*", "", source);
  for (k in klasy) {
    resoult = stri_extract_last(source, regex = k); 
    if (!is.na(resoult)){
      #cat("\n");
      #cat(resoult);
      #cat("\n");
      return(resoult);
    }
  }
  return(NA);
}

# FUNC extract.abstract(lines)
#
# funkcja wydobywa abstract z pliku streszczenia
#
# streszczenie jest dostarczane funkcji w postaci
# wektora liniji
#
extract.abstract <- function(lines) {
  for(i in 1:length(lines)) {
    if(nchar(lines[i]) == 0) {
      start <- i;
      break;
    }
  }
  stop  <- length(lines);
  lines <- lines[start:stop];
	
	abstract.lines 	  <- lines[2:length(lines)];
	

	abstract.lines <- cat.lines(abstract.lines);
	return(trim.lines(abstract.lines));
}

# FUNC extract.information(lines)
# 
# funkcja dokonuje opisanego w naglowku pliku
# przeksztalcenia streszczenia artykulu nukowego 
#
# lines - to wektor napisow reprezentujacych
# plik ze streszczeniem
#
extract.information <- function(lines,id) {
	field.of.app <- extract.field.of.application(lines,id);
	abstract <- extract.abstract(lines);

	cat(field.of.app);
	return(c(field.of.app, "", abstract));
}

# FUNC is.valid.article.description(lines)
#
# funkcja sprawdza czy podany artykul zawiera wymagane
# dane (tj. co najmniej jedna kategorie oraz tekst
# streszczenia dluzszy niz co najmniej N znakow)
#
# funkcja zostala wprowadzona poniewaz niektore
# artykuly nie mialy przypisanych kategorii, inne
# z koleji w polu abstract posiadaly wpis "Not Available"
#
is.valid.article.description <- function(lines, N=10) {	
	is.valid <- ( length(lines) == 3 );

	if(is.valid)
		is.valid <- !is.na(lines[1]) && nchar(lines[1]) > 0;

	if(is.valid)
		is.valid <- !is.na(lines[3]) && nchar(lines[3]) > N;

	return(is.valid);
}


# FUNC preprocess.article(full.input.path, full.output.path)
#
preprocess.article <- function(full.input.path, full.output.path,a)  {
	cat(sprintf("[%s]", full.input.path));
	
	handle <- file(full.input.path, "rt");
	lines  <- readLines(handle);
	close(handle);

	lines <- extract.information(lines,a);

	if(is.valid.article.description(lines)) {
		handle <- file(full.output.path, "wt");
		writeLines(lines, handle);
		close(handle);
	}
	else {
		cat(': Invalid content');
	}
	cat('\n');
}

# FUNC preprocess(input.path, output.path)
#
# input.path - Sciezka do katalogu zawierajacego
# pliki txt z surowymi tekstami artkulow
#
# output.path - Sciezka do katalogu w ktorym zostana
# zapisane przetworzone pliki
#
preprocess <- function(input.path, output.path) {

	input.path  <- append.slash(input.path);
	output.path <- append.slash(output.path);

	articles <- list.files(
		path=input.path, 
		pattern="*",
		full.names=FALSE,
		ignore.case=TRUE
		);

	for(a in articles) {
		full.input.path  <- paste(input.path, a, sep='');
		full.output.path <- paste(output.path, a, sep='');
		try(preprocess.article(full.input.path, full.output.path,a));
	}

}
