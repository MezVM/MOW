
# Uzywac tego skryptu jako pierwszego.

# NAZWY KATALOGOW POWINNY KONCZYC SIE /
# Sciezka do katalogu ktory zawiera rozpakowane archiwum
org.data.directory <- 'D:/MOW/20_newsgroups/'


# Sciezka do katalogu ktory bedzie zawieral
# przetworzone dane oraz dane tymczsowe
data.directory <- 'D:/MOW/tmp/';



# FUNC str.join
#
# funkcja jest aliasem dla wywolania
# paste(a, b, sep='')
#
str.join <- function(a, b) {
	return(paste(a, b, sep=''));
}


data.in  <- str.join(data.directory, 'data_original/');
data.out <- str.join(data.directory, 'data_preprocessed/');
dir.create(data.in, recursive=TRUE);
dir.create(data.out, recursive=TRUE);
cat('Kopiowanie plikow...\n');
flist  <- list.files(org.data.directory, recursive=TRUE, pattern='*', full.names=TRUE);
for(f in flist) {
	cat(sprintf("Plik: %s\n", f));

	new.name <- str.join(data.in, basename(f));
	file.copy(f, new.name, overwrite=TRUE);
}
source('preprocess.R');
preprocess(data.in, data.out);

