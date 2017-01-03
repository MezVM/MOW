library(stringi)

lines <- "Xref: cantaloupe.srv.cs.cmu.edu comp.archives.msdos.d:2258 comp.os.ms-windows.apps:12106 comp.os.ms-windows.misc:10000"
id = 10000
#([ ].{1,24}:60291)

lines <- gsub("^Xref:", "", lines);
rx <-  paste("([ ].{1,24}:", as.character(id), sep='')
rx <-  paste(rx, ")", sep='')
lines <- stri_extract_last(lines, regex = rx)
lines <- gsub("[ ]","", lines);
rx <-  paste(":", id, sep='')
lines <- gsub(rx, "", lines);
lines
klasy
asd = NA;
for (k in klasy) {
  asd = stri_extract_last(lines, regex = k); 
  if (!is.na(asd)){
    break;
  }
}
asd
