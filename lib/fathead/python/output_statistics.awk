#!/usr/bin/awk -f
BEGIN { FS="\t"; nbrArticles=0; nbrRedirects=0; nbrDis=0 }
$2=="A" { nbrArticles=nbrArticles+1 } 
$2=="R" { nbrRedirects=nbrRedirects+1 } 
$2=="D" { nbrDis=nbrDis+1 } 
END { 
	print "Output contains:"; 
	print "Articles:" nbrArticles; 
	print "Redirects:" nbrRedirects; 
	print "Disambiguations:" nbrDis }
