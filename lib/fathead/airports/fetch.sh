#!/bin/bash

[ ! -e download ] && mkdir download

for i in A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
do
    wget -q -O download/$i -N 'https://en.wikipedia.org/wiki/List_of_airports_by_IATA_code:_'$i
done
