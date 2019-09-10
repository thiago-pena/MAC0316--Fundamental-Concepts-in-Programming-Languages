#!/bin/bash
for i in 1 2 3 4 5 6 7 8 9 10
do
   {    echo "teste$i";
        cat "teste$i.txt";
        ./mcalc < "teste$i.txt";
        ./mcalc < "teste$i.txt" | ./direto
    } | tr "\n" "\n"
  printf "\n"
done
