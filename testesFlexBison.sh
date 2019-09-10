#!/bin/bash
for i in 1 2 3 4 5 6 7 8 9 10
do
   {    echo "teste$i";
        cat "teste$i";
        ./mcalc < "teste$i";
        ./mcalc < "teste$i" | ./direto
    } | tr "\n" "\n"
  printf "\n"
done
