# conceitos-ep1

Nome: Thiago Jose Benitez Pena
nUSP: 6847829

1. Descrição da linguagem
As operações básicas estão como no arquivo de exemplo. As sintaxes das operações adicionais são:
    1.1. divisão:
        <número>/<número>

    1.2. condicional:
        if <condição> { <expressão> } else { <expressão> }

    1.3. chamada de função:
        call <nome da função> ( <argumento> )

    Para realizar os testes, basta abrir e rodar o arquivo 'testesRacket.rkt' no Dr. Racket que eles serão executados automaticamente. Ela é uma cópia do arquivo 'direto.rkt', mas sem o comando 'read'.

    Foram criadas 2 funções adicionais:
        (A) 'pow2n', que recebe um parâmetro n e retorna 2**n;
        (B) 'Sn', que recebe um parâmetro n e retorna a soma 1 + 2 + 3 + ... + n


2. Gramática
Para o compilador, foram usados o FLEX e o BISON.
Para os testes, utilizei um script em bash 'testesFlexBison.sh'
Para executá-lo, após rodar o make, basta rodar o comando 'sh testesFlexBison.sh'
dentro do diretório.
O teste mostra 3 linhas, contendo:
    (A) a entrada do teste
    (B) a tradução feita pelo Flex + Bison
    (C) o resultado

Segue a saída do script.
    2.1. Saída do teste com script.
        teste1
        2. + 3
        (+ 2. 3)
        5.0

        teste2
        (10*4) + ( 1+ 1)
        (+ (* 10 4) (+ 1 1))
        42

        teste3
        8 * (7 - 4*(5 - -3))
        (* 8 (- 7 (* 4 (- 5 (~ 3 )))))
        -200

        teste4
        12     /   4
        (/ 12 4)
        3

        teste5
        3 * 4 / ( 2 + 4 ) + 1
        (+ (/ (* 3 4) (+ 2 4)) 1)
        3

        teste6
        if 3 { 4 }else {1}
        (if 3 4 1)
        4

        teste7
        call dobro (2)
        (call dobro 2)
        4

        teste8
        if 1 { call fatorial (5) } else { -1 / 4 - 1 + 2 * 3 }
        (if 1 (call fatorial 5) (+ (- (/ (~ 1 ) 4) 1) (* 2 3)))
        120

        teste9
        call pow2n (10)
        (call pow2n 10)
        1024

        teste10
        call Sn (100)
        (call Sn 100)
        5050
