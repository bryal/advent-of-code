USING: io io.encodings.ascii io.files unicode kernel sequences math locals formatting ;
IN: main

: input ( -- lines )
    "../inputs/day2.txt" ascii file-lines ;
: parse-line ( line -- left right )
    [ 0 swap nth CHAR: A - ]
    [ 2 swap nth CHAR: X - ]
    bi ;
:: round-score ( opponent response -- score )
    response 1 +
    response opponent - 1 + 3 rem 3 *
    + ;
:: needed-move ( opponent outcome -- response )
    outcome opponent + 1 - 3 rem ;
: part1 ( -- total )
    input
    [ parse-line round-score ] map
    sum ;
: part2 ( -- total )
    input
    [ parse-line
      over swap
      needed-move
      round-score ] map
    sum ;

part1 "%d\n" printf
part2 "%d\n" printf
