readLine( Result, Char ) :-
    get_char( Char ),
    ( char_type( Char, end_of_file ) ->
        Result = [];
        ( char_type( Char, end_of_line ) ->
            Result = [];
            readLine( Rest, _ ),
            Result = [ Char | Rest ]
        )
    ).

readLines( Result ) :-
    readLine( First, Char ),
    ( char_type( Char, end_of_file ) ->
        Result = [];
        readLines( Rest ),
        Result = [ First | Rest ]
    ).

formatLine( [ A, B, C ], Result ) :-
    char_type( A, alpha ),
    char_type( B, white ),
    char_type( C, alpha ),
    A \= C,
    Result = A-C.
formatLine( _ ) :- false.

parseLines( [], Result ) :- Result = [].
parseLines( [ Line | Lines ], Result ) :-
    parseLines( Lines, Rest ),
    ( formatLine( Line, FormatedLine ) ->
        reverseEdge( FormatedLine, Reversed ),
        ( member( FormatedLine, Rest ) ->
            Result = Rest;
            ( member( Reversed, Rest ) ->
                Result = Rest;
                Result = [ FormatedLine | Rest ]
            )
        );
        Result = Rest
    ).

reverseEdge( A-B, Reversed ) :- Reversed = B-A.

allPermutations( Graph, Result ):-findall( Permutation, permutation( Graph, Permutation ), Result ).

spanningTree( Graph, Result ) :-
    nth0( 0, Graph, Edge ),
    seek( [ Edge ], Graph, Result ).

connected( A, B, Graph ) :- member( A-B, Graph ); member( B-A, Graph ).

appendEdge( Edges, Graph, Result ) :-
    connected( A, B, Graph ),
    connected( A, _, Edges ),
    not( connected( B, _, Edges ) ),
    Result = [ A-B | Edges ].

seek( Edges, Graph, Edges ) :- not( appendEdge( Edges, Graph, _ ) ).
seek( Edges, Graph, Result ) :- appendEdge( Edges, Graph, Modified ), write( hrany: Edges ), nl, write( modifikovano: Modified ), nl, seek( Modified, Graph, Result ).

reverse( [ X|XS ], A, B ):-  reverse( XS, [X|A], B ).
reverse( [], A, A ).

formatResults( Input, [ R | Results ], Formated ) :- formatResult( Input, R, Modified ), formatResults( Input, Results, Rest ), sort( [ Modified | Rest ], Formated ).
formatResults( _, [] , Formated ) :- Formated = [].

formatResult( Input, [ Edge | Graph ], Formated ) :- formatEdge( Input, Edge, Modified ), formatResult( Input, Graph, Rest ), sort( [ Modified | Rest ], Formated ).
formatResult( _, [] , Formated ) :- Formated = [].

formatEdge( Input, Edge, Modified ) :- member( Edge, Input ), Modified = Edge; reverseEdge( Edge, Modified ).

main :-
    readLines( InputLines ),
    write( InputLines ), nl,
    parseLines( InputLines, ParsedInput ),
    write( ParsedInput ), nl,
    allPermutations( ParsedInput, Permutations ),
    write( Permutations ), nl,
    maplist( spanningTree, Permutations, Results ),
    formatResults( ParsedInput, Results, FormatedResults ),
    %list_to_set( FormatedResults, Set ),
    write( FormatedResults ), nl,
    halt;
    write( 'wtf??' ),
    nl;
    halt.