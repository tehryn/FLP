/*
Author: Jiri Matejka
Login: xmatej52
Modified: 17. 04. 2019
Project: Logicky projekt do predmetu FLP - kostra grafu
*/

% Reads one line from stdin
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

% Reads whole stdin
readLines( Result ) :-
    readLine( First, Char ),
    ( char_type( Char, end_of_file ) ->
        Result = [];
        readLines( Rest ),
        Result = [ First | Rest ]
    ).

% Formats the line into Edge. Return false if line is invalid
formatLine( [ A, B, C ], Result ) :-
    char_type( A, alpha ),
    char_type( B, white ),
    char_type( C, alpha ),
    A \= C,
    Result = A-C.
formatLine( _ ) :- false.

% Formats input from user. Ignore invalid or multiple edges. Result is valid representation of graph.
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

% Reverse oriented edge
reverseEdge( A-B, Reversed ) :- Reversed = B-A.

% Find one spanning tree in graph
spanningTree( Graph, Result ) :-
    member( Edge, Graph ), % takes some edge
    seek( [ Edge ], Graph, Result ). % Edge is spanning tree, lets seek more edges that can be added

% Append edges to new graph until spanning tree is built
seek( Result, Graph, Result ) :- not( appendEdge( Result, Graph, _ ) ). % recursion end - no edge can be added without creating cycle
seek( Edges, Graph, Result ) :- appendEdge( Edges, Graph, Modified ), seek( Modified, Graph, Result ).

% Test if nodes are connected in graph
connected( A, B, Graph ) :- member( A-B, Graph ); member( B-A, Graph ).

% Tries to append edge into spanning tree
appendEdge( Edges, Graph, Result ) :-
    connected( A, B, Graph ), % edge is from graph
    connected( A, _, Edges ), % A is connected with something
    not( connected( B, _, Edges ) ), % B is not in current spanning tree
    Result = [ A-B | Edges ].

% sorts all results and delete duplcities
formatResults( Input, [ R | Results ], Formated ) :- formatResult( Input, R, Modified ), formatResults( Input, Results, Rest ), sort( [ Modified | Rest ], Formated ).
formatResults( _, [] , Formated ) :- Formated = [].

% sorts one result
formatResult( Input, [ Edge | Graph ], Formated ) :- formatEdge( Input, Edge, Modified ), formatResult( Input, Graph, Rest ), sort( [ Modified | Rest ], Formated ).
formatResult( _, [] , Formated ) :- Formated = [].

% reverses edge if needed
formatEdge( Input, Edge, Modified ) :- member( Edge, Input ), Modified = Edge; reverseEdge( Edge, Modified ).

% writes all results to stdout
writeResults( [] ).
writeResults( [ Graph | Results ] ) :- writeResult( Graph ), writeResults( Results ).

% writes one result to stdout
writeResult( [] ).
writeResult( [ Edge | [] ] ) :- write( Edge ), nl.
writeResult( [ Edge | Result ] ) :- write( Edge ), write( ' ' ), writeResult( Result ).

% get sorted list of all nodes from graph
getNodes( [], Result ) :- Result = [].
getNodes( [ A-B | Graph ], Result ) :- getNodes( Graph, Rest ) ,  sort( [ A | [ B | Rest ] ], Result ).

% test if all results are correct
testResults( [], _ ).
testResults( [ Result | Results ], Nodes ) :- testResult( Result, Nodes ), testResults( Results, Nodes ).

% test if result is correct
testResult( [] ).
testResult( Result, Nodes ) :- getNodes( Result, RNodes ), Nodes == RNodes.

main :-
    prompt(_, ''),
    readLines( InputLines ), % loads data
    parseLines( InputLines, ParsedInput ), % creates graph
    findall( Result, spanningTree( ParsedInput, Result ), Results ), % finds all solutions
    formatResults( ParsedInput, Results, FormatedResults ), % remove duplicities and sorts solutions
    getNodes( ParsedInput, Nodes ),
    ( testResults( Results, Nodes ) -> writeResults( FormatedResults ); halt ), % if solutions are correct, write them to stdout
    halt.