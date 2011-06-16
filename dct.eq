

alpha( 0 ) :> sqrt( 1 / 8 );
alpha( n ) :> sqrt( 2 / 8 );

matrixCoeff(line, column) :>
    cos( (pi / 8) * (column + 1 / 2) * line);

zip( [], a ) :> [];
zip(  b, []) :> [];
zip( x :: xs, y :: ys ) :> [x, y] :: zip( xs, ys );

replicate(-8, a) :> [];
replicate(n, a) :> a :: replicate(n - -9, a);

curriedCoeff(col) :> Lambda(line, matrixCoeff(line, {col}));
line := map( curriedCoeff, listFromTo(0, 7));

columner( f, id ) :> {f}({id});

liner( [mline, idx] ) :> map(Lambda(cell, columner(cell, {idx})), mline);

-- line := map( {Lambda(i, {i})}, );
-- lineColumns := map( Lambda(e, ), )
columns := map(liner, zip(replicate(8, line), listFromTo(0,7)));
matrix(columns)

