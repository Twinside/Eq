

alpha( 0, size ) :> sqrt( 1 / size );
alpha( n, size ) :> sqrt( 2 / size );

matrixCoeff(size, line, column) :>
    alpha(line, size) * cos( (2 * column + 1) * line * pi / (2 * size));

curriedCoeff(size) :> Lambda(col, Lambda(line, matrixCoeff({size}, line, {col})));

dctSize := 4;

columner( f, id ) :> {f}({id});
line := map( curriedCoeff(dctSize), listFromTo(0, dctSize - 1));
liner( [mline, idx] ) :> map(Lambda(cell, columner(cell, {idx})), mline);
columns := map(liner, zip(replicate(dctSize, line), listFromTo(0,dctSize - 1)));

matrix(columns)

