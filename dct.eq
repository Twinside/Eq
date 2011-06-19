
coeff(line, col) :> xx _ line _ col;

-- matrix(generateMatrix(coeff, 4, 4))
-- ;

-- Real dct coefficient generation
alpha( 0, size ) :> sqrt( 1 / size );
alpha( n, size ) :> sqrt( 2 / size );

matrixCoeff(size, line, column) :>
    alpha(line, size) * cos( (2 * column + 1) * line * pi / (2 * size));

dctMatrix(size) :>
    generateMatrix( Lambda(line, col, matrixCoeff({size}, line, {col}))
                  , size, size);

ourSize := 4;
coeffs := dctMatrix(ourSize);
image := generateMatrix(Lambda(line, col, a _ line _ col), ourSize, ourSize);

coeffs
-- coeffs * image * transpose(coeffs)

