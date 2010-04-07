width := 5;

indexer( n, m ) :> a _ n _ m;
lister( n ) :> map( Lambda( j, indexer( n, j )), listFromTo( 1, width ) );

mtrx := matrix( map( lister, listFromTo( 1, width ) ) );
Hold( Force( mtrx ) _ 2 _ 2 ) = mtrx _ 2 _ 2

