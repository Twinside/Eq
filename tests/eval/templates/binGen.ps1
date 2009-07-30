function genBin( $op, $name )
{
    ls *.txt | % {
        $filename = $_.name
        $outname = "../" + $filename -replace ".txt",$($name + ".txt")
        cat $filename | % { $_ -replace "<OP>",$op } | out-file -encoding ASCII $outname
    }
}

genBin "+" "Add"
genBin "-" "Sub"
genBin "*" "Mul"
genBin "/" "Div"
genBin "^" "Pow"
