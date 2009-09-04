function patternFormatting( $pattern, $op, $name )
{
    ls $pattern | % {
        $filename = $_.name
        $outname = "../" + $filename -replace ".txt",$($name + ".txt")
        cat $filename | % { $_ -replace "<OP>",$op } | out-file -encoding ASCII $outname
    }
}

function genBin( $op, $name ) { patternFormatting "bin*.txt" $op $name }
function genComp( $op, $name ) { patternFormatting "comp*.txt" $op $name }

genBin "+" "Add"
genBin "-" "Sub"
genBin "*" "Mul"
genBin "/" "Div"
genBin "^" "Pow"

genComp ">" "Gt"
genComp "<" "Lt"
genComp ">=" "Ge"
genComp "<=" "Le"
genComp "/=" "Diff"

