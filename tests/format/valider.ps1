if ( $args.count -gt 0 )
{
    ..\..\eq format -o "out.txt" -f $args[0]
    cat $args[0]
    cat out.txt
}
else
{
    ls *Test.txt | % {
        ..\..\eq format -o "out.txt" -f $_.Fullname
        cat $_.Name
        cat out.txt
    }
}

