if ( $args.count -gt 0 )
{
    ..\dist\build\formularender\formularender.exe $args[0] "out.txt"
    cat $args[0]
    cat out.txt
}
else
{
    ls *Test.txt | % {
        ..\dist\build\formularender\formularender.exe $_.Name "out.txt"
        cat $_.Name
        cat out.txt
    }
}

