ls *Test.txt | % {
    ..\dist\build\formularender\formularender.exe $_.Name "out.txt"
    cat $_.Name
    cat out.txt
}

