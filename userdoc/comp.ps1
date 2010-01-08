cp doccss.css compiled/doccss.css
ls *.html | % { runhaskell doccompiler.hs $_.name $("compiled/" + $_.name) }

