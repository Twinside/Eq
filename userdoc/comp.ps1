function indexmaker()
{
    '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">'
    '<html>'
    '    <head>'
    '<!-- %INCLUDE% common.html -->'
    '        <title>Eq - documentation</title>'
    '    </head>'
    '    <body>'
    '    <div class="section">'
    '        <h2>Eq - documentation</h2>'
    '        <ul>'
    ls *.html | ? { $_.name -ne 'temp.html' } | ? { $_.name -ne 'common.html' } | % {
        '<li><a href="' + $_.name + '">' + $($_.name -replace '.html','') + '</a></li>'
    }
    '        </ul>'
    '    </div>'
    '    </body>'
    '</html>'
}

cp doccss.css compiled/doccss.css
ls *.html | % { runhaskell doccompiler.hs $_.name $("compiled/" + $_.name) }

# create the index
indexmaker | out-file -encoding ASCII temp.html
runhaskell doccompiler.hs temp.html 'compiled/index.html'
rm temp.html

