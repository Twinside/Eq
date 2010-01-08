$excludeList = ('temp.html', 'common.html', 'footer.html', 'header.html')

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
    ls *.html | ? { -not ($excludeList -contains $_.name ) } | % {
        stripDocTitle $_.name;
        '<ul>'
            stripDocToc $_.name ;
        '</ul>'
    }
    '        </ul>'
    '    </div>'
    '    </body>'
    '</html>'
}

function stripDocTitle( $filename )
{
    cat $filename | ? { $_ -match 'title' } | 
                    % { $_ -replace '<title>',$('<h3><a href="' + $filename + '">')} |
                    % { $_ -replace '</title>','</a></h3>' } |
                    % { $_ -replace "Eq - ", "" }
}

function stripDocToc( $filename )
{
    cat $filename | ? { $_ -match 'h3' } | 
                    % { $_ -replace "h3","li" } |
                    % { $_ -replace 'name="', $( 'href="' + $filename + '#' ) }
}

cp doccss.css compiled/doccss.css
ls *.html | ? { -not ($excludeList -contains $_.name ) } | % { 
    echo $("=> " + $_.name);
    runhaskell doccompiler.hs $_.name $("compiled/" + $_.name)
}

# create the index
echo "=> Index"
indexmaker | out-file -encoding ASCII temp.html
runhaskell doccompiler.hs temp.html 'compiled/index.html'
rm temp.html

