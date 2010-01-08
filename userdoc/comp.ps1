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

echo "=> Updating CSS"
cp doccss.css compiled/doccss.css

# create the index
echo "=> Index"
indexmaker | out-file -encoding ASCII temp.html
runhaskell doccompiler.hs temp.html 'compiled/index.html'
rm temp.html

$i = 0;
$fileList = ls *.html | ? { -not ($excludeList -contains $_.name ) } | % { $_.name }

$fileList | % { 
    echo $("=> " + $_);

    if ($i -ne 0)
        { echo $('<a href="' + $fileList[$i - 1] + '" class="navigation">Previous</a>') | out-file -encoding ASCII previous.txt }
    else { echo "" | out-file -encoding ASCII previous.txt }

    if ($i -lt $fileList.length - 1)
        { echo $('<a href="' + $fileList[$i + 1] + '" class="navigation">Next</a>') | out-file -encoding ASCII next.txt }
    else { echo "" | out-file -encoding ASCII next.txt }

    runhaskell doccompiler.hs $_ $("compiled/" + $_)

    $i = $i + 1;
}

rm previous.txt
rm next.txt

