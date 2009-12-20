function evalFormat( $command, $showFile )
{
    ls .\tests\format\*Test.txt | % {
        ..\..\eq $command -o "out.txt" -f $_.Fullname
        if ($showFile) { cat $_.FullName }
        cat out.txt
    }
}

function Latexify(  )
{
    echo "\documentclass[11pt]{article}"
    echo "\usepackage{amssymb}"
    echo "\usepackage{amsmath}"
    echo "\begin{document}"
    evalFormat latexify $false 
    echo "\end{document}"
}

function makePage()
{
    echo '<?xml version="1.0" encoding="UTF-8" ?>'
    #echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "DTD/xhtml1-strict.dtd">  '
    echo '<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN"'
    echo '               "http://www.w3.org/TR/MathML2/dtd/xhtml-math11-f.dtd" ['
    echo '  <!ENTITY mathml "http://www.w3.org/1998/Math/MathML">'
    echo ']>'
    echo '<html xmlns="http://www.w3.org/1999/xhtml">'
    echo '  <head>'
    echo '    <title>Minimum XHTML Document</title>'
    echo '  </head>'
    echo '  <body>'
    echo '    <p>'
    echo '      Accept all valid XHTML, including therefore'
    echo '      <a href="http://www.mozilla.org/">links</a>.'
    echo '    </p>'

    evalFormat mathmlify $false

    echo '  </body>'
    echo '</html>'
}

evalFormat format $true > txtFormat.txt

makePage | out-file -encoding ASCII mathml.xhtml

Latexify | out-file -encoding ASCII latexFormat.tex
pdflatex latexFormat.tex
