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

    ls *Test.txt | % {
        echo '<p>'
        ..\..\eq mathmlify -f $_.fullname
        echo '</p>'
    }

    echo '  </body>'
    echo '</html>'
}

makePage | out-file -encoding ASCII da.xhtml
