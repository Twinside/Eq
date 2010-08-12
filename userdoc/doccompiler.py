#!/usr/bin/env python
# coding: utf-8
import glob
import re

def execPrint( l, m, acc ):
    rez = os.system( m.group('command') )
    return ('<pre class="brush:shell">' + acc + '</pre>', rez)
    
def includeFile( l, m, acc ):
    f = open( m.group('file'), 'r' )
    content = f.read()
    f.close()
    return (content, acc)

reglist = [ ( re.compile( '<!-- %%%(?P<brush>[^ ]+) -->' )
            , lambda l, m, acc: 
                ('<pre class="brush: ' + m.group('brush')+ '">' + acc + '</pre>', acc))

          , ( re.compile( '<!-- %%% *-->' )
            , lambda l, m, acc: ('<pre class="brush:shell">' + acc + '</pre>',acc))

          , ( re.compile( '<!-- !!! (?P<command>.*) -->' )
            , lambda l, m, acc: ('', os.system( m.group('command') )))

          , (re.compile( '.*<!-- %INCLUDE% (?P<file>[^ ]*) -->' ), includeFile)
          , (re.compile( '<!-- %% (?P<command>.*) -->' ), execPrint)
          , (re.compile( '.*' ), lambda l, m, acc: (l, acc))
          ]

def compileDocFile( filename, outfilename ):
    docfile = open( filename, 'r' )
    outfile = open( openfilename, 'w' )
    acc = ''

    for line in docfile:
        for (reg,func) in reglist:
            m = reg.match( line )
            if m:
                (toWrite, newAcc) = func( line, m, acc )
                acc = newAcc
                outfile.write( toWrite )
                break

    docfile.close()
    outfile.close()


for docfile in glob.iglob( "" ):
    aoi
