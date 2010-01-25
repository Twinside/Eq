
$codeBrush = 'brush: shell'
$outputBrush = 'brush: shell'

function lineAdapter()
{
    Begin { $memorized = '' }
    Process
    {
        $line = $_
        switch -regexp ( $line )
        {
            '<!-- %%%([^ ]+) -->' { '<pre class="brush: ' + $matches[1] + '">'; $memorized; '</pre>' }
            '<!-- %%% *-->'       { '<pre class="' + $outputBrush + '">'; $memorized; '</pre>' }
            '<!-- !!! (.*) -->'   { $memorized = cmd /c $matches[1] 2>&1 }
            '.*<!-- %INCLUDE% ([^ ]*) -->' { cat $matches[1] | lineAdapter }
            '<!-- %% (.*) -->' {
                $command = $matches[1]
                $memorized = cmd /c $matches[1] 2>&1
                '<pre class="' + $codeBrush + '">' + $command + '</pre>'
            }

            default { $line }
        }
    }
}

cat $args[0] | lineAdapter | out-file -encoding ascii $args[1]

