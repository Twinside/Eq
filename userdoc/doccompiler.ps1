
$codeBrush = 'brush: shell'
$outputBrush = 'brush: shell'

function lineAdapter()
{
    Begin { $memorized = '' }
    Process
    {
        $line = $_
        if ($line -match '<!-- %% (.*) -->')
        {
            $command = $matches[1]
            $memorized = cmd /c $matches[1] 2>&1
            '<pre class="' + $codeBrush + '">' + $command + '</pre>'
        }
        elseif ( $line -match '<!-- %%%([^ ]*) -->')
        {
            $lang = $matches[1]
            if ( $lang -ne '' )
            { '<pre class="brush: ' + $lang + '">' }
            else { '<pre class="' + $outputBrush + '">' }
            $memorized
            '</pre>'
        }
        elseif ( $line -match '.*<!-- %INCLUDE% ([^ ]*) -->')
            { cat $matches[1] | lineAdapter }
        else { $line }
    }
}

cat $args[0] | lineAdapter | out-file -encoding ascii $args[1]

