
function performTest( $showAll )
{
    ls invalid/*.txt | % {
        $rez = ../../eq eval -f $("invalid/" + $_.name)
        if ( $showAll ) {
            echo "`n==========================================================="
            cat $_
            $rez
        }

        if ($LASTEXITCODE -eq 0)
        {
            if ( -not $showAll ) {
                echo "`n==========================================================="
                cat $_
            }
            $rez
            echo "FAILURE OF TEST##########"
        }
    }

    ls valid/*.txt | % {
        $rez = ../../eq eval -f $("valid/" + $_.name)
        if ( $showAll ) {
            echo "`n==========================================================="
            cat $_
            $rez
        }

        if ($LASTEXITCODE -ne 0)
        {
            if ( -not $showAll ) {
                echo "`n==========================================================="
                cat $_
            }
            $rez
            echo "FAILURE OF TEST##########"
        }
    }
}

if ($args[0] -eq "-showAll") { performTest( $true ) }
else { performTest( $false ) }

