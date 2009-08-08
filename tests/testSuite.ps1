function isInValid( [bool]$isValid )
{
    if ($isValid) { return $LASTEXITCODE -ne 0 }
    else { return $LASTEXITCODE -eq 0 }
}

function performTestSuite( [string]$where, [bool]$isValid, [string]$command, [bool]$showAll )
{
    if ( $isValid ) { $testPath = $where + "\valid\" }
    else { $testPath = $where + "\invalid\" }

    $toSearch = $testPath + "*.txt"
    echo $toSearch

    ls $($toSearch) | % {
        echo $_.FullName
        $rez = ../eq $command -f $($testPath + $_.name)
        if ( $showAll ) {
            echo "`n==========================================================="
            cat $_
            $rez
        }

        if (isInvalid $isValid)
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

function performValidTest( $where, $command, $showAll )
    { performTestSuite $where $true $command $showAll }
function performInvalidTest( $where, $command, $showAll )
    { performTestSuite $where $false $command $showAll }

function performFullTest( $where, $command, $showAll )
{
    performValidTest $where $command $showAll
    performInvalidTest $where $command $showAll
}

$toShowAll = $false
if ($args[0] -eq "-showAll") { $toShowAll = $true }

performFullTest "eval" "eval" $toShowAll
performFullTest "derivate" "eval" $toShowAll

