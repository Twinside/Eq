
ls invalid/*.txt | % {
    echo "`n==========================================================="
    cat $_
    ../../eq eval -f $("invalid/" + $_.name)
    if ($LASTEXITCODE -eq 0)
        { echo "FAILURE OF TEST##########" }
}

ls valid/*.txt | % {
    echo "`n==========================================================="
    cat $_
    ../../eq eval -f $("valid/" + $_.name)
    if ($LASTEXITCODE -ne 0)
        { echo "FAILURE OF TEST##########" }
}

