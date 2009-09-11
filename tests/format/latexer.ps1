echo "\documentclass[11pt]{article}"
echo "\usepackage{amssymb,amsmath}"

echo "\begin{document}"
ls *Test.txt | % {
    ..\..\eq latexify -f $_.fullname
}
echo "\end{document}"

