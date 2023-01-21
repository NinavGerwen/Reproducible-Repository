DATA ARCHIVE
===
---

This repository exhibits how dichotomous data can be simulated for Item Response Theory (IRT) related research according to
different models. Furthermore, a real-life example is given of where this can be used. Software requirements are R (http://www.r-project.org)
and packages magrittr (version 2.0.3, https://cran.r-project.org/web/packages/magrittr/magrittr.pdf), 
dplyr (version 1.0.10, https://cran.r-project.org/web/packages/dplyr/dplyr.pdf) and ltm (version 1.2.0, https://cran.r-project.org/web/packages/ltm/ltm.pdf)
to run the simulation study. Optionally, to render the LaTeX file yourself, a TeX distribution program is necessary (e.g., MiKTeX, https://miktex.org/).

---

# Contents

| Files/Folders 	| Description	|
| ------------------	| ------------- |
| Example.rmd		| Rmarkdown file that runs an example simulation study that uses simulated dichotomous questionnaire data |
| Example.html		| Knitted version of the Rmarkdown file with the example simulation study |
| Functions		| Folder containing annotated functions used to generate data by '1.Example.R'|
| Example-Manuscript.pdf| A rendered LaTeX file that uses the results of the Example.rmd file in their results (Table 4)|
| Example-Manuscript.tex| The .tex file that was used to render the Example-Manuscript.pdf file|
| Manuscript-Files	| Folder containing the necessary bibliography and documentclass file to render Example-Manuscript.pdf|

For any help with the files in this archive, please contact Nina van Gerwen (n.l.vangerwen@uu.nl).