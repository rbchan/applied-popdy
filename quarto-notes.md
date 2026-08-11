# Notes on moving to Quarto

I used to use `*.Rnw` to create beamer slides. Beamer is great but the PDFs don't meet UGA's new accessibility requirements. So I'm trying to convert from knitr's `*.Rnw` to Quarto's `*.qmd` file structure for [revealjs](https://quarto.org/docs/presentations/revealjs/).

The new html presentations will be hosted on github pages using the `gh-pages` branch of the repo. The update the website:

```
git checkout gh-pages
<make changes>
quarto render
```

Then commit and push as usual. You might want to update `schedule.qmd` and `index.qmd` too.




## Converting from beamer to Quarto/reveal.js

There are several options:

1. For plain .tex files, you can try this:
  i. `pandoc -f latex -t markdown pres.tex -o pres.qmd`
  ii. Open pres.qmd and add YAML header
  iii. Then you have to make *extensive* edits
  
2. Or could try to convert to Rmd first using Rnw2Md package, but this doesn't seem to work well:
```
# 1. Install conversion utilities
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("Bioconductor/Rnw2Rmd")

# 2. Run the programmatic conversion
# This translates your standard <<>>= and @ block wrappers into ````{r} blocks
Rnw2Rmd::Rnw2Rmd(from = "presentation.Rnw", to = "presentation.Rmd")

# 3. Rename file to Quarto format
file.rename("presentation.Rmd", "presentation.qmd")
```

3. Another option is to work on custom `rnw2qmd.R` script


- See [](lectures/intro/lecture-intro.qmd) for an example of what the formatted qmd file should look like that meets accessibility standards
  * Important to add `fig-alt` tags to every figure


## To create PDF handout for students

- Maybe try veraPDF

- To put regular pdf output in handout format with 4 slides/page:
  * `pdfjam --nup 2x2 --landscape original_slides.pdf --output handout_4up.pdf`
  * Or `pandoc input.html --pdf-engine=chrome -o output.pdf`
  * Or `pandoc input.html --pdf-engine=weasyprint -o output.pdf`

- The above isn't great. Better to create HTML, then enter print mode with `e` command in Chrome. Then print with 4 slides/page


## To render from command line:
  * `quarto render doc.qmd --to revealjs`
  * Or if you want just a compressed version
    + `quarto render doc.qmd --to html`
    + `quarto render doc.qmd --to html --toc`
  * To render the entire website, go to gh-pages branch, then:
    + `quarto render`


