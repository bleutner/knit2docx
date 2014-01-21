\docType{package}
\name{knit2docx}
\alias{knit2docx}
\alias{knit2docx-package}
\title{knit2docx}
\usage{
knit2docx(x, out = NULL, withBib = TRUE, bib = NULL,
  style = "harvard1_mod.csl")
}
\arguments{
  \item{x}{character. basename of .Rmd file without
  extension, e.g. "Chapter1"}

  \item{out}{optional. character. basename of output docx
  file. If not suplied \code{x} will be used.}

  \item{withBib}{logical. run pandoc with or without
  pandoc-citeproc, i.e. with or without resolving
  citatations to a bibliography}

  \item{bib}{character. basename of Bibtex .bib file.
  Optional if equal to \code{x}.}

  \item{style}{literature formatting style}
}
\description{
knit2docx

Converts Rmd to docx including some sideeffects, such as
figure labelling, file renaming etc. Currently settings are
rather tightly tied to the requirements of pelagic
publishing. Also it is not tested on anything else than
linux.  Pandoc and pandoc-citeproc must be installed and in
your path. It is not overly beautiful, could be more
efficient and less of a hack, but does the job.
}

