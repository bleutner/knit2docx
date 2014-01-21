knit2docx
=========
 
Converts Rmd to docx including some side effects, such as figure labelling, file renaming etc.
Currently settings  are rather tightly tied to the requirements of pelagic publishing. 
Also it is not tested on anything else than linux. Pandoc and pandoc-citeproc must be installed and in your path. 
Could be more efficient and less of a hack, but does the job. 

## Installation
    
    library(devtools)
    install_github("bleutner/knit2docx")
    
## Usage
    knit2docx("yourRmdFileBasename")
    
    
        

