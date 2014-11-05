#'  Convert Rmd to docx
#'  
#'  Converts Rmd to docx including some sideeffects, such as figure labelling, file renaming etc. Currently settings
#'  are rather tightly tied to the requirements of pelagic publishing. Also it is not tested on anything else than linux.
#'  Pandoc and pandoc-citeproc must be installed and in your path. It is not overly beautiful, could be more efficient and less of a hack, but does the job.
#'  
#'  @param .fileBasename character. basename of .Rmd file without extension, e.g. "Chapter1". Should be preceded by a path if not in the current working directory, e.g. "FolderX/Chapter1".
#'  @param .docxFile optional. character. basename of output docx file. If not suplied \code{.fileBasename} will be used.
#'  @param .withBibliography logical. run pandoc with or without pandoc-citeproc, i.e. with or without resolving citatations to a bibliography
#'  @param .bibFile character. basename of Bibtex .bib file. Optional if equal to \code{x}. 
#'  @param .bibStyle literature formatting style
#'  @export 
#' @examples 
#' \dontrun{
#' ## Copy demo files to knit2docx_demo folder
#' dir.create("knit2docx_demo")
#' file.copy(list.files(system.file("extdata/demo/", package = "knit2docx")), "knit2docx_demo", recursive = TRUE)
#' rlist.files("knit2docx_demo")
#' ## Convert Rmd to docx
#' knit2docx("knit2docx_demo/Chapter_template")
#' ## Open with Word:
#' list.files("knit2docx_demo", pattern = "docx")
#' ## Remove demo directory
#' unlink("knit2docx_demo", recursive = TRUE)
#' }
#'  
knit2docx <- function(.fileBasename, .docxFile = NULL, .withBibliography = TRUE, .bibFile = NULL,  .bibStyle = "harvard1_mod.csl"){

    .rmdFile <- paste0(.fileBasename, ".Rmd")
    .mdFile  <- paste0(.fileBasename, ".md")
    if(is.null(.docxFile)) .docxFile <- paste0(.fileBasename, ".docx")
    if(.withBibliography & is.null(.bibFile)) .bibFile <- paste0(.fileBasename, ".bib")
    
    if(!file.exists(.rmdFile)) stop(paste0(.rmdFile, " does not exist!"), call. = FALSE)
    if(.withBibliography){
        if(!file.exists(.bibFile))  stop(paste0(.bibFile, " does not exist!"), call. = FALSE)
        if(!file.exists(.bibStyle)) stop(paste0(.bibStyle, " does not exist!"), call. = FALSE)
    }
    
    ## Create figure captions function in local environment
    caption <- local({
                .figureNr <- 0
                function(.captionText, label=NA){
                    .figureNr <<- .figureNr + 1
                    out <- paste0("Fig ", currentChapter,".",.figureNr, " ", .captionText)
                    if(!is.na(label)) out <- paste0(out, "<reflab>", label, "<reflab>")
                    return(out)
                }
            })
    
    
    ## Knit to markdown 
    knit(input = .rmdFile, output = .mdFile)
    
    
    ## Get plot lines in md
    .md_internal <- readLines(.mdFile)
    
    
    ## Copy external files to 'figure' folder #######################
    fix <- grep("^.*\\(external/", .md_internal)
    if(length(fix) > 0){
        exFile <- inFile <- str_trim(str_replace(basename(.md_internal[fix]),")",""))
        ## Check if files actually exist
        if(any(!file.exists(paste0("external/",exFile)))) stop(paste0("File external/", exFile[!file.exists(paste0("external/",exFile))], " does not exist!"), call. = FALSE)       
        ## Add some stuff to keep unique (in randomness we trust our lazy soul)
        inFile <- paste0(sample(10000:99999, length(fix)), exFile)
        ## Copy
        file.copy(paste0("external/", exFile), paste0("figure/", inFile), overwrite=TRUE)
        ## Fix md
        .md_internal[fix]  <- str_replace(.md_internal[fix], paste0("external/", exFile), paste0("figure/", inFile))
        cat("Files copied from external/ to figure/: ", exFile)
    }
    
    ## Rename and label figures ######################################
    fix <- grep("^!\\[Fig", .md_internal)
    if(length(fix) > 0) {
        if(any(str_count(.md_internal[fix], "\\[Fig") > 1)) stop ("Execution halted! Each plot must be generated in a separate chunk. No chunk should produce two plots.", call. = FALSE)
        
        g   <- .md_internal[fix]
        bn <- str_replace(basename(g), ")", "")
        filedot <- lapply(lapply(str_locate_all(bn, pattern = "\\."), max),"-",1)
        fbase <- substring(bn, 1, filedot)
        numb  <- lapply(str_split(substr(g,7,11)," "),"[",1)
        
        ## Fix reference in markdown
        .md_internal[fix] <- str_replace(g, paste0("figure/",fbase), paste0("figure/Fig_", numb))
        
        ## Rename files
        for(fb in seq_along(fbase)){
            fil <- list.files(path = "figure", pattern = paste0(fbase[[fb]],"\\."))
            exts <- paste0(".",unlist(lapply(str_split(fil,"\\."),tail,1)))
            for(ext in exts){
                old <- paste0("figure/", fbase[fb], ext)
                new <- paste0("figure/Fig_", numb[fb], ext)
                cat("\nrenamed file", old, "to", new)
                file.rename(old, new)
            }
        } 
    }
    
    ## Resolve cross-references #####################################
    refs <- grep("<ref>",.md_internal)
    if(length(refs) > 0){
        nref <-  sum(str_count( .md_internal, "<ref>"))
        if(nref %% 2) stop("Execution halted! There's at least one '<ref>' tag which is not closed", call. = FALSE)
        cat("\n\nfound", nref/2, "cross-references to figures")
        
        for(i in refs){
            howMany <- str_count( .md_internal[i], "<ref>") 
            while(howMany > 0) {
                fi <- str_locate_all(.md_internal[i], "<ref>")[[1]]
                label <- substr(.md_internal[i], fi[1,"end"]+1, fi[2,"start"]-1)
                
                ## Search label target
                fig <-  grep(paste0("<reflab>", label,"<reflab>"), .md_internal)
                
                ## Security gate: does it exist and is it unique?
                if(length(fig) == 0) stop(paste0("Execution halted! Reference label '", label, "' does not exist. Use caption(x,label='yourLabel') to fix this"), call. = FALSE)
                if(length(fig) > 1)  stop(paste0("Execution halted! Reference label '", label, "' is not unique."), call. = FALSE)
                
                ## Extract target numbering
                numb  <-   lapply(str_split(substr(.md_internal[fig],7,11)," "),"[",1)
                
                ## Replace label by proper reference
                Fig_string <- "" # "Fig. "
                .md_internal[i] <- str_replace(.md_internal[i], paste0("<ref>",label,"<ref>"), paste0(Fig_string, numb))
                howMany <- str_count( .md_internal[i], "<ref>") 
            }
        }
        
        ## Tidy captions
        cat("\ncross-references resolved")
        .md_internal <-  str_replace(.md_internal, "<reflab>.*<reflab>", "")
    }
    
    ## Number captions ##############################################
    Hn <- grep("^#", .md_internal)
    
    ## Fix deprecated
    fixmeImOld <- grep("<H[12345]>", .md_internal)
    if(length(fixmeImOld) > 0){
        message("Usage of <H1> <H2> etc. for headings is deprecated. Its enough now to use #, ##, etc.")
        .md_internal[fixmeImOld] <- str_replace(.md_internal[fixmeImOld], "<H[12345]>", "")
    }
    
    chunkdelim <- grep("^```", .md_internal)
    if(length(chunkdelim) %% 2)  stop("At least one chunk of R code is not properly opened or closed!")
    chunkopen <- chunkdelim[seq(1,length(chunkdelim), 2)]
    chunkclose <- chunkdelim[seq(2,length(chunkdelim), 2)]
    
    chunkmembers <- unlist(sapply(seq_along(chunkopen), function(i) chunkopen[i]:chunkclose[i]))
    
    Hn <- Hn[!Hn %in% chunkmembers]
    if(length(Hn) > 0) {
        Hnum <- sapply(sapply(str_split(.md_internal[Hn], ""), "==", "#"), "sum")
        RmdF <- readLines(.rmdFile, 20)
        chap <-  as.numeric(gsub("\\D","",RmdF[grep("currentChapter", RmdF)]))
        
        Hl <- c(chap,0,0,0,0)
        
        labs <- character(length(Hn))
        for(i in seq_along(Hn)){
            s <- Hnum[i]
            Hl[s] <- Hl[s]+1
            Hl[(s+1):5] <- 0
            if(s == 2) labs[i] <- paste0(" ", Hl[1],".",Hl[[2]], " ")
            if(s == 3) labs[i] <- paste0(" ", Hl[1],".",Hl[2],".", Hl[3], " ")
            #if(s == 4) labs[i] <- paste0(Hl[1],".",Hl[2],".", Hl[3],".", Hl[4]) ## pleagic doesn't want #4 numbered
        }
        
        cat("\n\nadded section counters to ", length(Hn), "headings (Headings >H3 won't be labelled)")
        .md_internal[Hn] <- str_replace(.md_internal[Hn], "#\ ", labs)
    }
    
    ## Rewrite md
    writeLines(.md_internal, .mdFile)
    
    ## Run pandoc	
    pcall <- paste("pandoc -sS", .mdFile, "-o", .docxFile, "--highlight-style=monochrome" )
    if(.withBibliography) pcall <- paste(pcall, "--bibliography", .bibFile, "--csl", .bibStyle)
    
    cat("\n\nrunning pandoc with call: ", pcall)  
    system(pcall, intern = FALSE, wait = FALSE)
    cat("\n",rep("-",20),"\n")
    message("\nwrote ", .fileBasename, ".docx")
    
}


