#'  Convert Rmd to docx
#'  
#'  Converts Rmd to docx including some sideeffects, such as figure labelling, file renaming etc. Currently settings
#'  are rather tightly tied to the requirements of pelagic publishing. Also it is not tested on anything else than linux.
#'  Pandoc and pandoc-citeproc must be installed and in your path. Could be more efficient and less of a hack, but does the job.
#'  
#'  @param x character. basename of .Rmd file without extension, e.g. "Chapter1"
#'  @param out optional. character. basename of output docx file. If not suplied \code{x} will be used.
#'  @param withBib logical. run pandoc with or without pandoc-citeproc, i.e. with or without resolving citatations to a bibliography
#'  @param bib character. basename of Bibtex .bib file. Optional if equal to \code{x}. 
#'  @param style literature formatting style
#'  
#'  @author Benjamin Leutner \email{benjamin.leutner@@uni-wuerzburg.de}
#'
knit2docx <- function(x, out = NULL, withBib = TRUE, bib = NULL,  style = "harvard1_mod.csl"){
	
	Rmd     <- paste0(x, ".Rmd")
	md      <- paste0(x, ".md")
	if(is.null(out)) out <- paste0(x, ".docx")
	if(withBib & is.null(bib)) bib <- paste0(x, ".bib")
	
	if(!file.exists(Rmd)) stop(paste0(Rmd, " does not exist!"), call. = FALSE)
	if(withBib){
		if(!file.exists(bib)) stop(paste0(bib, " does not exist!"), call. = FALSE)
		if(!file.exists(style)) stop(paste0(style, " does not exist!"), call. = FALSE)
	}
	require(knitr)
	require(stringr)
	## Knit to markdown
	knit(input = Rmd, output = md)
	
	## Get plot lines in md
	mdF <- readLines(md)
	
	## Copy external files to 'figure' folder #######################
	fix <- grep("^.*\\(external/", mdF)
	if(length(fix) > 0){
		exFile <- inFile <- str_replace(basename(mdF[fix]),")","")
		## Add some stuff to keep unique (in randomness we trust our lazy soul)
		inFile<- paste0(sample(1000:9000, length(fix)), exFile)
		## Copy
		file.copy(paste0("external/", exFile), paste0("figure/", inFile))
		## Fix md
		mdF[fix]  <- str_replace(mdF[fix], paste0("external/", exFile), paste0("figure/", inFile))
		cat("Files copied from external/ to figure/: ", exFile)
	}
	
	## Rename and label figures ######################################
	fix <- grep("^!\\[Fig", mdF)
	if(length(fix) > 0) {
		if(any(str_count(mdF[fix], "\\[Fig") > 1)) stop ("Execution halted! Each plot must be generated in a separate chunk. No chunk should produce two plots.", call. = FALSE)
		
		g   <- mdF[fix]
		bn <- str_replace(basename(g), ")", "")
		filedot <- lapply(lapply(str_locate_all(bn, pattern = "\\."), max),"-",1)
		fbase <- substring(bn, 1, filedot)
		numb  <- lapply(str_split(substr(g,7,11)," "),"[",1)
		
		## Fix reference in markdown
		mdF[fix] <- str_replace(g, paste0("figure/",fbase), paste0("figure/Fig_", numb))
		
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
	refs <- grep("<ref>",mdF)
	if(length(refs) > 0){
		nref <-  sum(str_count( mdF, "<ref>"))
		if(nref %% 2 != 0) stop("Execution halted! There's at least one '<ref>' tag which is not closed", call. = FALSE)
		cat("\n\nfound", nref/2, "cross-references to figures")
		
		for(i in refs){
			howMany <- str_count( mdF[i], "<ref>") 
			while(howMany > 0) {
				fi <- str_locate_all(mdF[i], "<ref>")[[1]]
				label <- substr(mdF[i], fi[1,"end"]+1, fi[2,"start"]-1)
				
				## Search label target
				fig <-  grep(paste0("<reflab>", label,"<reflab>"), mdF)
				
				## Security gate: does it exist and is it unique?
				if(length(fig) == 0) stop(paste0("Execution halted! Reference label '", label, "' does not exist. Use caption(x,label='yourLabel') to fix this"), call. = FALSE)
				if(length(fig) > 1)  stop(paste0("Execution halted! Reference label '", label, "' is not unique."), call. = FALSE)
				
				## Extract target numbering
				numb  <-   lapply(str_split(substr(mdF[fig],7,11)," "),"[",1)
				
				## Replace label by proper reference
				mdF[i] <- str_replace(mdF[i], paste0("<ref>",label,"<ref>"), paste0("Fig. ", numb))
				howMany <- str_count( mdF[i], "<ref>") 
			}
		}
		
		## Tidy captions
		cat("\ncross-references resolved")
		mdF <-  str_replace(mdF, "<reflab>.*<reflab>", "")
	}
	
	## Number captions ##############################################
	Hn <- grep("<H\\d>", mdF)
	
	if(length(Hn) > 0) {
		Hnum <- as.numeric(gsub('.+<H([0-9]+)>.+?$',"\\1",mdF[Hn]))
		RmdF <- readLines(Rmd, 20)
		chap <-  as.numeric(gsub("\\D","",RmdF[grep("currentChapter", RmdF)]))
		
		Hl <- c(chap,0,0,0,0)
		labs <- character(length(Hn))
		for(i in seq_along(Hn)){
			s <- Hnum[i]
			Hl[s] <- Hl[s]+1
			Hl[(s+1):5] <- 0
			if(s == 2) labs[i] <- paste0(Hl[1],".",Hl[[2]])
			if(s == 3) labs[i] <- paste0(Hl[1],".",Hl[2],".", Hl[3])
			#if(s == 4) labs[i] <- paste0(Hl[1],".",Hl[2],".", Hl[3],".", Hl[4]) ## pleagic doesn't want #4 numbered
		}
		
		cat("\n\nadded section counters to ", length(Hn), "headings (Headings >H3 won't be labelled)")
		mdF[Hn] <- str_replace(mdF[Hn], "<H\\d>", labs)
	}
	
	## Rewrite md
	writeLines(mdF, md)
	
	## Run pandoc
	if(withBib) {
		pcall <- paste("pandoc -sS", md, "-o", out, "--no-highlight --bibliography", bib, "--csl", style)
	} else {
		pcall <- paste("pandoc -sS", md, "-o", out, "--no-highlight")
	}
	
	cat("\n\nrunning pandoc with call: ", pcall)  
	system(pcall, intern = TRUE)
	cat("\n",rep("-",20),"\n")
	message("\nwrote ", x, ".docx")
	
}


