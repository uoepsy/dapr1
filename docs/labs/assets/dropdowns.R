
##### these are just adapted from msmbstyle, quite a lot has been cut...

qbegin<-function(label=NULL,qlabel=TRUE){
    if(!is.null(label)){label = as.character(label)}
    if(qlabel){qlab = "Question "}else{qlab = ""}
    if (knitr::is_html_output()) {
        id <- generate_idq2()
        part1 <- paste0("<div class=\'question-begin\'>",qlab,label,"</div>",
                        "<div class=\'question-body\'>")
        output <- structure(part1, format = "HTML", class = "knitr_kable")
    }
    if(knitr::is_latex_output()){
        id <- generate_idq2()
        part1 <- paste0("\\textbf{",qlab,label,"}")
        output <- part1
    }
    return(output)
}

qend<-function(){
    if (knitr::is_html_output()) {
        part2 <- paste0("</div><p class=\"question-end\"></p>")
        output <- structure(part2, format = "HTML", class = "knitr_kable")
    }
    if(knitr::is_latex_output()){
        part2 <- paste0("")
        output <- part2
    }
    return(output)
}

solbegin<-function(label=NULL,slabel=TRUE,show=TRUE,toggle=TRUE){
    if(slabel){slab = "Solution "}else{slab = ""}
    if(!is.null(label)){
        label = as.character(label)
        slab = paste0(slab, label)
    }
    if(show){
        if (knitr::is_html_output()) {
            id <- generate_id2()
            id1 <- paste0("sol-start-", id)
            id2 <- paste0("sol-body-", id)
            part1 <- paste0("<div class=\"solution-begin\">",
                            ifelse(toggle, sprintf("<span id='%s' class=\"fa fa-chevron-circle-right solution-icon clickable\" onclick=\"toggle_visibility('%s', '%s')\">  %s</span>",id1, id2, id1, slab),""),
                            "</div>",
                            ifelse(toggle, paste0("<div class=\"solution-body\" id = \"",id2, "\" style=\"display: none;\">"), "<div class=\"solution-body\">")
            )
            output <- structure(part1, format = "HTML", class = "knitr_kable")
        }
        if(knitr::is_latex_output()){
            id <- generate_id2()
            part1 <- paste0("\\textbf{",slab,label,"}")
            output <- part1
        }
    } else {
        if (knitr::is_html_output()){
            part1 <- paste0("<div style=\"display:none;\">")
            output <- structure(part1, format = "HTML", class = "knitr_kable")
        }
        if(knitr::is_latex_output()){
            id <- generate_id2()
            part1 <- paste0("\\iffalse")
            output <- part1
        }
    }
    return(output)
}

solend<-function(show=TRUE){
    if (knitr::is_html_output()) {
        part2 <- "</div><p class=\"solution-end\"></p>"
        output <- structure(part2, format = "HTML", class = "knitr_kable")
    }
    if(knitr::is_latex_output()){
        if(show){
            part2 <- ""
            output <- part2
        }else{
            part2 <- paste0("\\fi")
            output <- part2
        }
    }
    return(output)
}



generate_id2 <- function() {
    f1 <- file.path(tempdir(), "solution_idx")
    
    id <- ifelse(file.exists(f1), readLines(f1), "1")
    id_new <- as.character(as.integer(id) + 1)
    writeLines(text = id_new, con = f1)
    
    return(id)
}
generate_idq2 <- function() {
    f1 <- file.path(tempdir(), "question_idx")
    
    id <- ifelse(file.exists(f1), readLines(f1), "1")
    id_new <- as.character(as.integer(id) + 1)
    writeLines(text = id_new, con = f1)
    
    return(id)
}



# Optional

optbegin<-function(label=NULL,olabel=TRUE,show=TRUE, toggle=TRUE){
    if(olabel){olab = "Optional "}else{olab = ""}
    if(!is.null(label)){
        label = as.character(label)
        olab = paste0(olab, label)
    }
    if(show){
        if (knitr::is_html_output()) {
            id <- generate_id2()
            id1 <- paste0("opt-start-", id)
            id2 <- paste0("opt-body-", id)
            part1 <- paste0("<div class=\"optional-begin\">",
                            ifelse(toggle, sprintf("<span id='%s' class=\"fa fa-chevron-circle-right optional-icon clickable\" onclick=\"toggle_visibility('%s', '%s')\">  %s</span>",id1, id2, id1, olab),""),
                            "</div>",
                            ifelse(toggle, paste0("<div class=\"optional-body\" id = \"",id2, "\" style=\"display: none;\">"), "<div class=\"optional-body\">")
            )
            output <- structure(part1, format = "HTML", class = "knitr_kable")
        }
        if(knitr::is_latex_output()){
            id <- generate_id2()
            part1 <- paste0("\\textbf{",olab,label,"}")
            output <- part1
        }
    } else {
        if (knitr::is_html_output()){
            part1 <- paste0("<div style=\"display:none;\">")
            output <- structure(part1, format = "HTML", class = "knitr_kable")
        }
        if(knitr::is_latex_output()){
            id <- generate_id2()
            part1 <- paste0("\\iffalse")
            output <- part1
        }
    }
    return(output)
}

optend<-function(show=TRUE){
    if (knitr::is_html_output()) {
        part2 <- "</div><p class=\"optional-end\"></p>"
        output <- structure(part2, format = "HTML", class = "knitr_kable")
    }
    if(knitr::is_latex_output()){
        if(show){
            part2 <- ""
            output <- part2
        }else{
            part2 <- paste0("\\fi")
            output <- part2
        }
    }
    return(output)
}
