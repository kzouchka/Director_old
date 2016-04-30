# Director R package for creating directed regulatory cascades.
pkg.env <- new.env()
pkg.env$absoluteDirectory <- getwd()
pkg.env$sankeyPlot <- NULL
pkg.env$sankeyHTML <- NULL
pkg.env$nodMin <- "blue"
pkg.env$nodeMax <- "red"
pkg.env$pathMin <- "blue"
pkg.env$pathMax <- "red"
pkg.env$noughtColor <- "#f5f5f0"
pkg.env$nought <- 0
pkg.env$params <- list(id="ID", height=1000, width=1000)
pkg.env$dependencies <- list(css=NULL, layout=NULL, d3=NULL, sankey=NULL)

#' initSankey
#'
#' Internally generates supporting JavaScript and CSS files.
#' @param pathOpacity Opacity of connecting path between nodes in the figure.
#' @param pathHover Opacity of connecting path between nodes upon mouseover.
#' @param font Font used for the node names and additional mouseover text in figure.
#' @param fontsize Pixel font size used for the visible node names. Use to adjust range of font sizes (with proportions) or to set a single font size when fontsizeProportion is disabled.
#' @param fontsizeProportion Boolean to enable/disable text being proportional to node widths. When enabled, all node names will appear with parameter fontsize.
#' @param d3js Path to download latest zip version of D3 library. e.g. https://github.com/mbostock/d3/releases/download/v3.5.16/d3.zip. See http://www.d3js.org for more details. If NULL, will use version 3.5.16 currently installed with Director.
#' @param sankeyjsFile Path to download sankey javascript file. If NULL, will use version installed with Director (https://raw.githubusercontent.com/d3/d3-plugins/master/sankey/sankey.js)
#' @param d3jsMethod Function method to use to download D3 library. ?download.file for more detail on parameter.
#' @param sankeyjsMethod Function method to use to download sankey script. ?download.file for more detail on parameter.
#' @keywords download sankey
#' @export
#' @import htmltools
#' @import BiocInstaller
#' @import utils
#' @import grDevices
#' @examples
#' initSankey("temp") # Creates a temp folder to output HTML files and necesary dependencies.
initSankey <- function(pathOpacity=0.2, pathHover=0.5, font="lato, helvetica, sans-serif", fontsize=NULL, fontsizeProportion=TRUE, d3js=NULL, sankeyjsFile=NULL, d3jsMethod="auto", sankeyjsMethod="auto") {

    if (is.null(fontsize)) {
        FontInfo <- paste("font-family: ", font, ";
        font-weight: 500;", sep="")
    } else {
        FontInfo <- paste("font: ", fontsize, "px ", font, ";", sep="")
    }

    if (fontsizeProportion) {
        fontsizeProportioned <- '
    .style("font-size",
    function() {
        var textSize = 117 * this.__data__.value;
            if (textSize > 117) {
                return "117%"
            } else {
                return textSize.toString() + "%";
            }
    })'
    } else {
        fontsizeProportioned <- ""
    }

    pathOpacText <- paste("stroke_opacity = ", pathOpacity, ";", sep="")
    pathOpacTextcss <- paste("stroke-opacity: ", pathOpacity, ";", sep="")
    pathHovText <- paste("stroke_opacity = ", pathHover, ";", sep="")
    pathHovTextcss <- paste("stroke-opacity: ", pathHover, ";", sep="")
    fontText <- paste("font-family: ", font, ";", sep="")
    # Customize required supporting files with user-defined parameters using htmltools
    cssScript <- htmlTemplate(paste(system.file("www", package="Director"), "/css/sankey.css", sep=""), pathOpacity = pathOpacTextcss, pathHover = pathHovTextcss, FontInfo = FontInfo, font = fontText)
    layoutHTML <- htmlTemplate(paste(system.file("www", package="Director"), "/layouts/sankey.html", sep=""), Params = HTML(as.character("{{ Params }}")), fontsizeProportioned = HTML(fontsizeProportioned), pathOpacity=pathOpacText, pathHover=pathHovText)        

    pkg.env$dependencies$css <- cssScript
    pkg.env$dependencies$layout <-  layoutHTML
    
    # Write JS files temporarily to working directory
    if (length(grep(".zip",d3js)) == 1) { # need to unzip!
            # download D3 JavaScript library from server
            d3js <- gsub("https", "http", d3js) # ensure wget works
            download.file(d3js, paste(pkg.env$absoluteDirectory,"d3.zip",sep="/"), method=d3jsMethod)
            # unload library
            unzip(paste(pkg.env$absoluteDirectory,"d3.zip", sep="/"), exdir="temp")
            system(paste("mv ", pkg.env$absoluteDirectory, "/temp/d3.js ", pkg.env$absoluteDirectory, "/temp/d3.v3.js", sep=""))
            pkg.env$dependencies$d3 <- HTML(paste(readLines(paste(pkg.env$absoluteDirectory, "temp/d3.v3.js", sep="/"), warn=FALSE), collapse="\n"))
            system(paste("rm -rf ", "temp", sep=""))
            system(paste("rm ", pkg.env$absoluteDirectory, "/d3.zip", sep=""))
        } else if (is.null(d3js)) { # Use installed version
            d3js <- NULL
        } else {
            stop("D3 javascript library (v3) zip file is needed to produce HTML files correctly. See http://d3js.org for more details.")
        }

    if (is.null(sankeyjsFile)) { # use installed version
            sankeyjsFile <- NULL
        } else {
            # Download Sankey JavaScript file
            download.file(sankeyjsFile, paste(pkg.env$absoluteDirectory,"sankey.js", sep="/"), method=sankeyjsMethod)
            pkg.env$dependencies$sankey <- HTML(paste(readLines(paste(pkg.env$absoluteDirectory, "sankey.js", sep="/"), warn=FALSE), collapse="\n"))
            system(paste("rm ", pkg.env$absoluteDirectory, "/sankey.js", sep=""))
        }
        
}

#' append2List
#'
#' Appends a data frame containing additional relationship information to an existing List having 6 columns: source, target, description, value, sourcefc, targetfc. Order matters! For example, add a map of transcripts to genes to a List of miRNAs and their target transcripts so that the final List connects miRNAs -> transcripts -> genes.
#' @return a combined List.
#' @param List Data frame containing the necessary columns above. e.g. Formatted with createList function.
#' @param appendList Data frame or matrix to append to List.
#' @param source Column name of appendList corresponding to the sources to append.
#' @param target Column name of appendList corresponding to the targets to append.
#' @param value Column name of appendList correspondig to the relationship values to append.
#' @param description Column name of appendList corresponding to the descriptions to append.
#' @param sourcefc Column name of appendList corresponding to the sourcefcs to append.
#' @param targetfc Column name of appendList correspondig to the targetfcs to append.
#' @param appendMatch Filter and remove 1) rows in List that contain targets without a corresponding source in appendList, and 2) rows in appendList that contain sources without a corresponding target in List.
#' @keywords append
#' @export
#' @examples
#' tempList <- createList(data.frame(source=c("A","B","C"),
#'     target=c("D","E","G"),
#'     description=c("consonant","vowel","consonant"),
#'     value=runif(3,-1,1),
#'     sourcefc=runif(3,-2,2),
#'     targetfc=runif(3,-2,2)))
#' tempAppendList <- data.frame(source="D",target="I",
#'     description="vowel",value=runif(1,-1,1),
#'     sourcefc=runif(1,-2,2), targetfc=runif(1,-2,2))
#' append2List(tempList,tempAppendList) # Will combine only 1 row from each list.
#' append2List(tempList,tempAppendList, appendMatch=FALSE) # Will combine all rows
append2List <- function(List, appendList, description="description", sourcefc="sourcefc", targetfc="targetfc", value="value", target="target", source="source", appendMatch = TRUE) {
    if (is.null(List)) {
        stop("Input List is missing.")
    }

    if (appendMatch) { # FILTER and remove rows in List that contain targets without a corresponding source in appendList
        transcriptsToRemove <- c()
        for (t in 1:nrow(List)) {
            if (length(grep(paste("^", List[t,target],"$",sep=""),appendList[,source])) == 0) {
                transcriptsToRemove <- c(transcriptsToRemove, t)
            }
        }
        transcriptsToRemove2 <- c()
        for (t in 1:nrow(appendList)) {
            if (length(grep(paste("^",appendList[,source][t], "$",sep=""),List[,target])) == 0) {
                transcriptsToRemove2 <- c(transcriptsToRemove2,t)
            }
        }

        if (length(transcriptsToRemove) > 0) {
            List <- List[-transcriptsToRemove,]
        }
        if (length(transcriptsToRemove2) > 0) {
            appendList <- appendList[-transcriptsToRemove2,]
        }
    }
        return(rbind(List,appendList))
}

#' createList
#'
#' Take a subset of the input data frame or matrix corresponding to the required Sankey values.
#' @return a data.frame List
#' @param inputList Data frame or matrix containing the necessary parameters described below.
#' @param inputFC Data frame or matrix containing node names (source and target) and corresponding quantitative values. If this input is defined, then input-specific parameters 'node' and 'fc' should be defined. Inputs 'source', 'target', 'description' and 'value' are still referenced from inputList.
#' @param node Column name of inputFC containing names to display of source and target nodes. Paths defined in inputList identify which nodes are sources and which are targets.
#' @param fc Column name of inputFC containing quantitative values representing the nodes.
#' @param source Column name of inputList containing names to display of starting nodes. Paths are drawn from these points to their corresponding target nodes.
#' @param target Column name of inputList containing names to display of destination nodes. Paths are drawn to these point from their corresponding source nodes.
#' @param description Optional column name of inputList containing additional information about connection, e.g. the gene name of a transcript target node, or family name of related target genes.
#' @param value Column name of inputList containing quantitative values representing the relationship between sources and targets.
#' @param sourcefc Column name of inputList containing quantitative values representing the sources.
#' @param targetfc Column name of inputList containing quantitative values representing the targets.
#' @keywords input
#' @export
#' @examples
#' nodevals <- runif(5,-2,2)
#' tempList <- data.frame(source=c("A","B","C","D"),
#'     target=c("C","D","E","E"),
#'     addedInfo=c("c","d","vowel","vowel"),
#'     relationValue=runif(4,-1,1),
#'     sourceValue=nodevals[1:4],
#'     targetValue=nodevals[c(3,4,5,5)])
#' tempFC <- data.frame(genes=c("A","B","C","D","E"), foldChange=runif(5,-2,2))
#' # inputList only
#' createList(tempList, description="addedInfo", value="relationValue", 
#' sourcefc="sourceValue", targetfc="targetValue")
#' # inputList and inputFC
#' createList(tempList, tempFC, value="relationValue",sourcefc="sourceValue",
#' targetfc="targetValue")
createList <- function(inputList, inputFC=NULL, node="genes", fc="foldChange", source="source", target="target",description="description",value="value", sourcefc="sourcefc", targetfc="targetfc") {
    if (is.null(dim(inputList))) {
        stop("inputList is not a data frame or table.")
    } else {
        if (is.null(dim(inputFC))) { # No inputFC defined, all information is in inputList
            if (length(grep(source, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", source, sep=""))
            }
            if (length(grep(target, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", target, sep=""))
            }
            if (length(grep(value, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", value, sep=""))
            }
            if (length(grep(sourcefc, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", sourcefc, sep=""))
            }
            if (length(grep(targetfc, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", targetfc, sep=""))
            }
            if (length(grep(description, colnames(inputList))) == 0) {
                description <- rep("",nrow(inputList)) # description is not a mandatory input
                edgelist <- data.frame(source=inputList[,source], target=inputList[,target], description=description, value=inputList[,value], sourcefc=inputList[,sourcefc], targetfc=inputList[,targetfc], stringsAsFactors=FALSE)
                colnames(edgelist) <- c("source","target","description","value","sourcefc","targetfc")
            } else {
                edgelist <- data.frame(inputList[,c(source, target, description, value, sourcefc, targetfc)], stringsAsFactors=FALSE)
                colnames(edgelist) <- c("source","target","description","value","sourcefc","targetfc")
                edgelist$description <- paste("<br>",edgelist$description)
            }
            edgelist <- edgelist[which(!duplicated(edgelist)),]
            # make sure no duplicate source and target with differing values
            if (length(which(duplicated(edgelist[,c("source","target")]))) > 0) {
                duplSource <- edgelist$source[which(duplicated(edgelist[,c("source","target")]))]
                duplTarget <- edgelist$target[which(duplicated(edgelist[,c("source","target")]))]
                stop(paste("Duplicate source-target pair found on lines ", paste(intersect(grep(paste("^",duplSource,"$",sep=""),edgelist$source),grep(paste("^",duplTarget,"$",sep=""),edgelist$target)), sep=","), ". Please verify values.", sep=""))
            }
        } else { # InputFC for source, target, sourcefc, targetfc. inputList for source, target, value.
            if (length(grep(source, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", source, sep=""))
            }
            if (length(grep(target, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", target, sep=""))
            }
            if (length(grep(value, colnames(inputList))) == 0) {
                stop(paste("inputList does not contain the column ", value, sep=""))
            }
            if (length(grep(node, colnames(inputFC))) == 0) {
                stop(paste("inputFC does not contain the column ", node, sep=""))
            }
            if (length(grep(fc, colnames(inputFC))) == 0) {
                stop(paste("inputFC does not contain the column ", fc, sep=""))
            }
            inputfc <- data.frame(inputFC[,c(node, fc)])
            if (length(grep(description, colnames(inputList))) == 0) {
                description <- rep("",nrow(inputList)) # description is not a mandatory input
                inputlist <- data.frame(source=inputList[,source], target=inputList[,target], description=description, value=inputList[,value], stringsAsFactors=FALSE)
                colnames(inputlist) <- c("source","target","description","value")
            } else {
                inputlist <- data.frame(inputList[,c(source, target, description, value)])
                colnames(inputlist) <- c("source","target","description","value")
                inputlist$description <- paste("<br>",inputlist$description)
            }
            sourceFC <- c()
            targetFC <- c()
            if (sum(duplicated(inputfc[,node])) != 0) {
                stop(paste("inputFC column '", node, "' does not contain unique names.", sep=""))
            }
            for (r in 1:nrow(inputlist)) {
                if (length(grep(paste("^",gsub("[*]","[*]",inputlist[r,"source"]),"$", sep=""), inputfc[,node])) == 0) {
                    message(paste("inputFC does not contain a node called ", inputlist[r,"source"], sep=""))
                }
                if (length(grep(paste("^",gsub("[*]","[*]",inputlist[r,"target"]),"$", sep=""), inputfc[,node])) == 0) {
                    message(paste("inputFC does not contain a node called ", inputlist[r,"target"], sep=""))
                }
                sourceFC <- c(sourceFC, inputfc[grep(paste("^",gsub("[*]","[*]",inputlist[r,"source"]),"$", sep=""), inputfc[,node]), fc])
                targetFC <- c(targetFC, inputfc[grep(paste("^", gsub("[*]","[*]",inputlist[r,"target"]),"$", sep=""),inputfc[,node]), fc])
            }
            if ((length(sourceFC) != length(targetFC)) || (length(sourceFC) != nrow(inputlist)) || (length(targetFC) != nrow(inputlist))) {
                stop(paste("Incorrect number of nodes defined for given inputList. ", length(sourceFC), " source nodes, ", length(targetFC), " target nodes, and ", nrow(inputlist), " interactions detected.", sep=""))
            }
            edgelist <- cbind(inputlist, data.frame(sourcefc=sourceFC, targetfc=targetFC))
        }
        return(edgelist)
    }
}


#' drawSankey
#'
#' Create an HTML document that can be viewed and saved to file. Diagram properties can be modified in this function, makeSankey() and initSankey().
#' @return HTML document containing diagram.
#' @param List Data frame containing 6 columns: source, target, description, value, sourcefc, targetfc.
#' @param height Pixel height of the figure to draw. If empty, the figure will be given a pixel height proportional to the number of rows in List up to a maximum 1800px or minimum of 300px. These can be overridden by defining this parameter.
#' @param width Pixel width of the figure to draw. By default, 1000px.
#' @param caption Sankey figure caption. HTML formatting is possible.
#' @param legendfont Font of the legend text.
#' @param legendsize Font size of the legend text.
#' @param nodeValue Description of node scale in legend.
#' @param pathValue Description of path scale in legend.
#' @param directory Absolute path to output directory. If null, the working directory obtained from getwd() will be used. This is required if D3 and sankey JS files were downloaded with initSankey().
#' @keywords draw sankey
#' @export
#' @examples
#' Level1 <- createList(poorprog$Level1)
#' Level2 <- createList(poorprog$Level2)
#' tempList <- append2List(Level1,Level2)
#' initSankey()
#' tempList2 <- makeSankey(tempList, averagePath=TRUE)
#' sankey <- drawSankey(tempList2)
#' library(htmltools) # can also be launched with
#' html_print(sankey)
drawSankey <- function(List, height=NULL, legendfont="sans-serif", legendsize=12, width=1000, caption="Sankey figure", nodeValue="node values", pathValue="path values", directory = NULL) {
    if (is.null(pkg.env$dependencies$css) || is.null(pkg.env$dependencies$layout)) {
        stop("Dependency files missing. Have you run initSankey() before this function?")
        }
    # make sure List is formatted correctly
    if (length(List) != 7) {
        stop("A List with a set number of columns must be provided. See ?drawSankey for details or run makeSankey() and try again.")
    }
    trueVals <- List$reference$truevalue
    FC <- c(List$reference$sourcefc, List$reference$targetfc)

    # Calculate legend values to display
    if (length(which(List$reference$targetfc == min(FC))) > 0) {
        FCmin <- List$targetRange[which(List$targetDomain == List$reference[which(List$reference$targetfc == min(FC)),"target"][1])]
    } else {
        FCmin <- List$sourceRange[which(List$sourceDomain == List$reference[which(List$reference$sourcefc == min(FC)),"source"][1])]
    }
    if (length(which(List$reference$targetfc == max(FC))) > 0) {
        FCmax <- List$targetRange[which(List$targetDomain == List$reference[which(List$reference$targetfc == max(FC)),"target"][1])]
    } else {
        FCmax <- List$sourceRange[which(List$sourceDomain == List$reference[which(List$reference$sourcefc == max(FC)),"source"][1])]
    }

    # Do legend colour range span nought?
    corrBar <- ""; FCbar <- ""
    if ((max(trueVals) < pkg.env$nought && min(trueVals) < pkg.env$nought) ||(max(trueVals) > pkg.env$nought && min(trueVals) > pkg.env$nought)) {
        corrBar <- "');
        grd.addColorStop(1, '"
    } else {
        corrBar <- paste("');
        grd.addColorStop(",round(abs(min(trueVals))/(abs(max(trueVals))+abs(min(trueVals))),2),", '", pkg.env$noughtColor,"');
        grd.addColorStop(1, '",sep="")
    }

    if ((max(FC) < 0 && min(FC) < 0) ||(max(FC) > 0 && min(FC) > 0)) {
    FCbar <- "');
        grd.addColorStop(1, '"
    } else {
    FCbar <- paste("');
        grd.addColorStop(",round(abs(min(FC))/(abs(max(FC))+abs(min(FC))),2),", '", pkg.env$noughtColor,"');
        grd.addColorStop(1, '",sep="")
    }

    # Create a unique 10-letter ID for the diagram div
    idmaker <- function() { 
        lets <- toupper(sample(letters,sample(2:10,1), replace=T))
        nums <- sample(0:9,10-length(lets), replace=T)
        ids <- paste(c(lets[1], sample(c(nums,lets[2:length(lets)]),9)), collapse="") # always start with a letter
      return(ids)
      }
    pkg.env$params$id <- paste("Dir", idmaker(),sep="")

    # Create additional legend and caption for 
    customScript <- paste("
    <script>
    var color = d3.scale.ordinal()
        .domain(['", paste(List$valDomain,collapse="','"),"'])
        .range(['", paste(List$valRange, collapse="','"),"']);

    var color2 = d3.scale.ordinal()
        .domain(['", paste(List$targetDomain,collapse="','"),"','", paste(List$sourceDomain,collapse="','"),"'])
        .range(['", paste(List$targetRange, collapse="','"),"','", paste(List$sourceRange, collapse="','"),"']);

    d3.selectAll('#", pkg.env$params$id, " svg path.link')
        .style('stroke', function(d){
            return color(d.value);
        })
        d3.selectAll('#", pkg.env$params$id, " svg .node rect')
        .style('fill', function(d){
            return color2(d.name)
        })
        .style('stroke', 'none')
    </script>

    <table width=",width, " align=center><tr><td>
    <p style='font-family:", legendfont, "; font-size:", legendsize, "px' align='center'><b>", pathValue,"<br>
    ",round(min(trueVals),2), " <canvas id='myCanvas' width='150' height='20' style='border: 0px;'>
    Your browser does not support the HTML5 canvas tag.</canvas> ", round(max(trueVals),2), "</b>
    </p>
    <script>
    var c = document.getElementById('myCanvas');
    var ctx = c.getContext('2d');
    var grd = ctx.createLinearGradient(0, 0, 170, 0);
    grd.addColorStop(0, '",List$valRange[grep(List$reference$value[which(trueVals==min(trueVals))[1]], List$valDomain)],corrBar,List$valRange[grep(List$reference$value[which(trueVals==max(trueVals))[1]], List$valDomain)],"');
    ctx.fillStyle = grd;
    ctx.globalAlpha = 0.45;
    ctx.fillRect(0,0,150,20);
    </script>
    </td><td>
    <p style='font-family:", legendfont, "; font-size:", legendsize, "px' align='center'><b>", nodeValue, "<br>
    ",round(min(FC),2), " <canvas id='myCanvas2' width='150' height='20' style='border:0px;'>
    Your browser does not support the HTML5 canvas tag.</canvas> ", round(max(FC),2) , "</b>
    </p>
    <script>
    var c = document.getElementById('myCanvas2');
    var ctx = c.getContext('2d');
    var grd = ctx.createLinearGradient(0, 0, 170, 0);
    grd.addColorStop(0, '",FCmin,FCbar,FCmax,"');
    ctx.fillStyle = grd;
    ctx.fillRect(0,0,150,20);
    </script>
    <tr><td colspan=2><p style='font-family:", legendfont, "; font-size:", legendsize+2, "px' align='center'><b>", caption, "</b></p></td></tr>
    </td></tr></table>
    ", sep="")


        # Calculate height of figure based on number of connections to map
        if (is.null(height)) {
            if (nrow(List$reference)*30 >= 1800) {
                warning(paste("Number of interacting nodes is large (",nrow(List$reference),"). If figure is not clear, consider rerunning this function with a defined parameter 'height' > 1800px or using a smaller subset of nodes.", sep=""))
            }
            H <- min(nrow(List$reference)*30, 1800) # predefine a height
            H <- max(300,H) # predefine minimum height
        } else if (!is.numeric(height)) {
            stop("Given height parameter is non-numeric.")
        } else {
            H <- height
        }

    # Create List$reference in data array format recognized by D3.
    dataList <- paste('source: [ "', paste(List$reference$source, collapse='", "'), '" ],
        target: [ "', paste(List$reference$target, collapse='", "'), '" ],
        description: [ "', paste(List$reference$description, collapse='", "'), '" ],
        value: [ ', paste(List$reference$value, collapse=', '), ' ],
        sourcefc: [ ', paste(List$reference$sourcefc, collapse=', '), ' ],
        targetfc: [ ', paste(List$reference$targetfc, collapse=', '), ' ],
        truevalue: [ ', paste(List$reference$truevalue, collapse=', '), ' ],
        score_info: [ "', paste(List$reference$score_info, collapse='", "'), '" ]',
        sep="")

    pkg.env$params$height <- H
    pkg.env$params$width <- width
    Params <- list(dom=pkg.env$params$id,
                      width= width,
                      height= H,
                      data= dataList,
                      nodeWidth= 15, # default parameters
                      nodePadding= 10,
                      layout= 32,
                      id=pkg.env$params$id
    )

    ParamsInput <- paste('var params = {  
    dom: "', Params$dom, '",
    width: ', Params$width, ",
    height: ", Params$height, ",
    data: {", Params$data, " },
    nodeWidth: ", Params$nodeWidth, ",
    nodePadding: ", Params$nodePadding, ",
    layout: ", Params$layout, ',
    id: "', Params$id, '" };',sep="")

    layoutHTML <- htmlTemplate(text_=as.character(pkg.env$dependencies$layout), Params = ParamsInput)
    
    # Used by shiny
    pkg.env$sankeyPlot <- HTML(paste(layoutHTML, HTML(customScript), sep="\n")) 
 
    # Use htmltools to create and render a full HTML page.
    if (is.null(pkg.env$dependencies$sankey)) {
        sankeyJS <- paste(system.file(package="Director"), "/www/js", sep="")
    } else {
        if (is.null(directory)) { # Write to working directory
            if (!dir.exists(pkg.env$absoluteDirectory)) {
                stop(paste("Path ", pkg.env$absoluteDirectory, " does not exist.", sep=""))
            }
        } else { # Write to custom directory
            if (!dir.exists(directory)) {
                stop(paste("Path ", directory, " does not exist.", sep=""))
            } else {
                pkg.env$absoluteDirectory <- directory
            }
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www",sep="/"))
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www/js",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www/js", sep="/"))
        }
        sankeyJS <- paste(pkg.env$absoluteDirectory, "www/js", sep="/")
        sankeyJSconn <- file(paste(sankeyJS, "sankey.js", sep="/"))
        cat(HTML(as.character(pkg.env$dependencies$sankey)), file = sankeyJSconn)
        close(sankeyJSconn)
    }
    if (is.null(pkg.env$dependencies$d3)) {
        d3JS <- paste(system.file(package="Director"), "/www/js", sep="")
    } else {
        if (is.null(directory)) { # Write to working directory
            if (!dir.exists(pkg.env$absoluteDirectory)) {
                stop(paste("Path ", pkg.env$absoluteDirectory, " does not exist.", sep=""))
            }
        } else { # Write to custom directory
            if (!dir.exists(directory)) {
                stop(paste("Path ", directory, " does not exist.", sep=""))
            } else {
                pkg.env$absoluteDirectory <- directory
            }
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www",sep="/"))
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www/js",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www/js", sep="/"))
        }
        d3JS <- paste(pkg.env$absoluteDirectory, "www/js", sep="/")
        d3JSconn <- file(paste(d3JS, "d3.v3.js", sep="/"))
        cat(HTML(as.character(pkg.env$dependencies$d3)), file = d3JSconn)
        close(d3JSconn)
    }

    # Template HTML
    htmlContent <- '{{ Diagram }}
    {{ sankeyPlot }}
<p>Use <em>writeSankey</em> to save this diagram as a portable HTML file.</p>'

    headStyle <- paste('<style>
    ', sprintf("
            .d3-sankey {
                width: %s;
                height: %s;
                display: block;
                margin-left: auto;
                margin-right: auto;}",
                validateCssUnit(pkg.env$params$width),
                validateCssUnit(pkg.env$params$height)), '
    ', pkg.env$dependencies$css, '
</style>', sep="")

    htmlDocument <- htmlTemplate(text_=htmlContent, document_=TRUE, Diagram = attachDependencies(div(id=pkg.env$params$id, class="d3-sankey"), htmlDependency(name="d3", version="3.5.6", c(href=d3JS), script="d3.v3.js",  head=headStyle)), sankeyPlot= attachDependencies(pkg.env$sankeyPlot, htmlDependency(name="d3-sankey", version="3.5.6", c(href=sankeyJS), script="sankey.js")))
    
    pkg.env$sankeyHTML <- htmlDocument

    html_print(htmlDocument)
    return(pkg.env$sankeyHTML)
}

#' filterRelation
#'
#' Filter source-target relationships in List for a specific type: inversely related sourcefc-targetfc pairs only (inverseFC), positively related sourcefc-targetfc pairs only (correlatedFC), negative value scores only (inverseValue), or positive value scores only (correlatedValue). Default is to not apply any filtering.
#' @return a filtered List.
#' @param List Data frame containing 6 columns: source, target, description, value, sourcefc, targetfc.
#' @param relation One of: none, inverseFC, correlatedFC, inverseValue, correlatedValue. Default is none.
#' @param sourcefc Column name of List corresponding to sourcefcs to filter.
#' @param targetfc Column name of List correspondig to targetfcs to filter.
#' @param value Column name of List correspondig to value to filter.
#' @keywords filter
#' @export
#' @examples
#' tempList <- createList(data.frame(source=c("A","B","C"),
#'     target=c("D","E","G"),
#'     description=c("consonant","vowel","consonant"),
#'     value=runif(3,-1,1),
#'     sourcefc=runif(3,-2,2),
#'     targetfc=runif(3,-2,2)))
#' filterRelation(tempList,"inverseValue")
#' filterRelation(tempList,"correlatedValue")
#' filterRelation(tempList,"inverseFC")
#' filterRelation(tempList,"correlatedFC")
filterRelation <- function(List, relation=c("none", "inverseFC", "correlatedFC", "inverseValue", "correlatedValue"), sourcefc="sourcefc", targetfc="targetfc", value="value") {
    if (is.null(List)) {
        stop("Input List is missing.")
    }
    relation <- match.arg(relation)
    # FILTER to remove source-target relationships that are not defined as follows:
    negcorr <- data.frame(sourcefc=(List[,sourcefc] < 0),targetfc=(List[,targetfc] < 0), stringsAsFactors=FALSE)
    poscorr <- data.frame(sourcefc=(List[,sourcefc] > 0),targetfc=(List[,targetfc] > 0), stringsAsFactors=FALSE)
    fc_filter <- c(which(rowSums(negcorr) == 2),which(rowSums(poscorr) == 2))
    fc_filter2 <- which(List[,value] > 0)
    if (length(fc_filter) == 0 && (relation == "inverseFC" || relation == "correlatedFC")) {
        message("Note: all sourcefc-targetfc pairs are inversely related.")
    }
    if (length(fc_filter) ==nrow(List) && (relation == "inverseFC" || relation == "correlatedFC")) {
        message("Note: all sourcefc-targetfc pairs are positively related.")
    }
    if (length(fc_filter2) == 0 && (relation == "inverseValue" || relation == "correlatedValue")) {
        message("Note: all source-target relation values are negative.")
    }
    if (length(fc_filter2) ==nrow(List) && (relation == "inverseValue" || relation == "correlatedValue")) {
        message("Note: all source-target relation values are positive.")
    }
    if (relation == "inverseFC") { # inversely related sourcefc-targetfc pairs only
        if (length(fc_filter) > 0) {
            List <- List[-fc_filter,]
        }
    } else if (relation == "correlatedFC") { # positively related sourcefc-targetfc pairs only
        List <- List[fc_filter,]
    } else if (relation == "inverseValue") { # negative value scores only
        if (length(fc_filter2) > 0) {
            List <- List[-fc_filter2,]
        }
    } else if (relation == "correlatedValue") { # positive value scores only
        List <- List[fc_filter2,]
    } else if (relation == "none") {
        warning("No source-target filtering applied.")
    } else {
        stop("Parameter 'relation' option not recognized. Valid options are 'none', 'inverseFC', 'correlatedFC', 'inverseValue', 'correlatedValue'")
    }
    return(List)
}

#' filterNumeric
#'
#' Filter a quantitave column in List for minimum, maximum, or absolute value.
#' @return a filtered List
#' @param List Data frame containing 6 columns: source, target, description, value, sourcefc, targetfc.
#' @param column Name of column in List to filter.
#' @param min Minimum value to filter for in column.
#' @param max Maximum value to filter for in column.
#' @param absolute Absolute value to filter for in column.
#' @keywords filter
#' @export
#' @examples
#' tempList <- createList(data.frame(source=c("A","B","C"),
#'     target=c("D","E","G"),
#'     description=c("consonant","vowel","consonant"),
#'     value=runif(3,-1,1),
#'     sourcefc=runif(3,-2,2),
#'     targetfc=runif(3,-2,2)))
#' filterNumeric(tempList,"sourcefc", absolute=0.5)
#' filterNumeric(tempList, "targetfc", max=0) # only take down-regulated targets
filterNumeric <- function(List, column, min=NULL,max=NULL, absolute=NULL) {
    if (is.null(List) || is.null(column)) {
        stop("One or both of 'List' and 'column' are missing.")
    }
    if (!is.null(absolute)) {
        toKeep <- which(abs(List[,column]) >= absolute)
        List <- List[toKeep,]
    }
    if (!is.null(min)) {
        toKeep <- which(List[,column] >= min)
        List <- List[toKeep,]
    }
    if (!is.null(max)) {
        toKeep <- which(List[,column] <= max)
        List <- List[toKeep,]
    }
    if (is.null(min) && is.null(max) && is.null(absolute)) {
        warning("No filtering performed as no filtering parameter (min, max, absolute) was given")
    }
    return(List)
}

#' filterSubset
#'
#' Filter up to two qualitative columns (source and target) in List for a subset of names.
#' @return a filtered List
#' @param List Data frame containing 6 columns: source, target, description, value, sourcefc, targetfc.
#' @param source Column name of List containing source names.
#' @param target Column name of List containing target names.
#' @param sourceSubset Vector of source names to keep.
#' @param targetSubset Vector of target names to keep.
#' @param invert Take the inverse selection of defined subset.
#' @param join If both subsets are defined, take either union or intersect of subsets found.
#' @keywords filter
#' @export
#' @examples
#' tempList <- createList(data.frame(source=c("A","B","C"),
#'     target=c("D","E","G"),
#'     description=c("consonant","vowel","consonant"),
#'     value=runif(3,-1,1),
#'     sourcefc=runif(3,-2,2),
#'     targetfc=runif(3,-2,2)))
#' filterSubset(tempList,source="source", target="description", 
#' sourceSubset="C", targetSubset="consonant")
#' filterSubset(tempList,target="description", targetSubset="consonant")
#' filterSubset(tempList,target="description", targetSubset="consonant", invert=TRUE)
filterSubset <- function(List, sourceSubset=NULL, targetSubset=NULL,invert=FALSE, source="source",target="target", join=c("union","intersect")) {
    if (is.null(List)) {
        stop("Input List is missing.")
    }
    if (invert != FALSE && invert != TRUE) {
        stop("Parameter 'invert' is boolean: TRUE or FALSE")
    }
    found <- c()
    found2 <- c()
    if (!is.null(sourceSubset)) {
        found <- grep(paste(paste("^",sourceSubset,"$",sep=""),collapse="|"), List[,source], invert=invert, ignore.case=TRUE)
    }
    if (!is.null(targetSubset)) {
        found2 <- grep(paste(paste("^",targetSubset,"$",sep=""),collapse="|"), List[,target], invert=invert, ignore.case=TRUE)
    }
    if (is.null(sourceSubset) && is.null(targetSubset)) {
        warning("No source or target subset provided. No filtering performed.")
    }
    join <- match.arg(join)
    if (join == "intersect") {
        Found <- intersect(found, found2)
    } else {
        Found <- union(found, found2)
    }
    if (length(Found) == 0) {
        message("No subset found. No filtering performed")
        Found <- c(1:nrow(List))
    }
    return(List[Found,])
}

#' makeSankey
#'
#' Takes a list of source-target pairs and assigns colours to nodes and connections based on value, sourcefc and targetfc. Output is a list with List$reference = input List with additional description values, $valDomain = path values, $valRange = path colours, $targetDomain = target names, $targetRange = target node colours, $sourceDomain = source names, $sourceRange = source node colours.
#' @return a list of data.frames and colour vectors.
#' @param List Data frame containing 6 columns: source, target, description, value, sourcefc, targetfc.
#' @param averagePath Boolean to either keep List$value as-is, or calculate List$value for intermediary nodes (i.e. source nodes that were previously target nodes) as an average of previous path List$values.
#' @param nodeMin Colour assigned to minimum node value.
#' @param nodeMax Colour assigned to maximum node value.
#' @param pathMin Colour assigned to minimum path value.
#' @param pathMax Colour assigned to maximum path value.
#' @param nought 'Zero' value dividing node and paths into two distinct sets. i.e. positive and negative.
#' @param noughtColor Colour assigned to nought value.
#' @param noughtPath Optional parameter that sets a different 'zero' value for paths than for nodes.
#' @param noughtPathColor Optional parameter that assigns a different colour to the path 'zero' value from the node 'zero' value. 
#' @keywords sankey
#' @export
#' @examples
#' tempList <- createList(data.frame(source=c("A","B","C"),
#'     target=c("D","E","G"),
#'     description=c("consonant","vowel","consonant"),
#'     value=runif(3,-1,1),
#'     sourcefc=runif(3,-2,2),
#'     targetfc=runif(3,-2,2)))
#' initSankey()
#' tempList2 <- makeSankey(tempList)
makeSankey <- function(List, averagePath=FALSE, nodeMin="blue", nodeMax="red", pathMin="blue", pathMax="red", noughtColor="#f5f5f0", nought=0, noughtPath=NULL, noughtPathColor=NULL) {
    edgelist <- List
    edgelist_avg <- List
    edgelist_avg$value <- round(edgelist_avg$value,3)
    truevalue <- round(edgelist_avg$value,3)
    pkg.env$nodeMin <- nodeMin
    pkg.env$nodeMax <- nodeMax
    pkg.env$pathMin <- pathMin
    pkg.env$pathMax <- pathMax
    pkg.env$noughtColor <- noughtColor
    pkg.env$nought <- nought

    # make sure no duplicate source and target with differing values
    if (length(which(duplicated(edgelist[,c("source","target")]))) > 0) {
        duplSource <- edgelist$source[which(duplicated(edgelist[,c("source","target")]))]
        duplTarget <- edgelist$target[which(duplicated(edgelist[,c("source","target")]))]
        stop(paste("Duplicate source-target pair found on lines ", paste(intersect(grep(paste("^",duplSource,"$",sep=""),edgelist$source),grep(paste("^",duplTarget,"$",sep=""),edgelist$target)), sep=","), ". Please verify values.", sep=""))
    }

    # make sure no nodes that are both source and target have more than one value
    failCheck <- FALSE
    failNodes <- c()
    if (sum(duplicated(c(edgelist[,"source"],edgelist[,"target"]))) > 0) {
        for (row in 1:nrow(edgelist)) {
        dupli <- grep(paste("^",edgelist[row,"source"],"$",sep=""),sprintf("%s",edgelist[,"target"]))
        if (length(dupli) > 0) { # Check sourcefc and targetfc is the same for the node.
            if (length(unique(c(edgelist[row,"sourcefc"],edgelist[dupli,"targetfc"]))) > 1) {
                failCheck <- TRUE
                failNodes <- c(failNodes, as.character(edgelist[row,"source"]))
            }
        }
    }
    }
    if (failCheck) {
        stop(paste("Different sourcefc and targetfc values found for nodes: ", paste(failNodes, collapse=", "), ". Please verify values.", sep=""))
    }

        # Sum TOTAL value for a target that is also a source. This sets path widths.
        edgelist$value <- abs(edgelist$value)
        for (t in 1:length(unique(edgelist$target))) {
             found <- grep(paste("^",unique(edgelist$target)[t],"$",sep=""), as.character(edgelist$source))
            if (length(found) > 0) {
                summedTarget <- sum(round(edgelist[grep(paste("^",unique(edgelist$target)[t], "$", sep=""), edgelist$target),"value"],3))
                sources <- round(edgelist[grep(paste("^", unique(edgelist$target)[t], "$", sep=""), edgelist$source),"value"],3)
                if (sum(!is.na(sources)) > 0) { # if values are given for intermediary nodes, use them!
                    summedSource <- sum(sources)
                    adjustmentVals <- round(edgelist$value[found]/summedSource,3) # proportional values
                } else { # NA value given for intermediary nodes.
                    adjustmentVals <- round(1/length(sources),3)
                }
                    summed <- summedTarget*adjustmentVals
                    edgelist$value[found] <- summed
            } else { # End target. NA value must then be equal to sum of last connections of source as target.
                 summedTarget <- sum(round(edgelist[grep(paste("^",unique(edgelist$target)[t], "$", sep=""), edgelist$target),"value"],3))
                sources <- round(edgelist[grep(paste("^", unique(edgelist$target)[t], "$", sep=""), edgelist$source),"value"],3)
                
            }
        }
        edgelist$value <- round(edgelist$value, 3)

    if (averagePath) { # Sum AVERAGE value for a target that is also a source. This resets path value.
        for (t in 1:length(unique(edgelist_avg$target))) {
            found <- grep(paste("^",unique(edgelist_avg$target)[t],"$",sep=""), edgelist_avg$source)
            if (length(found) > 0) {
                edgelist_avg$value[found] <- mean(edgelist_avg[grep(unique(edgelist_avg$target)[t],edgelist_avg$target),"value"])
            }
        }
        truevalue <- edgelist_avg$value
    }

    if (sum(is.na(truevalue)) > 0) {
        stop(paste("NAs found in List values. Correct or set averagePath=TRUE to calculate values from incoming paths. Rows: ", paste(which(is.na(truevalue)), collapse=", "), sep=""))
    }

    # Assign colour scale according to fold change values
    FCrange <- as.numeric(unique(c(edgelist$sourcefc,edgelist$targetfc)))
    minFC <- min(FCrange)
    maxFC <- max(FCrange)
    FCrange_colours <- c()

    # Create color scales
    nodeMinCols <- colorRampPalette(c(nodeMin, noughtColor))
    nodeMins <- rev(nodeMinCols(length(which(FCrange < nought))+1)) # Revert order
    nodeMaxCols <- colorRampPalette(c(noughtColor, nodeMax))
    nodeMaxs <- nodeMaxCols(length(which(FCrange >= nought))+1)

    # Use color scales
    for (fc in FCrange) {
        if (fc < nought) { # nodeMin farther from 0. Black closer to 0.
               # assign colours proportional to distance between nought and minFC
               FCrange_colours <- c(FCrange_colours, nodeMins[min(max(round((nought-fc)/(nought-minFC)*length(nodeMins)),1),length(nodeMins))])
        } else { # nodeMax farther from 0, Black closer to 0.
               # assign colours proportional to distance between nought and maxFC
               FCrange_colours <- c(FCrange_colours, nodeMaxs[min(max(round((fc-nought)/(maxFC-nought)*length(nodeMaxs)),1),length(nodeMaxs))])
        }
    }

    # Assign colours by foldchange
    source_domain <- unique(edgelist$source)
    source_range <- rep("#999999",length(source_domain))
    target_domain <- unique(edgelist$target)
    target_range <- rep("#999999",length(target_domain))
    for (c in 1:length(FCrange)) {
        sources <- unique(edgelist$source[which(edgelist$sourcefc==FCrange[c])])
        if (length(sources) > 0) {
            source_range[grep(paste(sources,collapse="|"),source_domain)] <- FCrange_colours[c]
        }
        targets <- unique(edgelist$target[which(edgelist$targetfc==FCrange[c])])
        if (length(targets) > 0) {
            target_range[grep(paste(targets,collapse="|"),target_domain)] <- FCrange_colours[c]
        }
    }

    # Assign colour values to edgelist according to edgelist_avg correlation value
    truevalue <- round(truevalue,3)
    COrange <- truevalue[which(!duplicated(truevalue))]

    if (is.null(noughtPath)) { noughtPath <- nought }
    if (is.null(noughtPathColor)) {noughtPathColor <- noughtColor }

    maxCo <- max(COrange)
    COrange_colours <- c()
    pathMinCols <- colorRampPalette(c(pathMin, noughtPathColor))
    pathMins <- rev(pathMinCols(length(which(COrange < noughtPath))+1)) # reverse order
    pathMaxCols <- colorRampPalette(c(noughtPathColor, pathMax))
    pathMaxs <- pathMaxCols(length(which(COrange >= noughtPath))+1)

    # Use rgb color scales
    for (c in COrange) {
        if (c < noughtPath) { # Red further from 0 black.
            # assign colours proportional to distance between nought and minFC
            COrange_colours <- c(COrange_colours, pathMins[min(max(round((noughtPath-c)/(noughtPath-min(COrange))*length(pathMins)),1),length(pathMins))])
            #COrange_colours <- c(COrange_colours, pathMins[grep(paste("^",c,"$", sep=""),sort(COrange[which(COrange < noughtPath)]))])
        } else { # Green further from 0 black.
            # assign colours proportional to distance between nought and maxFC
            COrange_colours <- c(COrange_colours, pathMaxs[min(max(round((c-noughtPath)/(maxCo-noughtPath)*length(pathMaxs)),1),length(pathMaxs))])
            #COrange_colours <- c(COrange_colours, pathMaxs[grep(paste("^",c,"$", sep=""),sort(COrange[which(COrange >= noughtPath)]))+1])
        }
    }

    # Replace negative values, if any, before inputting to rCharts
    absValues <- edgelist$value
    dupli <- which(duplicated(absValues))
    for (dup in dupli) {
        # first, check that absValues is not also duplicated in truevalue
        # If truevals are also duplicated, then ignore.
 #       truevals <- truevalue[grep(paste("^",absValues[dup],"$",sep=""),absValues)]
 #       if (sum(duplicated(truevals)) == 0) { # Different true values but same absolute values
            toReplace <- absValues[dup] + 0.001
            adjusted <- grep(toReplace, absValues)
            if (length(adjusted) != 0) { # New value already exists
                toReplace <- absValues[dup] - 0.001
                adjusted2 <- grep(toReplace, absValues)
                while (length(adjusted2) != 0) {
                    toReplace <- toReplace - 0.001
                    adjusted2 <- grep(toReplace, absValues)
                }
                absValues[dup] <- toReplace
            } else {
                absValues[dup] <- toReplace
            }
#        }
    }
    edgelist$value <- absValues
    edgelist <- cbind(edgelist,truevalue)   

    # Assign colours by edgelist value
    value_domain <- edgelist$value
    value_range <- rep("#999999", length(value_domain))
    for (v in 1:length(value_domain)) {
        value_range[v] <- COrange_colours[grep(paste("^",round(edgelist$truevalue[v],3),"$",sep=""), COrange)]
    }


    # For presentation purposes, replace the values of transcript -> gene relationships with average correlation input.
    score_info <- rep("Relationship score: ",nrow(edgelist))
#    connected <- as.character(edgelist$description)

    for (t in 1:length(unique(edgelist$target))) {
        found7 <- which(as.character(edgelist$source) == unique(as.character(edgelist$target))[t])
        if (length(found7) > 0) {
#            connected[found7] <- paste(edgelist[grep(unique(edgelist$target)[t],edgelist$target),"source"],collapse=", ")
            if (averagePath) { # Sum AVERAGE value for a target that is also a source
                score_info[found7] <- paste("Average combined effect on ",edgelist$source[found7],":",sep="")
#                edgelist$truevalue[found7] <- mean(as.numeric(edgelist[grep(unique(edgelist$target)[t],edgelist$target),"truevalue"]))
            }
        }
    }
    edgelist <- cbind(edgelist,score_info)
#    edgelist$description <- connected

    # NUMERICAL ORDER OF VALUES IS IMPORTANT. Range is assigned in descending order AND round to 3 decimal places
    # to match rCharts values.
    resorted_vals <- order(value_domain, decreasing=TRUE)
    value_domain <- value_domain[resorted_vals]
    value_range <- value_range[resorted_vals]

    # Check for loops and rename source-target pairs if necessary.
    paths <- list()
    counter <- 1
    loop <- c()
    for (i in 1:nrow(edgelist)) {
        Source <- as.character(edgelist[i,"source"])
        Target <- as.character(edgelist[i, "target"])
        exists <- grep(Source, paths)
        if (length(exists) == 0) { # create new item in paths
            prefix <- grep(paste("^", Target, "$", sep=""),lapply(paths,'[[',1))
            if (length(prefix) > 0) { # Target is Source of a path
                for (pre in prefix) {
                paths[[pre]] <- c(Source,paths[[pre]])
                }
            } else {
                paths[[counter]] <- c(Source, Target)
                counter <- counter + 1
            }
        } else { # Source already exists in a path
            for (exist in exists) {
                checked <- grep(paste("^",gsub("[*]","[*]",Source), "$", sep=""), paths[[exist]]) # Exists as source or target?
                if (length(checked) == 0) {
                    checked <- -1
                }
                if (checked == length(paths[[exist]])) { # Source is recently a target
                    if (length(grep(paste("^",gsub("[*]","[*]",Target),"$",sep=""),paths[[exist]])) > 0) { # Loop exists!
                        warning(paste("Sankeys do not produce loops. ", Source, " -> ", Target, " on line ", i," of $reference has been renamed. For optimal figure, please break the loop of ", paste(paths[[exist]],collapse=" -> "), " -> ", Target, " and rerun this function.", sep=""))
                        edgelist$target[i] <- as.character(paste("Loop_",Target, sep="")) # rename target to remove loop
                        paths[[exist]] <- c(paths[[exist]], paste("Loop_",Target, sep=""))
                        loop <- c(paths[[exist]]) # save loop
                    } else {
                        if (length(intersect(loop, paths[[exist]])) > 1) { # Source and known loop members exist!
                          warning(paste("Sankeys do not produce loops. ", Source, " -> ", Target, " on line ", i," of $reference has been renamed. For optimal figure, please break the loop of ", paste(paths[[exist]],collapse=" -> "), " -> ", Target, " and rerun this function.", sep=""))
                        paths[[exist]] <- c(paths[[exist]], paste("Loop_",Target, sep=""))
                        } else {
                        paths[[exist]] <- c(paths[[exist]], Target)
                        }
                    }
                } else {
                    foundExist <- grep(paste("^",Source,"$",sep=""), paths[[exist]])
                    foundExist2 <- grep(paste("^",Target,"$",sep=""), paths[[exist]])
                    check2 <- length(foundExist)
                    if (check2 == 0) {
                        foundExist <- 1
                    }
                    if (foundExist != 1) {
                        if (length(foundExist2) > 0) {
                            if (foundExist > foundExist2) { # Loop in the path exists!
                                if (foundExist == length(paths[[exist]])) {
                                    warning(paste("Sankeys do not produce loops. ", Source, " -> ", Target, " on line ", i," of $reference has been renamed. For optimal figure, please remove the line and rerun this function.", sep=""))
                                    edgelist$target[i] <- as.character(paste("Loop_",Target, sep="")) # rename target to remove loop
                                    paths[[exist]] <- c(paths[[exist]], Target)
                                }
                            }
                        } else { # new path from Source
                          paths[[counter]] <- c(paths[[exist]][1:foundExist], Target)
                          counter <- counter + 1

                        }
                    } else { # new path from Source
                        #verify target is new for this Source
                        knownTargets <- unlist(paths[grep(Source, lapply(paths,'[[',1))])
                        if (length(grep(Target, knownTargets)) == 0 || min(grep(Target, knownTargets) - grep(Source, knownTargets)) > 1) { # new direct target for this Source
                            paths[[counter]] <- c(Source, Target)
                            counter <- counter + 1
                        }
                    }
                }
            }
        }
    }

    # ensure numerical
    edgelist$sourcefc <- as.numeric(edgelist$sourcefc)
    edgelist$targetfc <- as.numeric(edgelist$targetfc)
    edgelist$value <- as.numeric(edgelist$value)
    return(list(reference=edgelist,valDomain=value_domain, valRange=value_range, targetDomain=target_domain, sourceDomain=source_domain, targetRange=target_range, sourceRange = source_range))

}

#' writeSankey
#'
#' Save sankey figure as a simple HTML file accessible outside of R and shiny. 
#' Functions initSankey, makeSankey and drawSankey must be performed before this step to ensure a proper figure is saved.
#' @return a dynamic HTML file in the specified directory that is readable in any internet browser so long as the 'www' subfolder is included.
#' @param name Name to give file. If no path given, the working directory OR path set in Director will be used. Same name will be given as the title.
#' @param title Title of the HTML file produced. The file name is used by default.
#' @param directory Absolute path to output directory. If null, the working directory obtained from getwd() will be used. If no absolute path is given (i.e. no "/" is grepped), it will assume a new folder will be created in the working directory.
#' @keywords sankey html
#' @export
#' @examples
#' Level1 <- createList(poorprog$Level1)
#' Level2 <- createList(poorprog$Level2)
#' tempList <- append2List(Level1,Level2)
#' initSankey() # initializes working directory
#' tempList2 <- makeSankey(tempList, averagePath=TRUE) # Calculate node and path values
#' sankey <- drawSankey(tempList2)
#' writeSankey("temp") # Save figure as the file 'temp.html' in working directory.
writeSankey <- function(name=NULL, title=NULL, directory=NULL) {
    if (is.null(name)) {
            stop("Name is needed to create an HTML file.")
    } else {
            if (is.null(pkg.env$sankeyHTML)) {
                stop("No diagram content to write. Have you run makeSankey() and drawSankey()?")
            }
            if (is.null(title)) {
                title <- name
            }
            if (length(grep(".html$", name)) == 0) { # Add extension to open in browser
                name <- paste(name,"html",sep=".")
            }
           
    # Create and write files to working directory
        if (!is.null(directory)) { # designate folder to place output files
        pkg.env$absoluteDirectory <- gsub("/$","",directory)
        if (!dir.exists(pkg.env$absoluteDirectory) && length(grep("/",directory)) == 0) { # Not a path but new folder to create
            dir.create(directory)
            pkg.env$absoluteDirectory <- paste(getwd(),directory,sep="/")
        } else if (dir.exists(pkg.env$absoluteDirectory) && length(grep("/",directory)) == 0) { # Folder exists
            pkg.env$absoluteDirectory <- paste(getwd(),directory,sep="/")
        }
    } else { # default output folder is the working directory. localDirectory = "."
        pkg.env$absoluteDirectory <- getwd()
    }

    if (!dir.exists(pkg.env$absoluteDirectory)) {
        stop(paste("Path ", pkg.env$absoluteDirectory, " does not exist.", sep=""))
    } else {
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www",sep="/"))
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www/js",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www/js", sep="/"))
            system(paste("cp ", system.file(package="Director"),"/www/js/LICENSE ", pkg.env$absoluteDirectory, "/www/js",sep=""))
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"www/css",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"www/css", sep="/"))
        }
    }
    # Write JS libraries
    if (is.null(pkg.env$dependencies$sankey)) { # Use package default
        system(paste("cp ", system.file(package="Director"),"/www/js/sankey.js ", pkg.env$absoluteDirectory, "/www/js",sep=""))
    } else {
        jsConn <- file(paste(pkg.env$absoluteDirectory, "www/js/sankey.js", sep="/"))
        cat(pkg.env$dependencies$sankey, file=jsConn)
        close(jsConn)
    }            
    if (is.null(pkg.env$dependencies$d3)) { # Use package default
        system(paste("cp ", system.file(package="Director"),"/www/js/d3.v3.js ", pkg.env$absoluteDirectory, "/www/js",sep=""))
    } else {
        jsConn2 <- file(paste(pkg.env$absoluteDirectory, "www/js/d3.v3.js", sep="/"))
        cat(pkg.env$dependencies$d3, file=jsConn2)
        close(jsConn2)
    }            
    cssConn <- file(paste(pkg.env$absoluteDirectory, "www/css/sankey.css", sep="/"))
    cat(as.character(pkg.env$dependencies$css), file=cssConn)
    close(cssConn)

            # Write HTML file and make dependencies relative
            save_html(pkg.env$sankeyHTML, paste(pkg.env$absoluteDirectory, name, sep="/"))
            message(paste("Sankey saved externally to ", paste(pkg.env$absoluteDirectory, name, sep="/"),".",sep=""))
            
            # Convert absolute paths to local. This will make resulting HTML files portable
            HTMLfile <- file(paste(pkg.env$absoluteDirectory, name,sep="/"))
            HTMLtext <- readLines(HTMLfile)
            HTMLtext[grep("d3.v3.js", HTMLtext)] <- "<script src='www/js/d3.v3.js' type='text/javascript'></script>"
            HTMLtext[grep("sankey.js", HTMLtext)]  <- "<script src='www/js/sankey.js' type='text/javascript'></script>"
            HTMLtext[grep("</head>",HTMLtext)] <- gsub("</head>",paste("<link rel='stylesheet' href='www/css/sankey.css'><title>",title, "</title></head>",sep=""),HTMLtext[grep("</head>",HTMLtext)])
            HTMLtext[grep("writeSankey", HTMLtext)] <- "<p>Diagram uses resources in 'www' subfolder.</p>"
            startCSS <- grep("Code is a compilation", HTMLtext)-2
            endCSS <- grep("pointer-events: none;", HTMLtext)[length(grep("pointer-events: none;", HTMLtext))] + 2
            HTMLtext <- c(HTMLtext[c(1:startCSS)],  "", HTMLtext[c(endCSS:length(HTMLtext))])
            cat(HTMLtext, file=paste(pkg.env$absoluteDirectory, name,sep="/"),sep="\n")
            close(HTMLfile)
    }
}