# Director R package for creating directed regulatory cascades.
pkg.env <- new.env()
pkg.env$absoluteDirectory <- getwd()
pkg.env$localDirectory <- "."
pkg.env$sankeyPlot <- NULL
pkg.env$nodMin <- "blue"
pkg.env$nodeMax <- "red"
pkg.env$pathMin <- "blue"
pkg.env$pathMax <- "red"
pkg.env$noughtColor <- "#f5f5f0"
pkg.env$nought <- 0
#' initSankey
#'
#' Specify output folder for HTML figures and create/download required CSS and Javascript support files.
#' @return essential HTML files in specified directory.
#' @param directory Absolute path to output directory. If null, the working directory obtained from getwd() will be used. If no absolute path is given (i.e. no "/" is grepped), it will assume a new folder will be created in the working directory.
#' @param pathOpacity Opacity of connecting path between nodes in the figure.
#' @param pathHover Opacity of connecting path between nodes upon mouseover.
#' @param font Font used for the node names and additional mouseover text in figure.
#' @param fontsize Pixel font size used for the visible node names. Use to adjust range of font sizes (with proportions) or to set a single font size when fontsizeProportion is disabled.
#' @param fontsizeProportion Boolean to enable/disable text being proportional to node widths. When enabled, all node names will appear with parameter fontsize.
#' @param d3js Path to download latest zip version of D3 library. e.g. https://github.com/mbostock/d3/releases/download/v3.5.6/d3.zip. See http://www.d3js.org for more details. If NULL, will use version 3.5.6 currently installed with Director.
#' @param sankeyjsFile Path to download sankey javascript file. If NULL, will use version installed with Director (https://raw.githubusercontent.com/d3/d3-plugins/master/sankey/sankey.js)
#' @param d3jsMethod Function method to use to download D3 library. ?download.file for more detail on parameter.
#' @param sankeyjsMethod Function method to use to download sankey script. ?download.file for more detail on parameter.
#' @keywords download sankey
#' @export
#' @import devtools
#' @import rCharts
#' @import utils
#' @import grDevices
#' @examples
#' initSankey("temp") # Creates a temp folder to output HTML files and necesary dependencies.
initSankey <- function(directory=NULL, pathOpacity=0.2, pathHover=0.5, font="lato, helvetica, sans-serif", fontsize=NULL, fontsizeProportion=TRUE, d3js=NULL, sankeyjsFile=NULL, d3jsMethod="wget", sankeyjsMethod="wget") {

    if (!is.null(directory)) { # designate folder to place output files
        pkg.env$absoluteDirectory <- gsub("/$","",directory)
        if (!dir.exists(pkg.env$absoluteDirectory) && length(grep("/",directory)) == 0) { # Not a path but new folder to create
            dir.create(directory)
            pkg.env$absoluteDirectory <- paste(getwd(),directory,sep="/")
            pkg.env$localDirectory <- directory
        } else if (dir.exists(pkg.env$absoluteDirectory) && length(grep("/",directory)) == 0) { # Folder exists
            pkg.env$absoluteDirectory <- paste(getwd(),directory,sep="/")
            pkg.env$localDirectory <- directory
        }
    } else { # default output folder is the working directory. localDirectory = "."
        pkg.env$absoluteDirectory <- getwd()
    }

    if (is.null(fontsize)) {
        FontInfo <- paste("-family: ", font, ";
        font-weight: 500;", sep="")
    } else {
        FontInfo <- paste(": ", fontsize, "px ", font, ";", sep="")
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

    cssScript <- paste("/*
Code is a compilation of:
http://www.d3noob.org/2013/02/sankey-diagrams-description-of-d3js-code.html
http://bl.ocks.org/git-ashish/8959771
And the original, copyright (c) 2015 Mike Bostock http://bost.ocks.org/mike/sankey/
*/

.node rect {
    cursor: move;
    fill-opacity: .9;
    shape-rendering: crispEdges;
}

.node text {
    pointer-events: none;
    text-shadow: 1px 1px 0 #fff, -1px 1px 0 #fff, -1px -1px 0 #fff, 1px -1px 0 #fff;
}

.link {
    fill: none;
    stroke: #000;
    stroke-opacity: ", pathOpacity, ";
}

.link:hover {
    stroke-opacity: ", pathHover, ";
}

svg {
    font", FontInfo, "
}

div.tooltip { /* for node mouseover effect */
    position: absolute;
    text-align: left;
    padding: 4px;
    font-family: ", font, ";
    color: #000000;
    background: #ffffff;
    border: 0px;
    border-radius: 3px;
    pointer-events: none;
}",sep="")

chartHTML <- paste('<!--Attribution:',
'copyright (c) 2015 Mike Bostock https://github.com/d3/d3-plugins/tree/master/sankey
copyright (c) 2015 Mike Bostock http://bost.ocks.org/mike/sankey/
-->
<script>
(function(){
var params = {{{ chartParams }}};

params.units ? units = " " + params.units : units = "";

var formatNumber = d3.format("0,.3f"),    // three decimal places
    format = function(d) { return formatNumber(d) + units; },
    color = d3.scale.category20();

if(params.labelFormat){
    formatNumber = d3.format(".2%");
}

var svg = d3.select("#" + params.id).append("svg")
    .attr("width", params.width)
    .attr("height", params.height);

var sankey = d3.sankey()
    .nodeWidth(params.nodeWidth)
    .nodePadding(params.nodePadding)
    .layout(params.layout)
    .size([params.width,params.height]);

var path = sankey.link();

var data = params.data,
    links = [],
    nodes = [],
    nodesfc = [];

var div = d3.select("body").append("div")
    .attr("class", "tooltip")
    .style("opacity", 0);

//get all source and target into nodes
//will reduce to unique in the next step
//also get links in object form
data.source.forEach(function (d, i) {
    // all data you wish to show in the mouseover of nodes, place here.
    nodes.push({ "name": data.source[i] });
    nodes.push({ "name": data.target[i] });
    nodesfc.push({ "name": data.source[i], "fc": +data.sourcefc[i]  });
    nodesfc.push({ "name": data.target[i], "fc": +data.targetfc[i]  });
    // all data you wish to show in the mouseover of the links, place here.
    links.push({ "source": data.source[i], "target": data.target[i], "value": +data.value[i], "description": data.description[i], "truevalue": +data.truevalue[i], "score_info": data.score_info[i] });
});

//now get nodes based on links data
//thanks Mike Bostock https://groups.google.com/d/msg/d3-js/pl297cFtIQk/Eso4q_eBu1IJ
//this handy little function returns only the distinct / unique nodes
nodes = d3.keys(d3.nest()
                .key(function (d) { return d.name; })
                .map(nodes));

//it appears d3 with force layout wants a numeric source and target
//so loop through each link replacing the text with its index from node
links.forEach(function (d, i) {
    links[i].source = nodes.indexOf(links[i].source);
    links[i].target = nodes.indexOf(links[i].target);
});

//now loop through each node to make nodes an array of objects rather than an array of strings
nodes.forEach(function (d, i) {
    for(var key in nodesfc) {
        if(nodesfc[key].name==d) { fcIndex=key; break; }
    }
    nodes[i] = { "name": d , "fc": nodesfc[fcIndex].fc};
});

sankey
    .nodes(nodes)
    .links(links)
    .layout(params.layout);

var link = svg.append("g").selectAll(".link")
    .data(links)
.enter().append("path")
    .attr("class", "link")
    .attr("d", path)
    .attr("id", function(d,i){ // needed for path highlighting
        d.id = i;
        return "link-"+i;
    })
    .style("stroke-width", function (d) { return Math.max(1, d.dy); })
    .sort(function (a, b) { return b.dy - a.dy; })
    .on("mouseover", function(d) {
            div.transition()
                .duration(200)
                .style("opacity", 0.9);
            div .html("<b>" +d.source.name + " -> " + d.target.name + "</b><br><font size=-1>" + d.score_info + " " + format(d.truevalue)  + "<i>" + d.description + "</i></font>")
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
            })
    .on("mouseout", function(d) {
                div.transition()
                    .duration(500)
                    .style("opacity", 0);
            });

link.append("title") ;

var node = svg.append("g").selectAll(".node")
    .data(nodes)
.enter().append("g")
    .attr("class", "node")
    .on("click", highlight_node_links)
    .attr("transform", function (d) { return "translate(" + d.x + "," + d.y + ")"; })
.call(d3.behavior.drag()
    .origin(function (d) { return d; })
    .on("dragstart", function () { this.parentNode.appendChild(this); })
    .on("drag", dragmove))
    .on("mouseover", function(d) {
            div.transition()
                .duration(200)
                .style("opacity", 0.9);
            div .html("<b>" + d.name + "</b><br><font size=-1>Foldchange: " + format(d.fc)  + "</font>")
                .style("left", (d3.event.pageX) + "px")
                .style("top", (d3.event.pageY - 28) + "px");
            })
    .on("mouseout", function(d) {
                div.transition()
                    .duration(500)
                    .style("opacity", 0);
            });

node.append("rect")
    .attr("height", function (d) { return d.dy; })
    .attr("width", sankey.nodeWidth())
    .style("fill", function (d) { return d.color = color(d.name.replace(/ .*/, "")); })
    .style("stroke", function (d) { return d3.rgb(d.color).darker(2); })
    .append("title");

node.append("text")
    .attr("x", -6)
    .attr("y", function (d) { return d.dy / 2; })
    .attr("dy", ".35em")
    .attr("text-anchor", "end")
    .attr("transform", null)
    .text(function (d) { return d.name; })', fontsizeProportioned, '
    .filter(function (d) { return d.x < params.width / 2; })
    .attr("x", 6 + sankey.nodeWidth())
    .attr("text-anchor", "start");

// the function for moving the nodes
    function dragmove(d) {
    d3.select(this).attr("transform",
        "translate(" + (
                d.x = Math.max(0, Math.min(params.width - d.dx, d3.event.x))
                ) + "," + (
                d.y = Math.max(0, Math.min(params.height - d.dy, d3.event.y))
                ) + ")");
        sankey.relayout();
        link.attr("d", path);
    }

// the function for highlighting paths
    function highlight_node_links(node,i){

    var remainingNodes=[],
        nextNodes=[];

    var stroke_opacity = 0;
    if( d3.select(this).attr("data-clicked") == "1" ){
        d3.select(this).attr("data-clicked","0");
        stroke_opacity = ', pathOpacity, ';
    }else{
        d3.select(this).attr("data-clicked","1");
        stroke_opacity = ', pathHover, ';
    }

    var traverse = [{
                    linkType : "sourceLinks",
                    nodeType : "target"
                    },{
                    linkType : "targetLinks",
                    nodeType : "source"
                    }];

    traverse.forEach(function(step){
        node[step.linkType].forEach(function(link) {
        remainingNodes.push(link[step.nodeType]);
        highlight_link(link.id, stroke_opacity);
        });

    while (remainingNodes.length) {
        nextNodes = [];
        remainingNodes.forEach(function(node) {
            node[step.linkType].forEach(function(link) {
            nextNodes.push(link[step.nodeType]);
            highlight_link(link.id, stroke_opacity);
        });
        });
        remainingNodes = nextNodes;
        }
    });
}

    function highlight_link(id,opacity){
        d3.select("#link-"+id).style("stroke-opacity", opacity);
    }

})();
</script>\n',sep="")

configScript <- paste('d3_horizon:
    css: [css/sankey.css]
    jshead: [js/d3.v3.js,js/sankey.js]
    cdn:
        jshead:
            - "http://d3js.org/d3.v3.min.js"
            -', ' "https://raw.githubusercontent.com/d3/d3-plugins/master/sankey/sankey.js"\n',sep="")

    # Write files to folder
    if (!dir.exists(pkg.env$absoluteDirectory)) {
        stop(paste("Path ", pkg.env$absoluteDirectory, " does not exist.", sep=""))
    } else {
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"css",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"css",sep="/"))
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"layouts",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"layouts",sep="/"))
        }
        if (!dir.exists(paste(pkg.env$absoluteDirectory,"js",sep="/"))) {
            dir.create(paste(pkg.env$absoluteDirectory,"js", sep="/"))
        }
        cssConn <- file(paste(pkg.env$absoluteDirectory,"css/sankey.css",sep="/"))
        cat(cssScript, file=cssConn)
        close(cssConn)
        htmlConn <- file(paste(pkg.env$absoluteDirectory,"layouts/chart.html",sep="/"))
        cat(chartHTML, file=htmlConn)
        close(htmlConn)
        configConn <- file(paste(pkg.env$absoluteDirectory,"config.yml",sep="/"))
        cat(configScript, file=configConn)
        close(configConn)
        if (length(grep(".zip",d3js)) == 1) { # need to unzip!
            temp <- tempfile()
            # download D3 JavaScript library from server
            download.file(d3js, temp, method=d3jsMethod)
            # unload library
            unzip(temp, exdir=paste(pkg.env$absoluteDirectory,"js", sep="/"))
            system(paste("mv ", pkg.env$absoluteDirectory, "/js/d3.js ", pkg.env$absoluteDirectory, "/js/d3.v3.js", sep="")) # Something in Sankey rCharts looks for d3.v3.js rather than d3.js
        } else if (is.null(d3js)) { # Use installed version
            system(paste("cp ", system.file(package="Director"),"/d3-3.5.6/* ", pkg.env$absoluteDirectory, "/js",sep=""))
        } else {
            stop("Two d3js files are usually needed to produce HTML files correctly -- d3.js, and d3.min.js. See http://d3js.org for more details.")
        }

        if (is.null(sankeyjsFile)) { # use installed version
            system(paste("cp ", system.file(package="Director"),"/sankey.js ", pkg.env$absoluteDirectory, "/js",sep=""))
        } else {
        # Download Sankey JavaScript file
        download.file(sankeyjsFile, paste(pkg.env$absoluteDirectory, "js","sankey.js", sep="/"), method=sankeyjsMethod)
        }
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
#' Produce a sankey figure HTML with a given List.
#' @return a dynamic HTML
#' @param List Data frame containing 6 columns: source, target, description, value, sourcefc, targetfc.
#' @param height Pixel height of the figure to draw. If empty, the figure will be given a pixel height proportional to the number of rows in List up to a maximum 1800px or minimum of 300px. These can be overridden by defining this parameter.
#' @param width Pixel width of the figure to draw. By default, 1000px.
#' @param caption Sankey figure caption. HTML formatting is possible.
#' @param legendfont Font of the legend text.
#' @param legendsize Font size of the legend text.
#' @param nodeValue Description of node scale in legend.
#' @param pathValue Description of path scale in legend.
#' @keywords draw sankey
#' @export
#' @examples
#' Level1 <- createList(poorprog$Level1)
#' Level2 <- createList(poorprog$Level2)
#' tempList <- append2List(Level1,Level2)
#' initSankey()
#' tempList2 <- makeSankey(tempList, averagePath=TRUE)
#' drawSankey(tempList2)

drawSankey <- function(List, height=NULL, legendfont="sans-serif", legendsize=12, width=1000, caption="Sankey figure", nodeValue="Log2 fold-change", pathValue="Correlation coefficient") {
    if (!dir.exists(pkg.env$absoluteDirectory)) {
        stop("path provided does not exist. Consider running initSankey() function and rerun writeSankey with with default parameter.")
        }
    # make sure List is formatted correctly
    if (length(List) != 7) {
        stop("A List must be provided. Format a List of source-target pairs with makeSankey() function and try this function again.")
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


    customScript <- paste("
    <script>
    var color = d3.scale.ordinal()
        .domain(['", paste(List$valDomain,collapse="','"),"'])
        .range(['", paste(List$valRange, collapse="','"),"']);

    var color2 = d3.scale.ordinal()
        .domain(['", paste(List$targetDomain,collapse="','"),"','", paste(List$sourceDomain,collapse="','"),"'])
        .range(['", paste(List$targetRange, collapse="','"),"','", paste(List$sourceRange, collapse="','"),"']);

    d3.selectAll('#{{ chartId }} svg path.link')
        .style('stroke', function(d){
            return color(d.value);
        })
        d3.selectAll('#{{ chartId }} svg .node rect')
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

    if (!requireNamespace("rCharts",quietly=TRUE)) {
        stop("Package rCharts is needed for this function to work. Please install it.")
    } else {
        message(paste("Figure drawn using resources in", pkg.env$absoluteDirectory,sep=" "))
        pkg.env$sankeyPlot <- rCharts::rCharts$new()
        pkg.env$sankeyPlot$setLib(pkg.env$absoluteDirectory)
        pkg.env$sankeyPlot$setTemplate(afterScript = customScript[1])

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

        pkg.env$sankeyPlot$set(
        data = List$reference,
        nodeWidth = 15,
        nodePadding = 10,
        layout = 32,
        width = width,
        height = H
        )
        pkg.env$sankeyPlot
        return(pkg.env$sankeyPlot)
    }
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
    if (join == "intersect") {
        Found <- intersect(found, found2)
    } else {
        Found <- union(found, found2)
    }
    if (length(Found) == 0) {
        warning("No subset found. No filtering performed")
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

# Following ERROR still buggy:
# 1: In (function (..., deparse.level = 1)  :
#  number of columns of result is not a multiple of vector length (arg 86)

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
#' Save sankey figure as HTML file. Functions Director, makeSankey and drawSankey must be performed before this step to ensure a proper figure is saved.
#' @return a dynamic HTML file in the specified directory.
#' @param name Name to give file. If no path given, the working directory OR path set in Director will be used. Same name will be given as the title.
#' @param title Title of the HTML file produced. The file name is used by default.
#' @keywords sankey html
#' @export
#' @examples
#' Level1 <- createList(poorprog$Level1)
#' Level2 <- createList(poorprog$Level2)
#' tempList <- append2List(Level1,Level2)
#' initSankey()
#' tempList2 <- makeSankey(tempList, averagePath=TRUE) # Calculate node and path values
#' drawSankey(tempList2) # Create dynamic figure
#' writeSankey("temp") # Save figure as an HTML file.

writeSankey <- function(name=NULL, title=NULL) {
    if (!requireNamespace("rCharts",quietly=TRUE)) {
        stop("Package rCharts is needed for this function to work. Please install it.")
    } else {
        if (is.null(name)) {
            stop("Name is needed to create an HTML file.")
        } else {
            if (is.null(title)) {
                title <- name
            }
            if (length(grep(".html$", name)) == 0) { # Add extension to open in browser
                name <- paste(name,"html",sep=".")
            }
            # Write to file
            pkg.env$sankeyPlot$save(paste(pkg.env$absoluteDirectory, name,sep="/"))

            # Convert absolute paths to local. This will make resulting HTML files portable
            HTMLfile <- file(paste(pkg.env$absoluteDirectory, name,sep="/"))
            HTMLtext <- readLines(HTMLfile)
            HTMLtext[grep("sankey.css", HTMLtext)] <- "<link rel='stylesheet' href='css/sankey.css'>"
            HTMLtext[grep("d3.v3.js", HTMLtext)] <- "<script src='js/d3.v3.js' type='text/javascript'></script>"
            HTMLtext[grep("sankey.js", HTMLtext)]  <- "<script src='js/sankey.js' type='text/javascript'></script>"
            HTMLtext[grep("</head>",HTMLtext)] <- gsub("</head>",paste("<title>",title,"</title></head>",sep=""),HTMLtext[grep("</head>",HTMLtext)])
            cat(HTMLtext, file=paste(pkg.env$absoluteDirectory, name,sep="/"),sep="\n")
            close(HTMLfile)
        }
    }
}
