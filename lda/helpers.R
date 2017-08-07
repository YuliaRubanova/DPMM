list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

get_elem <- function(list, name) {
  target_elem <- list[grepl(paste0("^", name, "="), list)]
  target_elem <- gsub(paste0("^", name, "=(.*)"), "\\1", target_elem)
  return(as.numeric(target_elem))
}

processFile = function(filepath) {
  con = file(filepath, "r")
  # Read the document index
  line = readLines(con, n = 1)
  
  # Read training params
  line = unlist(strsplit(readLines(con, n = 1), ", "))
  alpha = get_elem(line, "a")
  beta = get_elem(line, "b")
  print(line)
  print(beta)
  
  perplex = c()
  clusters = c()
  big_clusters = c()
  while ( TRUE ) {
    line = unlist(strsplit(readLines(con, n = 1), " "))
    if ( length(line) == 0 || sum(grepl("p=", line)) == 0) {
      break
    }
    perplex <- c(perplex, get_elem(line, "p"))
    clusters <- c(clusters, get_elem(line, "clusters"))
    big_clusters <- c(big_clusters, get_elem(line, "big_clusters"))
  }

  # Read the topic indices
  line = readLines(con, n = 1)
  topics <- gsub("Topics: \\[(.*)\\]", "\\1", line)
  topics <- as.numeric(unlist(strsplit(topics, ", ")))
  
  # Read the topic counts
  line = readLines(con, n = 1)
  counts <- gsub("Counts: \\[(.*)\\]", "\\1", line)
  counts <- as.numeric(unlist(strsplit(counts, ", ")))
  
  close(con)
  
  return(list(alpha, beta, perplex, clusters, big_clusters, topics, counts))
}


authors_to_bibtex <- function(authors) {
  authors_s <- unlist(strsplit(authors, ", "))
  authors_si <- authors_s
  for (i in 1:length(authors_s)) {
    a = authors_s[i]
    a_s <- unlist(strsplit(a, " "))
    a_s[1] <- paste0(substr(a_s[1],1,1), ".")
    
    # handle Peter Van Loo
    if (a_s[length(a_s)] == "Loo") {
      a_s[length(a_s) -1] <- paste(a_s[length(a_s) -1], a_s[length(a_s)])
      a_s <- a_s[-length(a_s)]
    }
    
    if (length(a_s) == 3) {
      a_s[1] <- paste(a_s[1], a_s[2])
      a_s <- a_s[-2]
    }
    if (length(a_s) > 3) {
      stop("Handle me!")
    }
    
    authors_si[i] = paste0(a_s[length(a_s)], ", ", a_s[1])
  }
  
  return(paste(authors_si, collapse = ' and '))
}