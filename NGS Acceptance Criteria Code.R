#packages
library(dplyr)
library(data.table)
library("openxlsx")
library("readxl")


data <- readxl::read_excel("ATCC700603_output.xlsx")

#functions
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

#make xlsx file called out to continue to add too
OUT <- createWorkbook()
OUT2 <- createWorkbook()


#do just first gene gapA
gene <- subset(data, data$Gene == "gapA")
x <- as.data.frame(strsplit(gene$Result[1] , ","))
colnames(x) <- "result"
for(i in 1:nrow(x)){
  x[i,1]
  x[i,1] <- trim(x[i,1])
}

#add the result columns to the gene dataset
for(i in 1:nrow(x)){
  gene[[x$result[i]]] <- 0
}

#go through each column of results to put a 1 in if it is present
for(q in 1:nrow(gene)){
  for(j in 14:ncol(gene)){
    
    z <- as.data.frame(strsplit(gene$Result[q] , ","))
    colnames(z) <- "result"
    for(i in 1:nrow(z)){
      z[i,1] <- trim(z[i,1])
    }
    
    if(colnames(gene)[j] %in% z$result){gene[q,j] <- 1}
  }
}  


#make a count table of the result columns 14 plus
output <- as.data.frame(NA)
output$result <- ""
output$count <- 0
output <- output[,-c(1)]
for(i in 14:nrow(gene)){
    t <- as.data.frame(table(gene[[colnames(gene)[14]]]))
    output$result[1] <- colnames(gene)[14]
    output$count[1] <-  t$Freq[t$Var1 == 1]
}

#add summary stats columns to output
output$total_isolates <- nrow(gene)
output$percent_with_result <- 100*(output$count / output$total_isolates)

#add columns to output for averages of percentages and min and max
output$average_percent_coverage <- 0
output$min_percent_coverage <- 0
output$max_percent_coverage <- 0
for(p in 14:ncol(gene)){
    st <- gene %>%
            filter(gene[[colnames(gene)[p]]] == 1)
    output[p-13,5] <- mean(st$percent_coverage_mean)
    output[p-13,6] <- min(st$percent_coverage_min)
    output[p-13,7] <- max(st$percent_coverage_max)
}



#add the gene name to the workbook
addWorksheet(OUT, "gapA")
addWorksheet(OUT2, "gapA")
writeData(OUT, sheet = "gapA", x = output)
writeData(OUT2, sheet = "gapA", x = gene)



#go through the unique genes in data
unique_genes <- data %>%
                  select(Gene) %>%
                  unique()



#k <- 12
for(k in 2:nrow(unique_genes)){
  
  gene <- subset(data, data$Gene == unique_genes$Gene[k])
  x <- as.data.frame(strsplit(gene$Result[1] , ","))
  colnames(x) <- "result"
  for(i in 1:nrow(x)){
    x[i,1]
    x[i,1] <- trim(x[i,1])
  }
  
  res <- x
  
  for(j in 2:nrow(gene)){
    
    x <- as.data.frame(strsplit(gene$Result[j] , ","))
    colnames(x) <- "result"
    for(i in 1:nrow(x)){
      x[i,1]
      x[i,1] <- trim(x[i,1])
    }
    
    res <- rbind(res, x)
    
  }
  
  res <- unique(res)
  
  #add the result columns to the gene dataset
  for(p in 1:nrow(res)){
    gene[[res$result[p]]] <- 0
  }
  
  #go through each column of results to put a 1 in if it is present
  for(q in 1:nrow(gene)){
    for(u in 14:ncol(gene)){
      
      z <- as.data.frame(strsplit(gene$Result[q] , ","))
      colnames(z) <- "result"
      for(a in 1:nrow(z)){
        z[a,1] <- trim(z[a,1])
      }
      
      if(colnames(gene)[u] %in% z$result){gene[q,u] <- 1}
    }
  }  
  
  
  #make a count table of the result columns 14 plus
  output <- as.data.frame(NA)
  output$result <- ""
  output$count <- 0
  output <- output[,-c(1)]
  for(g in 14:ncol(gene)){
    t <- as.data.frame(table(gene[[colnames(gene)[g]]]))
    output[g-13,1] <- as.character(colnames(gene)[g])
    output[g-13,2] <- t$Freq[t$Var1 == 1]
  }
  
  #add summary stats columns to output
  output$total_isolates <- nrow(gene)
  output$percent_with_result <- 100*(output$count / output$total_isolates)
  
  #add columns to output for averages of percentages and min and max
  output$average_percent_coverage <- 0
  output$min_percent_coverage <- 0
  output$max_percent_coverage <- 0
  #p <- 19
  for(p in 14:ncol(gene)){
    st <- gene %>%
      filter(gene[[colnames(gene)[p]]] == 1)
    output[p-13,5] <- mean(st$percent_coverage_mean)
    output[p-13,6] <- min(st$percent_coverage_min)
    output[p-13,7] <- max(st$percent_coverage_max)
  }
  
  
  #add the gene name to the workbook
  addWorksheet(OUT, as.character(unique_genes$Gene[k]))
  writeData(OUT, sheet = as.character(unique_genes$Gene[k]), x = output)
  addWorksheet(OUT2, as.character(unique_genes$Gene[k]))
  writeData(OUT2, sheet = as.character(unique_genes$Gene[k]), x = gene)
  
  
  
  
  
  print(k)
}

saveWorkbook(OUT, "NGS QA by Gene.xlsx", overwrite = TRUE) 
saveWorkbook(OUT2, "NGS by result per gene.xlsx", overwrite = TRUE) 
















          


