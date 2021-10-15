## Reading the data
data_gexp <- read.delim("https://github.com/odomlab2/coding-exercises/blob/main/01_spreadsheets/data/E-GEOD-52564-query-results.csv?raw=true",comment.char = "#")
Genes_oxphos <- read.delim("https://github.com/odomlab2/coding-exercises/raw/main/01_spreadsheets/data/go-oxidate_phosphorylation.tsv?raw=true")

## Checking the data : content, dimensions of the datasets
head(data_gexp)
print(c("dimensions of the TPM dataset: "
        ,paste(dim(data_gexp),collapse="x")))
head(Genes_oxphos)
print(c("dimensions of the GO dataset: "
        ,paste(dim(Genes_oxphos),collapse="x")))

## Subsetting : gene expression values of only those genes in the GO term list
genes <- intersect(data_gexp$Gene.Name
                   ,Genes_oxphos$Gene.product)

data_gexp_oxphos <- subset(data_gexp
                           ,data_gexp$Gene.Name%in%genes)
## Checking dimensions of the new subset
print(c("dimensions of the new subset of gene expression data: "
        ,paste(dim(data_gexp_oxphos)
               ,collapse="x")))

## Writing the subset
write.csv(data_gexp_oxphos
          ,"expression_oxphos.csv"
          ,row.names = F)
## Identifying file location (current working directory)
print(c("Location of the file- expression_oxphos.csv:"
        , getwd()))

## filtering the subset from above - taking only TPM values>10 and computing mean for each cell type
filter_mean <- function(vec,th){
  new_vec = vec[vec>=th]
  return(mean(new_vec
              ,na.rm=T))
}

data_gexp_mean <-lapply(data_gexp_oxphos[,-c(1,2)]
                        ,filter_mean,10)

## Writing mean expression data computed above for each cell type to file
write.csv(data_gexp_mean,"expression_oxphos_mean_per_celltype.csv"
          ,row.names = "Mean expression_oxphos")

## Identifying file location (current working directory)
print(c("Location of the file - expression_oxphos_mean_per_celltype.csv:", getwd()))

