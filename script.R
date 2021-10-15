## Given Task:##############################################################################
## 1. Extract the expression values for all genes that are part of the GO-term oxidative 
## phosporylation and store it as "expression_oxphos.csv".
## 2. Filter the dataset from step (1) for genes with an expression with a TPM of 10 or higher. 
## Then, calculate the mean expression for the group of extracted genes for each of the eight 
## cell types and store it as "expression_oxphos_mean_per_celltype.csv".
############################################################################################
## Libraries used to complete the task : base R
## Data Files:
## 1. Source :https://github.com/odomlab2/coding-exercises/blob/main/01_spreadsheets/data
## File Name: 'E-GEOD-52564-query-results.csv'
## Description: RNA-Seq Expression values as TPM (Transcripts Per Million) of cells in 
## the mouse cerebral cortex

## 2. Source :https://github.com/odomlab2/coding-exercises/raw/main/01_spreadsheets/data
## File name: 'go-oxidate_phosphorylation.tsv'
## Description: Gene list for the Gene Ontology (GO) term oxidative phosphorylation in mouse

### Reading the files ########################################################################
### variables names for the datasets: data_gexp (TMP dataset); 
### Genes_oxphos (genelist for the GOterm)

data_gexp <- read.delim("https://github.com/odomlab2/coding-exercises/blob/main/01_spreadsheets/data/E-GEOD-52564-query-results.csv?raw=true",comment.char = "#")
Genes_oxphos <- read.delim("https://github.com/odomlab2/coding-exercises/raw/main/01_spreadsheets/data/go-oxidate_phosphorylation.tsv?raw=true")

## Checking the data : content, dimensions of the datasets
head(data_gexp)
print(c("dimensions of the TPM dataset: "
        ,paste(dim(data_gexp),collapse="x")))
head(Genes_oxphos)
print(c("dimensions of the GO dataset: "
        ,paste(dim(Genes_oxphos),collapse="x")))

### Task 1 #################################################################################

## Given: Extract the expression values for all genes that are part of the GO-term oxidative 
## phosporylation and store it as "expression_oxphos.csv"

## Step1: Extracting the data
## Genes that are present in both the data_gexp (column- Gene.Name) and Genes_oxphos 
## (column - Gene.product) are identified 
genes <- intersect(data_gexp$Gene.Name
                   ,Genes_oxphos$Gene.product)
## A subset of data_gexp, data_gexp_oxphos, that contains only the genes identified above 
## is extracted
data_gexp_oxphos <- subset(data_gexp
                           ,data_gexp$Gene.Name%in%genes)

print(c("dimensions of the new subset of gene expression data: "
        ,paste(dim(data_gexp_oxphos)
               ,collapse="x")))

## Step2: Writing to file
## The extracted subset, data_gexp_oxphos, is written to a file named "expression_oxphos.csv".
## File is saved in the current working directory.

write.csv(data_gexp_oxphos
          ,"expression_oxphos.csv"
          ,row.names = F)
print(c("Location of the file- expression_oxphos.csv:"
        , getwd()))


## Task 2 ###################################################################################
## Given: Filter the dataset from step (1) for genes with an expression with a TPM of 10 
## or higher. Then, calculate the mean expression for the group of extracted genes for each 
## of the eight cell types and store it as "expression_oxphos_mean_per_celltype.csv"

## Step1: Computing the mean expression for each cell type taking only genes with TPM>= 10 

## Defining a function that selects elements from a vector (vec) that are greater than or 
## equal to a given threshold (th) and thereafter computes its mean
filter_mean <- function(vec,th){
  new_vec = vec[vec>=th]
  return(mean(new_vec
              ,na.rm=T))
}
## filter_mean function with threshold 10 is applied to every column vector 
## (that corresponds to each celltype) in the data_gexp_oxphos dataset

data_gexp_mean <-lapply(data_gexp_oxphos[,-c(1,2)]
                        ,filter_mean,10)

## Step2: mean expression of the genes computed for each cell type stored in the variable 
## data_gexp_mean,is written to the file: "expression_oxphos_mean_per_celltype.csv. 
## File is saved in the current working directory.

write.csv(data_gexp_mean,"expression_oxphos_mean_per_celltype.csv"
          ,row.names = "Mean expression_oxphos")

print(c("Location of the file - expression_oxphos_mean_per_celltype.csv:", getwd()))
      
      