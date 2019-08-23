##this is the script to generate pfcInfo_df file
library(PredictMisc)
data(pheno)

pfcInfo_df <- summaryInfo(
      NumOfRep = 10,
      NumOfCv = 5,
      seed = 122,
      phenoData_df = pheno
)
