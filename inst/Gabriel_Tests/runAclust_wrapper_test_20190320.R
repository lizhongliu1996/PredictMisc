
# Function
runAcluster <- function(chromeAnnot_df, tBeta_df, minCpGs = 3){

  out_ls <- assign.to.clusters(
    betas = tBeta_df,
    annot = as.data.table(chromeAnnot_df),
    dist.thresh = 0.5,
    bp.merge = 200,
    dist.type = "spearman",
    method = "complete"
  )

  keep_idx <- which(lengths(out_ls) >= minCpGs)
  out_ls[keep_idx]

}


# Test
data(Example_df)
data(chrome_annot_files)

BetaTrans_df <- pathwayPCA::TransposeAssay(
  Example_df[, -1],
  omeNames = "rowNames"
)

library(Aclust)
runAcluster(chrome_annot_files$chr1, tBeta_df = BetaTrans_df)

a <- Sys.time()
test_ls <- lapply(chrome_annot_files, runAcluster, tBeta_df = BetaTrans_df)
Sys.time() - a
# 53.03345 sec
