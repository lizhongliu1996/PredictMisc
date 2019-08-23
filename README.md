# PredictMisc
package that do prediction analysis for methylation regions


### Required Packages
Because you must install this package from GitLab, you will need to have a few packages installed on your machine before you can build this package.

- `Aclust`: install this from <https://github.com/tamartsi/Aclust>. This package also suggests the Bioconductor package `AnnotationDbi`.
- `caret`
- `data.table`
- `dplyr`
- `pathwayPCA`: install this from <https://github.com/gabrielodom/pathwayPCA>
- `pROC`
- `tibble`

### purpose
Typically, when using doing methylation predictions, there are 450k number of CpGs, how to select predictors is a tricky problem. Usually, people choose CpGs that are most significant or CpGs with most varilibity, here we present a new method using differential methylation regions to select CpGs as prodictors
