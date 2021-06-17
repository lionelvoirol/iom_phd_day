library(chorddiag)
load("data/mat_3.rda")
complete_names = colnames(mat_3)
abr = substr( complete_names , start = 1 , stop = 3 )
rownames(mat_3) = abr
colnames(mat_3) = abr
diag(mat_3) <- 0
mat_3[lower.tri(mat_3)]  <- t(mat_3)[lower.tri(mat_3)]
mat_3 = mat_3/52
# isSymmetric(mat_3)
l2 = chorddiag(mat_3, groupnamePadding = 10, categorynameFontsize = 6, tooltipNames = complete_names)
# l2
frameWidget(l2,  width ='100%')