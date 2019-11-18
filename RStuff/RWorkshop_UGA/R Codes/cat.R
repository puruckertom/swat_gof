M <- as.table(rbind(c(25, 12), c(11,14)))
dimnames(M) <- list(education=c("High School","College"),
                    income=c("Low","High"))
res <- chisq.test(M)
res$expected
res

