## This lengthy block of code calculates all of the t-test required for this analysis. It is omitted from the
## report file because of its length. It is called by Part 2.Rmd.

ii = 0
for (i in c(0.5, 1.0, 2.0)){
        ii = ii+1 
        jj = 0
        for (j in c("OJ", "VC")){
                jj = jj + 1
                assign(paste0("df",ii,jj), filter(ToothGrowth, Delivery.Meth == j & Dose == i))
                
        }
}

## create 3 possible combinations of dose by treatment
df1. <- rbind(df11,df12) ## treatment by dose at 0.5
df2. <- rbind(df21,df22) ## treatment by dose at 1.0
df3. <- rbind(df31,df32) ## treatment by dose at 2.0

## create 6 possible combinations of treatment by dose
df.1.1 <- rbind(df11,df21) ## OJ at .05 and 1.0
df.1.2 <- rbind(df11,df31) ## OJ at .05 and 2.0
df.1.3 <- rbind(df21,df31) ## OJ at 1.0 and 2.0

df.2.1 <- rbind(df12,df22) ## VC at .05 and 1.0
df.2.2 <- rbind(df12,df32) ## VC at .05 and 2.0
df.2.3 <- rbind(df22,df32) ## VC at 1.0 and 2.0

## run first three t.tests on possible combinations of dose by treatment
tt1. <- t.test(len ~ Delivery.Meth, data = df1., alternative = "t", paired = F, var.equal = T)
tt2. <- t.test(len ~ Delivery.Meth, data = df2., alternative = "t", paired = F, var.equal = T)
tt3. <- t.test(len ~ Delivery.Meth, data = df3., alternative = "t", paired = F, var.equal = T)

ttt1 <- data.frame(Test_Conducted=as.character(),tvalue=as.double(),pvalue=as.double(),
                   Significant = as.logical(), Bonferroni = as.double(), BH = as.double(), stringsAsFactors = FALSE)
ttt1[1,1] <- "H_0: mean_OJ = mean_VC - Dose = 0.5 mg/day"
ttt1[1,2] <- format(tt1.$statistic, digits = 3, nsmall = 3)
ttt1[1,3] <- format(tt1.$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt1.$p.value <= 0.05) {ttt1[1,4] <- TRUE} else {ttt1[1,4] <- FALSE}

ttt1[2,1] <- "H_0: mean_OJ = mean_VC - Dose = 1.0 mg/day"
ttt1[2,2] <- format(tt2.$statistic, digits = 3, nsmall = 3)
ttt1[2,3] <- format(tt2.$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt2.$p.value <= 0.05) {ttt1[2,4] <- TRUE} else {ttt1[2,4] <- FALSE}

ttt1[3,1] <- "H_0: mean_OJ = mean_VC - Dose = 2.0 mg/day"
ttt1[3,2] <- format(tt3.$statistic, digits = 3, nsmall = 3)
ttt1[3,3] <- format(tt3.$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt3.$p.value <= 0.05) {ttt1[3,4] <- TRUE} else {ttt1[3,4] <- FALSE}

## Conduct Bonferroni and Benjamini-Hochberg corrections

ttt1[,5] <- format(p.adjust(ttt1[,3], method = "bonferroni"),scientific = FALSE, digits = 5, nsmall = 5)
ttt1[,6] <- format(p.adjust(ttt1[,3], method = "BH"),scientific = FALSE, digits = 5, nsmall = 5)

## run three t.tests on possible combinations of OJ by dose
tt.1.1 <- t.test(len ~ Dose, data = df.1.1, alternative = "t", paired = F, var.equal = T)
tt.1.2 <- t.test(len ~ Dose, data = df.1.2, alternative = "t", paired = F, var.equal = T)
tt.1.3 <- t.test(len ~ Dose, data = df.1.3, alternative = "t", paired = F, var.equal = T)

ttt2 <- data.frame(Test_Conducted=as.character(), tvalue=as.double(),pvalue=as.double(),
                   Significant = as.logical(), stringsAsFactors = FALSE, Bonferroni = as.double(), BH = as.double())
ttt2[1,1] <- "H_0: mean_dosea = mean_doseb - 0.5 to 1.0 mg/day, with Orange Juice delivery"
ttt2[1,2] <- format(tt.1.1$statistic, digits = 3, nsmall = 3)
ttt2[1,3] <- format(tt.1.1$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt.1.1$p.value <= 0.05) {ttt2[1,4] <- TRUE} else {ttt2[1,4] <- FALSE}

ttt2[2,1] <- "H_0: mean_dosea = mean_doseb - 0.5 to 2.0 mg/day, with Orange Juice delivery"
ttt2[2,2] <- format(tt.1.2$statistic, digits = 3, nsmall = 3)
ttt2[2,3] <- format(tt.1.2$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt.1.2$p.value <= 0.05) {ttt2[2,4] <- TRUE} else {ttt2[2,4] <- FALSE}

ttt2[3,1] <- "H_0: mean_dosea = mean_doseb - 1.0 to 2.0 mg/day, with Orange Juice delivery"
ttt2[3,2] <- format(tt.1.3$statistic, digits = 3, nsmall = 3)
ttt2[3,3] <- format(tt.1.3$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt.1.3$p.value <= 0.05) {ttt2[3,4] <- TRUE} else {ttt2[3,4] <- FALSE}

## run three t.tests on possible combinations of VC by dose
tt.2.1 <- t.test(len ~ Dose, data = df.2.1, alternative = "t", paired = F, var.equal = T)
tt.2.2 <- t.test(len ~ Dose, data = df.2.2, alternative = "t", paired = F, var.equal = T)
tt.2.3 <- t.test(len ~ Dose, data = df.2.3, alternative = "t", paired = F, var.equal = T)

ttt2[4,1] <- "H_0: mean_dosea = mean_doseb - 0.5 to 1.0 mg/day, with Ascorbic Acid delivery"
ttt2[4,2] <- format(tt.2.1$statistic, digits = 3, nsmall = 3)
ttt2[4,3] <- format(tt.2.1$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt.2.1$p.value <= 0.05) {ttt2[4,4] <- TRUE} else {ttt2[4,4] <- FALSE}

ttt2[5,1] <- "H_0: mean_dosea = mean_doseb - 0.5 to 2.0 mg/day, with Ascorbic Acid delivery"
ttt2[5,2] <- format(tt.2.2$statistic, digits = 3, nsmall = 3)
ttt2[5,3] <- format(tt.2.2$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt.2.2$p.value <= 0.05) {ttt2[5,4] <- TRUE} else {ttt2[5,4] <- FALSE}

ttt2[6,1] <- "H_0: mean_dosea = mean_doseb - 1.0 to 2.0 mg/day, with Ascorbic Acid delivery"
ttt2[6,2] <- format(tt.2.3$statistic, digits = 3, nsmall = 3)
ttt2[6,3] <- format(tt.2.3$p.value,scientific = FALSE, digits = 3, nsmall = 3)
if (tt.2.3$p.value <= 0.05) {ttt2[6,4] <- TRUE} else {ttt2[6,4] <- FALSE}

## Conduct Bonferroni and Benjamini-Hochberg corrections
ttt2[,5] <- format(p.adjust(ttt2[,3], method = "bonferroni"), scientific = FALSE, digits = 3, nsmall = 3)
ttt2[,6] <- format(p.adjust(ttt2[,3], method = "BH"), scientific = FALSE, digits = 3, nsmall = 3)
