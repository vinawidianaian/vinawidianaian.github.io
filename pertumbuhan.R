setwd("D:/METOPEL UAS/VINA METOPEL")
library(readxl)
library(writexl)
library(tidyverse)
library(kableExtra)
library(WDI)

# Mendefinisikan indikator
indi <- c(
          "pdb"="NY.GDP.MKTP.CD", 
          "pdbp"="NY.GDP.PCAP.CD", 
          "pop"="SP.POP.TOTL", 
          "peng"="SL.UEM.TOTL.ZS", 
          "kons"="NE.CON.PRVT.CD", 
          "inv"="NE.GDI.TOTL.ZS", 
          "eks"="NE.EXP.GNFS.CD", 
          "imp"="NE.IMP.GNFS.CD", 
          "infl"="FP.CPI.TOTL.ZG"
)

dat<-WDI(           # Menarik data World Bank
  country="IDN", # Ganti nama negaranya sesuai kelompok
  indicator=indi,
  start=1990,end=2023,
)

## Mengeksport dat ke excel (jika perlu)
write_xlsx(dat,"pertumbuhan.xlsx")

read_excel("pertumbuhan.xlsx")
dat <- read_excel("pertumbuhan.xlsx")
kbl(dat) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# regresi
reg1<-lm(pdb~pdbp+pop+peng+kons+inv+eks+imp+infl,data=dat)
summary(reg1)

# Membuat data frame
data <- data.frame(
  year = c(1990:2022),
  pdb = c(1.06E+11, 1.17E+11, 1.28E+11, 1.58E+11, 1.77E+11, 2.02E+11, 2.27E+11, 2.16E+11, 9.54E+10, 1.4E+11, 1.65E+11, 1.6E+11, 1.96E+11, 2.35E+11, 2.57E+11, 2.86E+11, 3.65E+11, 4.32E+11, 5.1E+11, 5.4E+11, 7.55E+11, 8.93E+11, 9.18E+11, 9.13E+11, 8.91E+11, 8.61E+11, 9.32E+11, 1.02E+12, 1.04E+12, 1.12E+12, 1.06E+12, 1.19E+12, 1.32E+12),
  pdbp = c(582.679, 629.1607, 678.9777, 824.0791, 907.4718, 1020.147, 1129.093, 1054.347, 459.1919, 663.5232, 770.8654, 739.0039, 888.9014, 1052.413, 1136.755, 1249.398, 1572.798, 1840.33, 2144.39, 2239.095, 3094.443, 3613.801, 3668.212, 3602.886, 3476.625, 3322.582, 3558.819, 3839.785, 3902.662, 4151.228, 3895.618, 4334.216, 4787.999),
  pop = c(1.82E+08, 1.85E+08, 1.89E+08, 1.92E+08, 1.95E+08, 1.98E+08, 2.01E+08, 2.05E+08, 2.08E+08, 2.11E+08, 2.14E+08, 2.17E+08, 2.2E+08, 2.23E+08, 2.26E+08, 2.29E+08, 2.32E+08, 2.35E+08, 2.38E+08, 2.41E+08, 2.44E+08, 2.47E+08, 2.5E+08, 2.53E+08, 2.56E+08, 2.59E+08, 2.62E+08, 2.64E+08, 2.67E+08, 2.7E+08, 2.72E+08, 2.74E+08, 2.76E+08),
  peng = c(NA, 2.62, 2.73, 2.78, 4.37, 4.597, 4.86, 4.68, 5.46, 6.36, 6.08, 6.08, 6.6, 6.66, 7.3, 7.94, 7.55, 8.06, 7.21, 6.11, 5.61, 5.15, 4.47, 4.34, 4.05, 4.51, 4.3, 3.78, 4.39, 3.59, 4.25, 3.83, 3.554),
  kons = c(6.74E+10, 7.49E+10, 8.05E+10, 9.25E+10, 1.06E+11, 1.24E+11, 1.42E+11, 1.33E+11, 6.47E+10, 1.04E+11, 1.02E+11, 1.01E+11, 1.32E+11, 1.6E+11, 1.71E+11, 1.84E+11, 2.28E+11, 2.75E+11, 3.09E+11, 3.17E+11, 4.24E+11, 4.95E+11, 5.18E+11, 5.19E+11, 5.09E+11, 4.95E+11, 5.39E+11, 5.82E+11, 5.94E+11, 6.48E+11, 6.24E+11, 6.6E+11, 7E+11),
  inv = c(32.70769, 31.86943, 30.17583, 28.28067, 29.57069, 30.42981, 31.60236, 30.30768, 27.42951, 22.13876, 22.2457, 22.53927, 21.40407, 25.5985, 24.05637, 25.08141, 25.40022, 24.92028, 27.81624, 30.98519, 32.88012, 32.98433, 35.07159, 33.83136, 34.60034, 34.06279, 33.85874, 33.71059, 34.57059, 33.78014, 32.34341, 31.44898, 29.74534),
  eks = c(2.9E+10, 3.31E+10, 3.88E+10, 4.23E+10, 4.69E+10, 5.32E+10, 5.87E+10, 6.01E+10, 5.06E+10, 4.97E+10, 6.76E+10, 6.26E+10, 6.4E+10, 7.16E+10, 8.27E+10, 9.74E+10, 1.13E+11, 1.27E+11, 1.52E+11, 1.3E+11, 1.83E+11, 2.35E+11, 2.26E+11, 2.18E+11, 2.11E+11, 1.82E+11, 1.78E+11, 2.05E+11, 2.19E+11, 2.08E+11, 1.84E+11, 2.54E+11, 3.23E+11),
  imp = c(2.72E+10, 3.09E+10, 3.47E+10, 3.76E+10, 4.49E+10, 5.59E+10, 6.01E+10, 6.07E+10, 4.12E+10, 3.84E+10, 5.03E+10, 4.94E+10, 5.16E+10, 5.43E+10, 7.07E+10, 8.55E+10, 9.34E+10, 1.1E+11, 1.47E+11, 1.15E+11, 1.69E+11, 2.13E+11, 2.29E+11, 2.26E+11, 2.17E+11, 1.79E+11, 1.71E+11, 1.95E+11, 2.3E+11, 2.13E+11, 1.66E+11, 2.23E+11, 2.76E+11),
  infl = c(7.819191, 9.419058, 7.523517, 9.671893, 8.532005, 9.420323, 7.973281, 6.226142, 58.45104, 20.47783, 3.688619, 11.50011, 11.90012, 6.757317, 6.06406, 10.4532, 13.10867, 6.406563, 10.22666, 4.386416, 5.134204, 5.356048, 4.2795, 6.412513, 6.394925, 6.363121, 3.525805, 3.808798, 3.198346, 3.030587, 1.920968, 1.56013, 4.209464)
)

# Mengubah format data frame dari wide ke long
data_long <- data %>% pivot_longer(cols = c(pdb, pdbp, pop, peng, kons, inv, eks, imp, infl), names_to = "Variable", values_to = "Value")

#Plot
ggplot(data_long, aes(x=year, y=Value, color=Variable)) +
  geom_line() +
  labs(title="Plot Tahun vs Variabel", x="Tahun", y="Variabel") +
  theme_minimal() +
  scale_color_discrete(name = "Variabel")


# Membuat model regresi linier
reg1 <- lm(pdb ~ pdbp + pop + peng + kons + inv + eks + imp + infl, data = dat)

# Menghitung residu
dat$resid <- resid(reg1)

# Membuat plot error untuk setiap variabel independen
variables <- c("pdbp", "pop", "peng", "kons", "inv", "eks", "imp", "infl")
for(i in 1:length(variables)) {
  plot(dat[[variables[i]]], dat$resid, xlab=variables[i], ylab="Error")
  abline(h=0) # membuat garis horizontal di y=0
}
