library(pwr)
library(epiR)
library(powerSurvEpi)

epi.sscc(OR = 2.0, p0 = 0.30, n = NA, power = 0.80, r = 1, conf.level = 0.95)
epi.sscc(OR = 2.0, p0 = 0.30, n = NA, power = 0.80, r = 2, conf.level = 0.95)

epi.sscc(OR = 2.5, p0 = 0.2, n = NA, power = 0.80, r = 1, conf.level = 0.95)

epi.sscc(OR = 2.5, p0 = 0.2, n = NA, power = 0.80, r = 1, conf.level = 0.9)

expovunexp = 0.5/0.5
epi.sscohortc(irexp1 = .1, irexp0 = 0.05, r=expovunexp)

irexp0 = (5 * 413)/100000
epi.sscohortc(irexp1 = NA, irexp0 = irexp0, pexp = NA, n = 5000,
              power = 0.90, r = 1, N = NA, design = 1, sided.test = 1,
              finite.correction = FALSE, nfractional = FALSE, conf.level = 0.95)
irexp1 = 1.4 * (5 * 413)/100000; irexp0 = (5 * 413)/100000
epi.sscohortc(irexp1 = irexp1, irexp0 = irexp0, pexp = NA, n = NA,
              power = 0.90, r = 1, N = NA, design = 1, sided.test = 1,
              finite.correction = FALSE, nfractional = FALSE, conf.level = 0.95)

expovunexp = 0.3/0.7
epi.sscohortc(irexp1 = .1, irexp0 = 0.05, r=expovunexp)

expovunexp = 0.5/0.5
epi.sscohortc(irexp1 = .1, irexp0 = 0.05, r=expovunexp)

epi.sscompc(treat =200, control = 180, n = NA, sigma = 50, power = 0.8, r = 1,
            design = 1, conf.level = 0.95)

epi.sscompb(treat = .1, control = .2, n = 40, power = NA)

epi.sscompb(treat = .05, control = .2, n = 40, power = NA)

epi.sscompb(treat = 2/30, control = 4/25, n = 55, r = 25/30, power = NA)
epi.sscompb(treat = 2/30, control = 4/25, n = NA, r = 25/30, power = 0.8)

epi.sscompb(n = NA,
            treat = 4/25,
            control = 2/30,
            r = 25/30,
            power = 0.8)

binom.test(x = 0, n = 150, conf.level = 0.95, alternative = "two.sided")

prop.test(x = 150, n = 300, conf.level=0.95, correct = FALSE)

epi.sscomps(treat = 0.45, control = 0.30, n = NA, power = 0.90,
            r = 1, design = 1, sided.test = 2, conf.level = 0.95)

epi.sscomps(treat = 0.6, control = 0.3, n = NA, power = 0.8, r = 1)

epi.sscompb(treat = 0.6, control = 0.3, n = NA, power = 0.8, r = 1)

treatexp = sqrt(0.3)
epi.sscompb(treat = treatexp, control = 0.3, n = NA, power = 0.8, r = 1)

sintto=0.17
contto=sqrt(sintto)
epi.sscomps(hazard=0.5, n = NA, power = 0.8, r = 1)

ssizeEpi.default(power = 0.8, theta = 0.5, p = NA, p = NA)

epi.sssimpleestb(N = 1E+06, Py = NA, epsilon.r, conf.level = 0.95)

binom.test(x = 105, n=600, p=0.2, alternative = "two.sided", conf.level = 0.9)
