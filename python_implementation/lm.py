from my import *
from math import *
from lmfn import *
y = [1,3,2,5,7,8,8,9,10,12]
y2 = [1,1,1,0,0,1,0,1,0,1]
x1 = [0,1,2,3,4,5,6,7,8,9]
x2 =[3,4,5,6,7,3,2,7,5,3]
x3 = [6,7,8,4,2,1,3,3,5,6]
xs = [x1,x2,x3] #col
xtx = [] 
alpha = 0.05
predx = [3,2,4]
has_intercept = True
ridge = False
k = 0.003
#==== end of define global var
seereg = True
seeformula = True
seeyhat = False
seeresidual = False
seeC = False
seePii = False
seeintR = False
seeextR = False
seecorrM = False
seeprplot = False
seecook = False
seedfits = False
seehadi = False
seeetet = False
needci = 0 # set to 1 if need
#==== end of setting system var

class lm:
  def __init__(self):
    self.y = "None"
    self.x = "None"
    self.n = "None"
    self.p = "None"
    self.X = "None"
    self.xtx = "None"
    self.C = "None"
    self.P = "None"
    self.pii = "None"
    self.piith = "None"
    self.beta = "None"
    self.xy_n_1 = "None"
    self.yhats = "None"
    self.res = "None"
    self.Setzetz1 = "None"
    self.dwnom = "None"
    self.Setet  = "None"
    self.dwdenom = "None"
    self.dw = "None"
    self.Setetz1 = "None"
    self.autocorrnom = "None"
    self.autocorrdenom = "None"
    self.autocorr = "None"
    self.mulrr = "None"
    self.SSE = "None"
    self.MSE = "None"
    self.SST = "None"
    self.MST = "None"
    self.SSR = "None"
    self.MSR = "None"
    self.F = "None"
    self.R2 = "None"
    self.R2adj = "None"
    self.sigmahat = "None"
    self.sebeta = "None"
    self.tstats = "None"
    self.intR = "None"
    self.run = "None"
    self.posrun = "None"
    self.negrun = "None"
    self.murun = "None"
    self.stdrun = "None"
    self.zrun = "None"
    self.extR = "None"
    self.xandy = "None"
    self.cook = "None"
    self.potential = "None"
    self.dfits = "None"
    self.normres = "None"
    self.residualf = "None"
    self.hadi = "None"
    self.AIC = "None"
    self.BIC = "None"
    self.n_p_1 = "None"
    self.n_1 = "None"
    self.pplus1 = "None"
    self.SSTdivn_1 = "None"
    self.check(print = False)
    self.check(print = False)
  def check(self, print = True):
    global ridge, k
    if self.y != "None":
      self.n = len(self.y)
    if self.x != "None":
      self.p = len(self.x)
      self.X = [[1 for i in range(len(self.x[0]))]]
      [self.X.append(x) for x in self.x]
      self.X = tr(self.X)
      self.xtx = mul(tr(self.X), self.X)
    if self.n != "None" and self.p!= "None":
      self.n_p_1 = self.n - self.p - 1
    if self.n != "None":
      self.n_1 = self.n - 1
    if self.p != "None":
      self.pplus1 = self.p + 1
    if self.xtx != "None" and ridge != True:
      self.C = inv(self.xtx)
    if ridge == True and self.xtx != "None" and self.n != "None" and self.p != None:
      self.C = inv(add(div(self.xtx, self.n-1), mul(k, Imat(self.p + 1))))
    if self.X != "None":
      self.P = Pfn(self.X)
    if self.P != "None":
      self.pii = piifn(self.P)
    if self.pii != "None":
      self.piith = piithfn(self.pii)
    if self.X != "None" and self.y != "None" and ridge != True:
      self.beta = betafn(self.X,self.y)
    if self.X != "None" and self.y != "None" and self.n != "None" and ridge == True:
      self.xy_n_1 = ridgexy1fn(self.X, self.y, self.n)
    if self.xy_n_1 != "None" and self.C != "None" and ridge == True:
      self.beta = ridgebetafn(self.C, self.xy_n_1)
    if self.X != "None" and self.beta != "None":
      self.yhats = yhatsfn(self.X, self.beta)
    if self.y != "None" and self.yhats != "None":
      self.res = resfn(self.y, self.yhats)
    if self.res != "None":
      self.Setzetz1 = Setzetz1fn(self.res)
    if self.Setzetz1 != "None":
      self.dwnom = dwnomfn(self.Setzetz1)
    if self.res != "None":
      self.Setet = Setetfn(self.res)
    if self.Setet != "None":
      self.dwdenom = dwdenomfn(self.Setet)
      self.autocorrdenom = self.dwdenom
    if self.dwnom != "None" and self.dwdenom != "None":
      self.dw = dwfn(self.dwnom, self.dwdenom)
    if self.res != "None":
      self.Setetz1 = Setetz1fn(self.res)
    if self.Setetz1 != "None":
      self.autocorrnom = autocorrnomfn(self.Setetz1)
    if self.autocorrnom != "None" and self.autocorrdenom != "None":
      self.autocorr = autocorrfn(self.autocorrnom, self.autocorrdenom)
    if self.res != "None":
      self.mulrr = mulrrfn(self.res)
    if self.mulrr != "None":
      self.SSE = SSEfn(self.mulrr)
    if self.SSE != "None" and self.n != "None" and self.p != "None":
      self.MSE = MSEfn(self.SSE, self.n, self.p)
    if self.y != "None":
      self.SST = SSTfn(self.y)
    if self.SST != "None" and self.n != "None":
      self.SSTdivn_1 = self.SST / (self.n - 1)
      self.MST = MSTfn(self.SST, self.n)
    if self.SST != "None" and self.SSE != "None":
      self.SSR = SSRfn(self.SST, self.SSE)
    if self.SSR != "None" and self.p != "None":
      self.MSR = MSRfn(self.SSR, self.p)
    if self.MSR != "None" and self.MSE != "None":
      self.F = Ffn(self.MSR, self.MSE)
    if self.SSR != "None" and self.SST != "None":
      self.SSE = self.SST - self.SSR
    if self.SSR != "None" and self.SST != "None":
      self.R2 = R2fn(self.SSR, self.SST)
    if self.SSE != "None" and self.SST != "None" and self.n != "None" and self.p != "None":
      self.R2adj = R2adjfn(self.SSE, self.SST, self.n, self.p)
    if self.SSE != "None" and self.n != "None" and self.p != "None":
      self.sigmahat = sigmahatfn(self.SSE, self.n, self.p)
    if self.C != "None" and self.sigmahat != "None" and self.beta != "None":
      self.sebeta = sebetafn(self.C, self.sigmahat, self.beta)
    if self.beta != "None" and self.sebeta != "None":
      self.tstats = tstatsfn(self.beta, self.sebeta)
    if self.res != "None" and self.sigmahat != "None" and self.pii != "None":
      self.intR = intRfn(self.res, self.sigmahat, self.pii)
    if self.intR != "None":
      self.run = runfn(self.intR)
      self.posrun = posrunfn(self.intR)
      self.negrun = negrunfn(self.intR)
    if self.posrun != "None" and self.negrun != "None":
      self.murun, self.stdrun = exprun(self.posrun, self.negrun)
    if self.run != "None" and self.murun != "None" and self.stdrun != "None":
      self.zrun = zrunfn(self.run, self.murun, self.stdrun)
    if self.intR != "None" and self.n != "None" and self.p != "None":
      self.extR = extRfn(self.intR, self.n, self.p)
    if self.extR != "None" and self.n != "None" and self.p != "None":
      self.intR = extRtointR(self.extR, self.n, self.p)
    if self.X != "None" and self.y != "None":
      self.xandy = xandyfn(self.X, self.y)
    if self.intR != "None" and self.p != "None" and self.pii != "None":
      self.cook = cookfn(self.intR, self.p, self.pii)
    if self.pii != "None":
      self.potential = potentialfn(self.pii)
    if self.extR != "None" and self.pii != "None":
      self.dfits = dfitsfn(self.extR, self.pii)
    if self.SSE != "None" and self.res != "None":
      self.normres = normresfn(self.SSE, self.res)
    if self.p != "None" and self.pii != "None" and self.normres != "None":
      self.residualf = residualfn(self.p, self.pii, self.normres)
    if self.pii != "None" and self.residualf != "None":
      self.hadi = hadifn(self.pii, self.residualf)
    if self.n != "None" and self.SSE != "None" and self.p != "None":
      self.AIC = AICfn(self.n, self.SSE, self.p)
      self.BIC = BICfn(self.n, self.SSE, self.p)
    if print:
      self.print()
  def gy(self, y, print = True):
    self.y = y
    self.check(print = False)
    self.check(print)
  def gx(self, x, print = True):
    if type(x) == list and type(x[0]) != list:
      self.x = [x]
    elif type(x) == list and type(x[0]) == list:
      self.x = x
    self.check(print = False)
    self.check(print)
  def gn(self, n, print = True):
    self.n = n
    self.check(print = False)
    self.check(print)
  def gp(self, p, print = True):
    self.p = p
    self.check(print = False)
    self.check(print)
  def gX(self, X, print = True):
    self.X = X
    self.check(print = False)
    self.check(print)
  def gxtx(self, xtx, print = True):
    self.xtx = xtx
    self.check(print = False)
    self.check(print)
  def gC(self, C, print = True):
    self.C = C
    self.check(print = False)
    self.check(print)
  def gP(self, P, print = True):
    self.P = P
    self.check(print = False)
    self.check(print)
  def gpii(self, pii, print = True):
    self.pii = pii
    self.check(print = False)
    self.check(print)
  def gpiith(self, piith, print = True):
    self.piith = piith
    self.check(print = False)
    self.check(print)
  def gbeta(self, beta, print = True):
    self.beta = beta
    self.check(print = False)
    self.check(print)
  def gyhats(self, yhats, print = True):
    self.yhats = yhats
    self.check(print = False)
    self.check(print)
  def gres(self, res, print = True):
    self.res = res
    self.check(print = False)
    self.check(print)
  def gSetzetz1(self, Setzetz1, print = True):
    self.Setzetz1 = Setzetz1
    self.check(print = False)
    self.check(print)
  def gdwnom(self, dwnom, print = True):
    self.dwnom = dwnom
    self.check(print = False)
    self.check(print)
  def gSetet (self, Setet, print = True):
    self.Setet = Setet
    self.check(print = False)
    self.check(print)
  def gdwdenom(self, dwdenom, print = True):
    self.dwdenom = dwdenom
    self.check(print = False)
    self.check(print)
  def gdw(self, dw, print = True):
    self.dw = dw
    self.check(print = False)
    self.check(print)
  def gSetetz1(self, Setetz1, print = True):
    self.Setetz1 = Setetz1
    self.check(print = False)
    self.check(print)
  def gautocorrnom(self, autocorrnom, print = True):
    self.autocorrnom = autocorrnom
    self.check(print = False)
    self.check(print)
  def gautocorrdenom(self, autocorrdenom, print = True):
    self.autocorrdenom = autocorrdenom
    self.check(print = False)
    self.check(print)
  def gautocorr(self, autocorr, print = True):
    self.autocorr = autocorr
    self.check(print = False)
    self.check(print)
  def gmulrr(self, mulrr, print = True):
    self.mulrr = mulrr
    self.check(print = False)
    self.check(print)
  def gSSE(self, SSE, print = True):
    self.SSE = SSE
    self.check(print = False)
    self.check(print)
  def gMSE(self, MSE, print = True):
    self.MSE = MSE
    self.check(print = False)
    self.check(print)
  def gSST(self, SST, print = True):
    self.SST = SST
    self.check(print = False)
    self.check(print)
  def gMST(self, MST, print = True):
    self.MST = MST
    self.check(print = False)
    self.check(print)
  def gSSR(self, SSR, print = True):
    self.SSR = SSR
    self.check(print = False)
    self.check(print)
  def gMSR(self, MSR, print = True):
    self.MSR = MSR
    self.check(print = False)
    self.check(print)
  def gF(self, F, print = True):
    self.F = F
    self.check(print = False)
    self.check(print)
  def gR2(self, R2, print = True):
    self.R2 = R2
    self.check(print = False)
    self.check(print)
  def gR2adj(self, R2adj, print = True):
    self.R2adj = R2adj
    self.check(print = False)
    self.check(print)
  def gsigmahat(self, sigmahat, print = True):
    self.sigmahat = sigmahat
    self.check(print = False)
    self.check(print)
  def gsebeta(self, sebeta, print = True):
    self.sebeta = sebeta
    self.check(print = False)
    self.check(print)
  def gtstats(self, tstats, print = True):
    self.tstats = tstats
    self.check(print = False)
    self.check(print)
  def gintR(self, intR, print = True):
    self.intR = intR
    self.check(print = False)
    self.check(print)
  def grun(self, run, print = True):
    self.run = run
    self.check(print = False)
    self.check(print)
  def gposrun(self, posrun, print = True):
    self.posrun = posrun
    self.check(print = False)
    self.check(print)
  def gnegrun(self, negrun, print = True):
    self.negrun = negrun
    self.check(print = False)
    self.check(print)
  def gmurun(self, murun, print = True):
    self.murun = murun
    self.check(print = False)
    self.check(print)
  def gstdrun(self, stdrun, print = True):
    self.stdrun = stdrun
    self.check(print = False)
    self.check(print)
  def gzrun(self, zrun, print = True):
    self.zrun = zrun
    self.check(print = False)
    self.check(print)
  def gextR(self, extR, print = True):
    self.extR = extR
    self.check(print = False)
    self.check(print)
  def gxandy(self, xandy, print = True):
    self.xandy = xandy
    self.check(print = False)
    self.check(print)
  def gcook(self, cook, print = True):
    self.cook = cook
    self.check(print = False)
    self.check(print)
  def gpotential(self, potential, print = True):
    self.potential = potential
    self.check(print = False)
    self.check(print)
  def gdfits(self, dfits, print = True):
    self.dfits = dfits
    self.check(print = False)
    self.check(print)
  def gnormres(self, normres, print = True):
    self.normres = normres
    self.check(print = False)
    self.check(print)
  def gresidualf(self, residualf, print = True):
    self.residualf = residualf
    self.check(print = False)
    self.check(print)
  def ghadi(self, hadi, print = True):
    self.hadi = hadi
    self.check(print = False)
    self.check(print)
  def gAIC(self, AIC, print = True):
    self.AIC = AIC
    self.check(print = False)
    self.check(print)
  def gBIC(self, BIC, print = True):
    self.BIC = BIC
    self.check(print = False)
    self.check(print)
  def fit(self, y, x):
    self.gy(y, print = False)
    self.gx(x)
  #see var
  def print(self):
    global seereg, seeformula, seeyhat, seeresidual, seeC, seePii, seeintR, seeextR, seecorrM, seeprplot, seecook, seedfits, seehadi, seeetet, needci
    if seereg:
      print()
      print("+++++ START: Multi Regression +++++ ")
      print("number of data:{}, predictors: {}".format(self.n, self.p))
      see_list(self.beta, "beta")
      see_list(self.sebeta, "se_beta")
      see_list(self.tstats, "t_stats[df = {}]:".format(self.n_p_1))
      print("SSE[df:{}] = {}, MSE = RMS = {}".format(self.n_p_1, strf(self.SSE), strf(self.MSE)))
      print("SSR[df:{}] = {}, MSR = {}".format(self.p,strf(self.SSR), strf(self.MSR)))
      print("SST[df:{}] = {}, MST = {}".format(self.n_1,strf(self.SST), strf(self.MST)))
      print()
      print("R^2 = {}".format(strf(self.R2)))
      print("R^adj = 1 - (SSE/(n-p-1))/(SST/(n-1)) = {}".format(strf(self.R2adj)))
      print("sigma_hat = sqrt(SSE/(n-p-1)) = sqrt(SSE/{}) = {}".format(self.n_p_1,strf(self.sigmahat)))
      print("SST/(n-1) = {}".format(strf(self.SSTdivn_1)))
      print("F = MSR/MSE = {}/{} = {}".format(strf(self.MSR), strf(self.MSE), strf(self.F)))
      print()
      print("test run = {}, n1(pos run) = {}, n2 = {}, mu = {}, std = {}, Z = {}".format(self.run, self.posrun, self.negrun, strf(self.murun), strf(self.stdrun), strf(self.zrun)))
      print("DW[n={},p={}]: nom: {}, denom: {}, nom/denom = {}".format(self.n,self.p,strf(self.dwnom), strf(self.dwdenom), strf(self.dw)))
      print("Autocorr: nom: {}, denom: {}, nom/denom = {}".format(strf(self.autocorrnom), strf(self.autocorrdenom), strf(self.autocorr)))
      print("AIC = {}, BIC = {}".format(strf(self.AIC), strf(self.BIC)))
      ####
      if seeyhat == True:
        see_list(self.yhats, "yhats")
      if seeresidual == True:
        see_list(self.res, "residuals")
      if seeC == True:
        see_list(self.C, "Covariance Matrix")
      if seePii == True:
        see_list(self.P, "P matrix = X(XtX)-1Xt")
        see_list(self.pii, "Pii, threshold = {}".format(strf(self.piithr)))
      if seeintR == True:
        see_list(self.intR, "internal studentized residual(default this one), threshold = 3")
      if seeextR == True:
        see_list(self.extR, "external studentized residual, threshold = 3")
      if seecorrM == True:
        see_list(self.corrMat, "Correlation Matrix[ones, Xp, y]")
      if seecook == True:
        see_list(self.cook, "Cook's distance, threshold = F(p+1,n-p-1)")
      if seedfits == True:
        see_list(self.dfits, "DFITS, |threshold| = 2*sqrt((p+1)/(n-p-1)) = {}".format(strf(2 * sqrt((self.pplus1)/ (self.n_p_1)))))
      if seehadi == True:
        see_list(self.hadi, "Hadi")
      if seeprplot == True:
        see_list(self.potential, "Potential")
        see_list(self.normres, "Norm residual")
        see_list(self.residualf, "Residual")
      if seeetet == True:
        see_list(self.Setzetz1, "Sum{et - et-1} from 2 to n")
        see_list(self.Setet, "Sum{et*et} from 1 to n")
        see_list(self.Setetz1, "Sum{et*et-1} from 2 to n")
      ###
      print("+++++ END: Multi Regression +++++ ")
      print()
def anova(y, rm, fm):
  global seereg
  print("===== ANOVA =====")
  seereg = False
  rmlm = lm()
  rmlm.fit(y, rm)
  SSErm = rmlm.SSE
  dfrm = rmlm.n_p_1
  fmlm = lm()
  fmlm.fit(y, fm)
  SSEfm = fmlm.SSE
  dffm = fmlm.n_p_1
  nom = (SSErm - SSEfm) / (dfrm - dffm)
  denom = (SSEfm / dffm)
  F = nom / denom
  print("SSE_rm[df = {}] = {}".format(dfrm, strf(SSErm)))
  print("SSE_fm[df = {}] = {}".format(dffm, strf(SSEfm)))
  print("SSE_rm - SSE_fm [df = {}] = {}".format(dfrm - dffm, strf(SSErm - SSEfm)))
  print("(SSE_rm - SSE_fm) / (df_rm - df_fm)[nom] = {}".format(strf(nom)))
  print("(SSE_fm / df_fm)[denom] = {}".format(strf(denom)))
  print("F = nom / denom = {}".format(strf(F)))
  print("##### ANOVA #####")
  seereg = True

def coor(y, args, time = 5):
  print("===== Start of Cochrane and Orcutt process:======")
  glm = lm()
  glm.fit(y, args)
  autocorr = glm.autocorr
  for i in range(time):
    print("=== Start of {}th of C-O process:".format(i+1))
    glm = lm()
    glm.fit(shift_neg1(y, autocorr), [shift_neg1(x, autocorr) for x in args])
    autocorr = glm.autocorr
    print("=== End of {}th of C-O process:".format(i+1))
    print()

def cal_vif(r):
  return [1/(1-rj) if rj != 1 else 999 for rj in r]

def vif(args):
  global seereg
  seereg = False
  print("===== Start of calculating VIF ====")
  R2list = []
  for i in range(len(args)):
    copy = [x for x in args]
    y = args[i]
    copy.remove(args[i])
    glm = lm()
    glm.fit(y, copy)
    R2 = glm.R2
    R2list.append(R2)
  seereg = True
  vif_list = cal_vif(R2list)
  see_list(R2list, "R2")
  see_list(vif_list, "VIF[threshold = 10]:")
  print("average of VIF:{}".format(strf(mean(vif_list))))
  print("===== End of calculating VIF ====")
def vifxtx(xtx,n):
  return inv(div(xtx, n-1))
def calmallow(SSE, np, sigmahat, p): #p here includes b0
  return SSE / (sigmahat * sigmahat) + 2 * p - np

def mallows(y, rm, fm):
  global seereg
  seereg = False
  print("===== Start of Mallows Cp: ===")
  fmlm = lm()
  fmlm.fit(y, fm)
  rmlm = lm()
  rmlm.fit(y, rm)
  sigmahat = fmlm.sigmahat
  SSEp = rmlm.SSE
  np = rmlm.n
  cp = calmallow(SSEp, np, sigmahat, len(rm))
  print("SSE_p = {}".format(strf(SSEp)))
  print("sigma_hat_q^2 = {}".format(strf(sigmahat * sigmahat)))
  print("Mallows Cp = {}".format(strf(cp)))
  seereg = True
  print("===== End of Mallows Cp: ===")

def testridge():
  global k, ridge
  file_name = "P241.txt"
  y = read_col(file_name, 1)
  x0 = read_col(file_name, 0)
  x1 = read_col(file_name, 2)
  x2 = read_col(file_name, 3)
  x3 = read_col(file_name, 4)
  mat = [y,x0,x1,x2,x3]
  s = myslicelt(mat, x0, 60)
  y = s[0]
  x1 = s[2]
  x2 = s[3]
  x3 = s[4]
  has_intercept = True
  ridge = True
  k = 0.04
  args = [x1,x2,x3]
  glm = lm()
  glm.fit(mynorm(y), mynorm([x1,x2,x3]))
  #fixedp(mynorm(y), mynorm([x1,x2,x3]))
  return y, x1,x2,x3

def calfixedp(p, beta, RMS):
  beta2 = [b*b for b in beta]
  see_list(beta2, "S{beta*beta}")
  fixedk = p * RMS / sum(beta2)
  print("fixed point k = {}(p) * {}(RMS) / {}(sum(b^2)) =  {}".format(p, strf(RMS), strf(sum(beta2)), strf(fixedk)))
  return fixedk

def fixedp(y, x):
  glm = lm()
  glm.fit(y, x)
  p = glm.p
  beta = glm.beta
  MSE = glm.MSE
  fixedk = calfixedp(p, beta, MSE)
  #rlm = lm()
  #rlm.fit(y, x)
def calorig(beta, y, x): #y, x need to be original
  newbetaj = [spstd(y)/spstd(x[i])*beta[i+1] for i in range(len(beta)-1)]
  see_list(newbetaj, "newbetaj")
  newbeta0 = mean(y) - sum([newbetaj[i] * mean(x[i]) for i in range(len(newbetaj))])
  newbeta = [newbeta0]
  [newbeta.append(bj) for bj in newbetaj]
  see_list(newbeta, "if y and x are standardized: new beta:")

def ridge(y, x, k2):
  global ridge, k
  ridge = True
  temp = k
  k = k2
  glm = lm()
  glm.fit(y, x)
  ridge = False
  k = temp
def ridgenorm(y, x, k2):
  global ridge, k
  ridge = True
  temp = k
  k = k2
  glm = lm()
  glm.fit(mynorm(y), mynorm(x))
  beta = glm.beta
  calorig(beta, y, x)
  ridge = False
  k = temp