from my import *
def Pfn(X):
  return mul(mul(X, inv(mul(tr(X), X))), tr(X))
def piifn(P):
  return [P[i][i] for i in range(len(P))]
def ridgexy1fn(X, y, n):
  return div(mul(tr(X), y), n-1)
def ridgebetafn(C, xy_n_1):
  return mul(C, xy_n_1)
def piithfn(pii):
  return 2*sum(pii)/len(pii)

def betafn(X, y):
  return mul(mul(C(X), tr(X)), y)
def yhatsfn(X, beta):
  return mul(X, beta)
def resfn(y, yhats):
  return minus(y, yhats)
def Setzetz1fn(res):
  return [(res[i] - res[i-1]) * (res[i] - res[i-1]) for i in range(1,len(res))]
def dwnomfn(Setzetz1):
  return sum(Setzetz1)
def Setetfn(res):
  return [res[i] * res[i] for i in range(len(res))]
def dwdenomfn(Setet):
  return sum(Setet)
def dwfn(dwnom, dwdenom):
  return dwnom / dwdenom
def Setetz1fn(res):
  return [(res[i] * res[i-1]) for i in range(1, len(res))]
def autocorrnomfn(Setetz1):
  return sum(Setetz1)
def autocorrdenomfn(res):
  return dwdenomfn(res)
def autocorrfn(autocorrnom, autocorrdenom):
  return autocorrnom/autocorrdenom
def mulrrfn(res):
  return [r * r for r in res]
def SSEfn(mulrr):
  return sum(mulrr)
def MSEfn(SSE, n, p):
  return SSE / (n-p-1)
def SSTfn(y):
  return Sxy(y,y)
def MSTfn(SST, n):
  return SST / (n-1)
def SSRfn(SST, SSE):
  return SST - SSE
def MSRfn(SSR, p):
  return SSR / p
def Ffn(MSR, MSE):
  return MSR / MSE
def R2fn(SSR, SST):
  return SSR / SST
def R2adjfn(SSE, SST, n, p):
  return 1 - SSE/(n-p-1) / (SST/(n-1))
def sigmahatfn(SSE, n, p):
  return sqrt(SSE/(n-p-1))
def sebetafn(C, sigmahat, beta):
  return [sqrt(C[i][i]) * sigmahat for i in range(len(beta))]
def tstatsfn(beta, sebeta):
  return [beta[i] / sebeta[i] for i in range(len(beta))]
def intRfn(res, sigmahat, pii):
  return [res[i] / (sigmahat * sqrt(1 - pii[i])) for i in range(len(pii))]
def runfn(intR):
  return sum([1 for i in range(len(intR) - 1) if intR[i] * intR[i+1] < 0 ]) + 1
def posrunfn(intR):
  return sum([1 for i in intR if i > 0])
def negrunfn(intR):
  return sum([1 for i in intR if i < 0])
def zrunfn(run, murun, stdrun):
  return (run - murun) / stdrun
def extRfn(intR, n, p):
  return [intR[i] * sqrt((n-p-2)/ (n-p-1-intR[i]*intR[i])) for i in range(len(intR))]
def extRtointR(extR, n, p):
  return [sign(extR[i]) * sqrt( (n-p-1) /((n-p-2)/(extR[i] * extR[i])+1) ) for i in range(len(extR))]
def xandyfn(X,y):
  xandy = tr(X)
  xandy.append(y)
  return xandy
def cookfn(intR, p, pii):
  return [intR[i] * intR[i] /(p+1) * pii[i] / (1 - pii[i]) for i in range(len(pii))]
def potentialfn(pii):
  return [pii[i] / (1 - pii[i]) for i in range(len(pii))]
def dfitsfn(extR, pii):
  return [extR[i] * sqrt(pii[i] / (1 - pii[i])) for i in range(len(pii))]
def normresfn(SSE, res):
  return [i/sqrt(SSE) for i in res]
def residualfn(p, pii, normres):
  return [(p + 1) / (1 - pii[i])*normres[i] * normres[i] / (1 - normres[i] * normres[i]) for i in range(len(pii))]
def hadifn(pii, residualf):
  return [pii[i] / (1 - pii[i]) + residualf[i] for i in range(len(pii))]
def AICfn(n, SSE, p):
  return n*log(SSE / n) + 2 * (p+1)
def BICfn(n, SSE, p):
  return n*log(SSE / n) + (p+1) * log(n)



