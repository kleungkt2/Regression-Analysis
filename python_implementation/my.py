from math import *

# ======== system ========== 
def strf(num, precision = 6):
  if num == "None":
    return "None"
  format_str = "{:." + str(precision) + "f}"
  return format_str.format(num)

def cls():
  print("\n" * 10)

def read_col(file_name, col):
  with open(file_name) as f:
    lines = f.readlines()
  li = []
  for line in lines[1:]:
    ln = line.replace('\t',' ')
    ln = ln.replace('\n',' ')
    ln = ln.replace('  ', ' ')
    li.append(float(ln.split(' ')[col]))
  return li

def see_list(li, name):
  print()
  print("==== " + str(name) + " ====")
  if type(li) == list and type(li[0]) != list:
    for i in range(len(li)):
      print("i=" + str(i+1) + "  " + str(name) + " = " + strf(li[i]))
  elif type(li) == list and type(li[0]) == list:
    for line in li:
      print('\t'.join(map(strf, line)))
  print("#### " + str(name) + " ####")
  print()
# ======== linear algebra ========== 
def mean(arr):
  return sum(arr)/len(arr)

def dot(v1, v2):
  return sum([x*y for x,y in zip(v1, v2)])

def mul(a, b):
  if type(a) == list and type(a[0]) != list and type(b) != list: #alist & bconst
    return [ai * b for ai in a]
  elif type(b) == list and type(b[0]) != list and type(a) != list: #aconst & blist
    return [a * bi for bi in b]
  elif type(b) == list and type(a) == list:
    if type(b[0]) == list and type(a[0]) == list: #aMatrix & bMatrix
      zip_b = list(zip(*b))
      return [[sum(ele_a*ele_b for ele_a, ele_b in zip(row_a, col_b)) 
             for col_b in zip_b] for row_a in a]
    elif type(b[0]) == list and type(a[0]) != list: #mvmult, b = M, a = v
      return [dot(r,a) for r in b]
    elif type(a[0]) == list and type(b[0]) != list: #mvmult, a = M, b = v
      return [dot(r,b) for r in a]
    elif type(a[0]) != list and type(b[0]) != list: #vvmult
      return [a[i] * b[i] for i in range(len(a))]
  elif type(a) == list and type(a[0]) == list and type(b) != list: #mcmult, a = M, b = const
    return [[a[i][j] * b for j in range(len(a[0]))] for i in range(len(a))]
  elif type(b) == list and type(b[0]) == list and type(a) != list: #mcmult, b = M, a = const
    return [[b[i][j] * a for j in range(len(b[0]))] for i in range(len(b))]

def tr(m):
  if type(m) == list and type(m[0]) == list:
    return list(map(list,zip(*m)))
  elif type(m) == list and type(m[0]) != list:
    return [[m[i]] for i in range(len(m))]

def getMatrixMinor(m,i,j):
    return [row[:j] + row[j+1:] for row in (m[:i]+m[i+1:])]

def det(m):
    #base case for 2x2 matrix
    if len(m) == 2:
        return m[0][0]*m[1][1]-m[0][1]*m[1][0]

    determinant = 0
    for c in range(len(m)):
        determinant += ((-1)**c)*m[0][c]*det(getMatrixMinor(m,0,c))
    return determinant

def inv(m):
    determinant = det(m)
    
    #special case for 2x2 matrix:
    if len(m) == 2:
        return [[m[1][1]/determinant, -1*m[0][1]/determinant],
                [-1*m[1][0]/determinant, m[0][0]/determinant]]

    #find matrix of cofactors
    cofactors = []
    for r in range(len(m)):
        cofactorRow = []
        for c in range(len(m)):
            minor = getMatrixMinor(m,r,c)
            cofactorRow.append(((-1)**(r+c)) * det(minor))
        cofactors.append(cofactorRow)
    cofactors = tr(cofactors)
    for r in range(len(cofactors)):
        for c in range(len(cofactors)):
            cofactors[r][c] = cofactors[r][c]/determinant
    return cofactors

def minus(a, b): # v-v, v-c, m-c, c-m, c-v
  if type(a) == list and type(b) == list: #alist - blist
    return [a[i] - b[i] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list and type(b) != list: #alist - bconst
    return [a[i] - b for i in range(len(a))]
  elif type(a) == list and type(a[0]) == list and type(b) != list: #amatrix - bconst
    return [[a[i][j] - b for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) != list and type(b) == list and type(b[0]) == list:
    return [[a - b[i][j] for j in range(len(b[0]))] for i in range(len(b))]
  elif type(a) != list and type(b) == list and type(b[0]) != list:
    return [a - b[i] for i in range(len(b))]
    
def add(a, b): # v+v, v+c, c+v, m+c, c+m, m+m, v+m, m+v, c+c
  if type(a) == list and type(b) == list and type(a[0]) == list and type(b[0]) == list: #aMatrix + bMatrix(same shape)
      return [[a[i][j] + b[i][j] for j in range(len(a[0])) ] for i in range(len(a))]
  elif type(a) == list and type(b) == list and type(a[0]) != list and type(b[0]) != list: #alist + blist
    return [a[i] + b[i] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list and type(b) != list: #alist + bconst
    return [a[i] + b for i in range(len(a))]
  elif type(b) == list and type(b[0]) != list and type(a) != list: #blist + aconst
    return [b[i] + a for i in range(len(b))]
  elif type(a) == list and type(a[0]) == list and type(b) != list: #aMatrix + bconst
    return [[a[i][j] + b for j in range(len(a[0]))] for i in range(len(a))]
  elif type(b) == list and type(b[0]) == list and type(a) != list: #bMatrix + aconst
    return [[b[i][j] + a for j in range(len(b[0]))] for i in range(len(b))]  
  elif type(a) == list and type(a[0]) == list and type(b) == list and type(b[0]) != list: #aMatrix + blist
    return [[a[i][j] + b[j] for j in range(len(a[0]))] for i in range(len(a))]
  elif type(b) == list and type(b[0]) == list and type(a) == list and type(a[0]) != list: #bMatrix + alist
    return [[b[i][j] + a[j] for j in range(len(b[0]))] for i in range(len(b))]
  elif type(a) != list and type(b) != list:
    return a + b

def div(a, b):
  if type(a) == list and type(b) == list: #alist / blist
    return [a[i] / b[i] if b[i] != 0 else 9e99 for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list and type(b) != list: #alist / bconst
    return [a[i] / b if b != 0 else 9e99 for i in range(len(a))]
  elif type(a) == list and type(a[0]) == list and type(b) != list: #amatrix / bconst
    return [[a[i][j] / b if b != 0 else 9e99 for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) != list and type(b) == list and type(b[0]) == list:
    return [[a / b[i][j] if b[i][j] != 0 else 9e99 for j in range(len(b[0]))] for i in range(len(b))]
  elif type(a) != list and type(b) == list and type(b[0]) != list:
    return [a / b[i] if b[i] != 0 else 9e99 for i in range(len(b))]

def Imat(p):
  return [[1 if i == j else 0 for i in range(p)] for j in range(p)]

def eig22(A):
  a = A[0][0]
  b = A[0][1]
  c = A[1][0]
  d = A[1][1]
  c1 = 1
  c2 = -(a+d)
  c3 = a*d - b*c
  print("eigenequation: ({})x^2 + ({})x + ({}) = 0".format(c1,c2,c3))
  small_eig = (a + d - sqrt((a+d) * (a+d) - 4 * (a*d - b*c))) / 2
  outside = (a + d) / 2
  inside = (a+d) * (a+d) - 4 * (a*d - b*c)
  big_eig = (a + d + sqrt((a+d) * (a+d) - 4 * (a*d - b*c))) / 2
  print("Eigenvalues: {} - 0.5sqrt({}) = {}, {} + 0.5sqrt({}) = {}".format(outside, inside, strf(small_eig), outside, inside, strf(big_eig)))
  k = sqrt(abs(big_eig / small_eig))
  print("condition num[thr=15] = sqrt(lam_max / lam_min) = sqrt({}/{}) = {}".format(strf(big_eig), strf(small_eig), strf(k)))

def eig33(A):
  a = A[0][0]
  b = A[0][1]
  c = A[0][2]
  d = A[1][0]
  e = A[1][1]
  f = A[1][2]
  g = A[2][0]
  h = A[2][1]
  i = A[2][2]
  c1 = -1
  c2 = a + e + i
  c3 = c*g+f*h+b*d-a*e-a*i-e*i
  c4 = a*e*i+b*f*g+c*d*h-c*g*e-f*h*a-b*d*i
  a = c1
  b = c2
  c = c3
  d = c4
  print("eigenequation: ({})x^3 + ({})x^2 + ({})x + ({}) = 0".format(a,b,c,d))
  alpha = -pow(b,3)/(27*pow(a,3)) - d/(2*a) + b*c/(6*a*a)
  beta = c/(3*a) - b*b/(9*a*a)
  x1 = -b/(3*a) + 2*sqrt(-beta)*cos(acos(alpha/pow(-beta, 3/2))/3)
  x2 = -b/(3*a) + 2*sqrt(-beta)*cos((acos(alpha/pow(-beta, 3/2))+2*pi)/3)
  x3 = -b/(3*a) + 2*sqrt(-beta)*cos((acos(alpha/pow(-beta, 3/2))-2*pi)/3)
  print("eigenvalues = {}, {}, {}".format(strf(x1), strf(x2), strf(x3)))
  lam_max = max(max(x1,x2),x3)
  lam_min = min(min(x1,x2),x3)
  k = sqrt(lam_max / lam_min)
  print("condition num = sqrt(lam_max / lam_min) = sqrt({}/{}) = {}".format(strf(lam_max), strf(lam_min), strf(k)))

def form01(a,b,c):
  outside = -b / (2*a)
  inside = b*b - 4*a*c
  r1 = outside - 0.5 * sqrt(inside)
  r2 = outside + 0.5 * sqrt(inside)
  print("roots of ({})x^2 + ({})x + {{})c = {} +- 0.5*sqrt({}) = {} , {}".format(a,b,c,strf(outside),strf(inside), strf(r1),strf(r2)))

def sign(x):
  if x >= 0:
    return 1
  else:
    return -1
# ======== transformation ==========
def mynorm(args): #args = cols of x
  if type(args) == list and type(args[0]) == list:
    return [norm(arg) for arg in args] 
  elif type(args) == list and type(args[0]) != list:
    return norm(args)
def mylog(a, k = 0):
  if type(a) == list and type(a[0]) == list:
    return [[log(a[i][j] + k) if a[i][j] + k > 0 else 0 for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list: #vector
    return [log(ai + k) if ai + k > 0 else 0 for ai in a]
  else:
    return log(a + k)

def mysqrt(a):
  if type(a) == list and type(a[0]) == list:
    return [[sqrt(a[i][j]) if a[i][j] >= 0 else -1 for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list:
    return [sqrt(ai) if ai >= 0 else -1 for ai in a]
  else:
    return sqrt(a)

def myinv(a):
  if type(a) == list and type(a[0]) == list:
    return [[1/(a[i][j]) if a[i][j] != 0 else 0 for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list:
    return [1/ai if ai != 0 else 0 for ai in a]
  else:
    return 1/a if a != 0 else 0

def mypow(a, power = 2):
  if type(a) == list and type(a[0]) == list:
    return [[pow(a[i][j], power) if a[i][j] != 0 else 0 for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list:
    return [pow(ai, power) if ai != 0 else 0 for ai in a]
  else:
    return pow(a, power)

def myasin(a):
  if type(a) == list and type(a[0]) == list:
    return [[asin(a[i][j]) for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list:
    return [asin(ai) for ai in a]
  else:
    return asin(a)

def myacos(a):
  if type(a) == list and type(a[0]) == list:
    return [[acos(a[i][j]) for j in range(len(a[0]))] for i in range(len(a))]
  elif type(a) == list and type(a[0]) != list:
    return [acos(ai) for ai in a]
  else:
    return acos(a)

def myportion(a):
  return [ai/sum(a) if sum(a) != 0 else 0 for ai in a]

# ======== slicing ========== 
def mysliceeq(mat, arr, val):
  index = [i for i in range(len(arr)) if arr[i] == val]
  return [[col[i] for i in index] for col in mat]

def myslicelt(mat, arr, val):
  index = [i for i in range(len(arr)) if arr[i] < val]
  return [[col[i] for i in index] for col in mat]

def myslicelte(mat, arr, val):
  index = [i for i in range(len(arr)) if arr[i] <= val]
  return [[col[i] for i in index] for col in mat]

def myslicegt(mat, arr, val):
  index = [i for i in range(len(arr)) if arr[i] > val]
  return [[col[i] for i in index] for col in mat]

def myslicegte(mat, arr, val):
  index = [i for i in range(len(arr)) if arr[i] >= val]
  return [[col[i] for i in index] for col in mat]

def factor(x, base = -1):
  d = list({i for i in x})
  if base == -1:
    base = d[len(d) - 1]
  d = [d[i] for i in range(len(d)) if d[i] != base]
  return [[1 if xi == d[i] else 0 for xi in x] for i in range(len(d))]

def shift_neg1(x, p = 1):
  return [x[i] - p*x[i-1]  for i in range(1,len(x))]

def shift_pos1(x, p = 1):
  return [x[i] - p*x[i+1] for i in range(len(x) - 1)]
# ======== statistics ===========
def Sxy(x,y):
  mx = mean(x)
  my = mean(y)
  return sum(mul(x,y)) - len(y) * mx * my

def corr(x,y):
  sxy = Sxy(x,y)
  sxx = Sxy(x,x)
  syy = Sxy(y,y)
  if sxx * syy == 0:
    return 0
  else:
    return sxy / sqrt(sxx * syy)

def corrM(X, Y):
  return [[corr(X[i], Y[j]) for j in range(len(Y))] for i in range(len(X))]

def exprun(n1, n2):
  mu = 2*n1*n2 / (n1 + n2) + 1
  sigma = sqrt(2*n1*n2*(2*n1*n2 - n1 - n2) / ((n1 + n2) * (n1 + n2) * (n1 + n2 - 1)))
  return mu, sigma

def spstd(x):
  return sqrt(Sxy(x,x)/(len(x) - 1))

def popstd(x):
  return sqrt(Sxy(x,x)/len(x))

def norm(x):
  return [(xi - mean(x)) / spstd(x) for xi in x]

def popnorm(x):
  return [(xi - mean(x)) / popstd(x) for xi in x]

def C(X):
  return inv(mul(tr(X),X))

def grid_search(x, y,lamb = 2):
  Xs = [mypow(x, pow) if pow != 0.0 else mylog(x) for pow in [(i-4)/2 for i in range(9)]]
  Ys = [mypow(y, pow) if pow != 0.0 else mylog(y) for pow in [(i-4)/2 for i in range(9)]]
  corrMatrix = corrM(Xs,Ys)
  see_list(corrMatrix, "corr Matrix for grid search")
def loglikep(y,p):
  li = 0
  for i in range(len(y)):
    yp = y[i] * p + (1 - y[i]) * p
    print("i={},y[i]p+(1-y[i])p = {}, loged = {}".format(i, strf(yp), strf(log(yp))))
    li += log(yp)
  print("sum[log(y[i]p+(1-y[i])p) = {}".format(strf(li)))

def loglike(y, beta, args, seedots = False, seelogl = False):
  logl = []
  dots = []
  m = sum([1 for yi in y if yi == 1.0])
  n = len(y)
  p = len(args)
  m2 = n - m
  baseline = max(m/n, m2/n)
  phats = []
  yhats = []
  correct = 0
  for i in range(len(y)):
    vec = [1]
    for arg in args:
      vec.append(arg[i])
    dotp = dot(beta, vec)
    phat = 1 / (1 + exp(-dotp))
    phats.append(phat)
    yhat = 1 if phat >= 0.5 else 0
    if yhat == y[i]: correct += 1 
    yhats.append(yhat)
    dots.append(dotp)
    loglikel = y[i] * dotp - log(1 + exp(dotp))
    logl.append(loglikel)
  
  if seedots:
    see_list(dots, "dot(beta, [1,X])")
  if seelogl:
    see_list(logl, "loglikelihood")
  logsum = sum(logl)
  resD = -2 * logsum
  
  nullPlog = 0
  b0 = log((m/n) / (1 - m/n))
  for i in range(len(y)):
    p1 = 1 / (1 + exp(-b0))
    p2 = y[i] * p1 + (1 - y[i]) * (1 - p1)
    nullPlog += log(p2)
  nullD = -2 * nullPlog
  R2 = 1 - resD / nullD
  G = nullD - resD
  AIC = resD + 2*(p+1)
  BIC = resD + (p+1)*log(n)
  print("number of data: {}, pos data: {}, neg data: {}, baseline: {}".format(n, m, m2, baseline))
  print("correct classify: {}, accuracy: {}".format(correct, strf(correct/n)))
  print("concordance index = #correct/n1n2 = {}".format(strf(correct/(m*m2))))
  print("sum of loglikelihood = {}".format(strf(logsum)))
  print("Residual Deviance[D] = -2*l(beta) = {}".format(strf(resD)))
  print("Null Deviance[D0] = -2*l(b0) = {}".format(strf(nullD)))
  print("R^2 = 1 - D/D0 = {}".format(strf(R2)))
  print("G(~chi^2) = D0 - D = {}".format(strf(G)))
  print("AIC = -2*D + 2p = {} + 2*{} = {}".format(strf(resD), p+1, strf(AIC)))
  print("BIC = -2*D + plogn = {} + {} = {}".format(strf(resD), strf((p+1)*log(n)), strf(BIC)))
