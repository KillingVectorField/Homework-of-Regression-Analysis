import numpy as np
import update#习题1.10中的更新法

tol=1e-3

data=np.random.random([5,3])
A0=update.mean_and_SXY(data)#返回对A扫描1次的结果（2.7.2中的矩阵），等价于对A扫描了支点0的结果

def Sweep(A,k):
    '''单次扫描，支点为k'''
    #支点k的容差检查
    B=A.copy()
    if abs(A[k,k])<tol/np.sum((A[:,k]-np.mean(A[:,k]))**2):#认为A[k,k]=0,A不可逆
        print('1: k=',k, 'is a colinear variate. Delete k')
        raise ValueError
    #然后检查对应于先前每一次扫过的支点
    for j in range(k):
        if A[j,j]>np.sum((A[:,j]-np.mean(A[:,j]))**2)/tol:
            print('2: k=',k, 'is a colinear variate. Delete k')
            raise ValueError
    #通过以上的容差检查
    B[:,k]/=-A[k,k]#第k列
    for i in list(range(k))+list(range(k+1,len(A))):
        for j in list(range(k))+list(range(k+1,len(A))):#既不在第k行也不在第k列
            B[i,j]+=A[k,j]*B[i,k]
    B[k,]/=A[k,k]#第k行
    B[k,k]=1/A[k,k]
    return B


def Sweep_whole(A,list):
    '''如果A可逆，返回A^{-1}'''
    for i in list:
        try:
            A=Sweep(A,i)
            print('i=',i,'RSS=',A[-1,-1])
        except ValueError:
            print('A is not invertible.')
            break
    return A

def MyLinearRegression(data):
    A0=update.mean_and_SXY(data)
    Result=Sweep_whole(A0,range(1,len(A0)-1))
    print('全扫描结果：',Result)
    beta=-Result[-1,:-1]
    MSE=Result[-1,-1]/(len(data)-len(A0)+1)
    R_squared=1-Result[-1,-1]/A0[-1,-1]
    se_beta=Result.diagonal()[:-1]
    return dict([('beta',beta),('MSE',MSE),('R-squared',R_squared),('se(beta)',se_beta)])




#2.7.4和2.7.5的证明见作业
#2.7.6
print('验证逆：',Sweep_whole(A0,range(len(A0))).dot(A0))#应为单位矩阵

from sklearn import linear_model#验证与标准库算出的结果相同
lm=linear_model.LinearRegression()
lm.fit(data[:,:-1].reshape([-1,data.shape[1]-1]),data[:,-1])
print('标准库结果：',lm.intercept_,lm.coef_,lm._residues)
#print('全扫描结果：',Sweep_whole(A0,range(1,len(A0)-1)))
print('全扫描结果：',MyLinearRegression(data))



