import numpy as np

def update(A,m,new):
    '''更新法'''
    #new_mean=data['mean']+1/(data_number+1)*(new-data['mean'])#更新均值
    #new_SXY=np.zeros((len(new_mean),len(new_mean)))
    #先更新SXY
    for i in range(1,len(A)):
        for j in range(1,i):
            A[i][j]=A[j][i]
        for j in range(i,len(A)):
            A[i][j]+=m/(m+1)*(new[i-1]-A[0,i])*(new[j-1]-A[0,j])
    #data['mean'],data['SXY']=new_mean,new_SXY
    A[0,1:]+=(new-A[0,1:])/(m+1)#更新均值
    A[1:,0]=-A[0,1:].transpose()
    return A


def mean_and_SXY(data):#data是(p+1)*(p+1)（p维的x和1维的y，没有截距项）
    '''返回Sweep A[1](p'+1)*(p'+1)'''
    A=np.zeros([data.shape[1]+1,data.shape[1]+1])
    A[0,0]=1/len(data)#A[0,0]=1/n
    A[0,1:]=data[0]#初始均值
    #data_SXY=np.zeros((len(data[0]),len(data[0])))
    #data_dict=dict([('mean',data_mean),('SXY',data_SXY)])
    for i in range(1,len(data)):#不断更新
        A=update(A,i,data[i])
    return A
