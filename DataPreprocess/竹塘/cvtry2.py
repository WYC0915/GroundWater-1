import numpy as np
import time
import pandas as pd
import cv2
A = pd.read_excel("竹塘(matlab2).xlsx")
S_depth = A.iloc[0:, 0]
S_depth = S_depth.values.round(3).tolist()
T = list(A)
T.pop(0)
#for i, t in enumerate(T):
#    t=str(t)
    #t = time.strptime(t, "%Y%m")
    #t = time.strftime("%Y/%m",t)

V=A.iloc[:,1:]
V=V.values.round(3)
V=V.tolist()
#try to trans to np
V=np.array(V)
#T=np.array(T)
S_depth=np.array(S_depth)


#colorbar=plt.colorbar('YLim',[-1.5,2])


print(np.shape(V),np.shape(np.transpose(V)),V.dtype)
#print(np.shape(T),T.dtype)
print(np.shape(S_depth),S_depth.dtype)
print(np.shape([1,2,3]))
#import cv2 as cv
#kernel = np.ones((5,5),np.float32)/25
#V = cv2.filter2D(V, -1, kernel)
#V=cv2.resize(V, (256, 256),interpolation=cv2.INTER_AREA)
trans = 120
V2 = ((V - V.min()) * (1/(V.max() - V.min()) * trans))
testimg= cv2.imread("test.jpg")
print(V2.max())
print(V2[0][-1])
b = cv2.resize(testimg, (36, 27), interpolation=cv2.INTER_AREA)
b[:,:,0]=V2
b[:,:,1]=240
b[:,:,2]=240
#np.savetxt('sample.csv', V2, delimiter=",")

hsv = cv2.cvtColor(b, cv2.COLOR_HSV2BGR)

t1=0
t2=0
tmp = hsv[:,:,t1]
hsv[:,:,t1]=hsv[:,:,t2]
hsv[:,:,t2]=tmp

G=hsv[:,:,1]
print(hsv[0][-1][:])
hsv[:,:,1]=G
hsv=cv2.resize(hsv, (36*5, 27*5), interpolation=cv2.INTER_AREA)
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
fig, ax = plt.subplots()
x_lims = []
for dat in T:
    # print (dat)
    dat=str(dat)
    x_lims.append(time.strptime(dat, "%Y%m"))

print(x_lims)
#strt = time.strftime("%Y/%m",x_lims[0])
#times = np.arange(np.datetime64('2018-01-01'),np.datetime64('2021-12-01'), np.timedelta64(75, 'm'))
#ax.plot(,times)
#print(x_lims[0],x_lims[-1])extent=[x_lims[0],x_lims[1],min(S_depth), max(S_depth)+1],
plt.imshow(hsv,cmap="jet",  aspect='auto')

ax.xaxis_date()
#date_format = mdates.DateFormatter('%Y/%m/%d')
#ax.xaxis.set_major_formatter(date_format)
fig.autofmt_xdate()
c=plt.colorbar()
plt.show()
#cv2.imshow('hsv', hsv)
#cv2.waitKey(0)
#cv2.destroyAllWindows()




















