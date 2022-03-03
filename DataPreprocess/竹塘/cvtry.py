import numpy as np

import pandas as pd
import cv2
A = pd.read_excel("竹塘(matlab2).xlsx")
S_depth = A.iloc[0:, 0]
S_depth = S_depth.values.round(3).tolist()
T = list(A)
T.pop(0)
V=A.iloc[:,1:]
V=V.values.round(3)
V=V.tolist()
V=np.array(V)
S_depth=np.array(S_depth)
print(np.shape(V),np.shape(np.transpose(V)),V.dtype)
print(np.shape(S_depth),S_depth.dtype)
print(np.shape([1,2,3]))
print(V.min(),V.max())
trans = 120
#V2 = ((V - V.min()) * (1/(V.max() - V.min()) * trans))
V2 = ((V + 0.2) * (1/(0.5 + 0.2) * trans))
testimg= cv2.imread("test.jpg")
print(V2.max())
print(V2[0][-1])
V2=np.array(V2,int)
l1=[]
for l in V2:
    l1.extend(l.tolist())
print(set(l1))
b = cv2.resize(testimg, (36, 27), interpolation=cv2.INTER_AREA)
b[:,:,0]=V2
b[:,:,1]=240
b[:,:,2]=240
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

#hsv = cv2.blur(hsv,(2,2))

#hsv = cv2.bilateralFilter(hsv,9,75,75)
kernel = np.array([
  [0, -1, 0],
  [-1, 5, -1],
  [0, -1, 0]
])
#hsv = cv2.filter2D(hsv,cv2.CV_8U,kernel)


import matplotlib.pyplot as plt
import matplotlib
import matplotlib.dates as mdates
import matplotlib.ticker
import datetime as dt
x_lims = []
for dat in T:
    dat=str(dat)
    x_lims.append(dt.datetime.strptime(dat, "%Y%m"))

fig, ax = plt.subplots()
x_lims = mdates.date2num(x_lims)
plt.imshow(V, extent=( np.amin(x_lims), np.amax(x_lims), np.amax(S_depth)+1,np.amin(S_depth)),  aspect = 'auto',cmap='jet')
ax.pcolormesh( T,S_depth, V, shading='interp', vmin=V.min(), vmax=V.max())
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y/%m')) 
ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))
fig.autofmt_xdate(rotation=90)
plt.ylabel('Deaph(m)')
plt.xlabel('Time(year/month)')
bar = plt.colorbar()
#plt.colorbar(V,fraction=0.046, pad=0.04)



plt.show()

