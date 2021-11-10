import time
import numpy as np
import pandas as pd
from matplotlib.colors import LightSource
from matplotlib import cm
import matplotlib.pyplot as plt #繪圖用的模組

from mpl_toolkits import mplot3d
from mpl_toolkits.mplot3d import Axes3D #繪製3D座標的函式
#from mpl_toolkits.mplot3d import axes3d #繪製3D座標的函式
import matplotlib.ticker as ticker # 處理figure間隔

A = pd.read_excel("竹塘(matlab2).xlsx")

S_depth = A.iloc[0:, 0]
S_depth = S_depth.values.round(3).tolist()
T = list(A)
T.pop(0)
for i, t in enumerate(T):
    t=str(t)
    t = time.strptime(t, "%Y%m")
    t = time.strftime("%Y/%m",t)

V=A.iloc[:,1:]
V=V.values.round(3)
V=V.tolist()
V=np.array(V)
T=np.array(T)
S_depth=np.array(S_depth)

print("")
print(np.shape(V),np.shape(np.transpose(V)),V.dtype)
print(np.shape(T),T.dtype)
print(np.shape(S_depth),S_depth.dtype)
print(np.shape([1,2,3]))
print("")

# 建立 3D 圖形
fig = plt.figure()
ax=Axes3D(fig)
# ax = fig.gca(projection='3d')

# 使figure間隔變多
plt.xticks(np.arange(min(T), max(T)+10, 10.0))
plt.yticks(np.arange(min(S_depth), max(S_depth)+10, 10.0))

#ax.xaxis.set_major_locator(ticker.MultipleLocator(T))
#ax.yaxis.set_major_locator(ticker.MultipleLocator(S_depth))


# 用這兩個arange物件中的可能取值一一對映去擴充為所有可能的取樣點
T,S_depth = np.meshgrid(T,S_depth)

# 繪製 3D 曲面圖形
surf = ax.plot_surface(T, S_depth, V, rstride=1, cstride=1, cmap='jet')# viridis jet
ax.view_init(elev=90,azim=0)

plt.xticks(rotation=320)
plt.yticks(rotation=0)

# 繪製 Colorbar 圖形
fig.colorbar(surf, ax=ax, shrink=0.75, aspect=12)

# label字樣...
ax.set_xlabel('Time (year/month)', fontsize = 14, fontfamily = 'Times New Roman', color = 'black')# sans-serif
ax.set_ylabel('Depth(m)'         , fontsize = 14, fontfamily = 'Times New Roman', color = 'black')
ax.set_zlabel('Displacement(cm)' , fontsize = 16, fontfamily = 'Times New Roman', color = 'black')

cmap = plt.cm.copper
ls = LightSource(315, 45)
rgb = ls.shade(V, cmap)
#ax.imshow(rgb, interpolation='bilinear')
#im = ax.imshow(V, cmap=cmap)
#im.remove()
#fig.colorbar(im)

plt.box(on=True)

# 顯示所有 figure 物件
plt.show()