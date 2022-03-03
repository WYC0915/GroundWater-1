# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

"""
clear;
clc;
close all;

A = importdata("竹塘(matlab2).xlsx");

S_depth = A.data(2:end,1);
T = A.data(1,2:end);
V = A.data(2:end,2:end);
T = string(T);
T = datenum(T,'yyyymm');

figure();
hold on;

colorbar= colorbar('YLim',[-1.5 2]);
surf(T,S_depth,V,'edgecolor','none');
h = colorbar;
ylabel(h, 'Displacement(cm)','fontsize',16,'fontname','Times New Roman','LineWidth',1.5)
set(gca,'YDir','reverse'); 
xticks(T);
%datetick('x','yyyy/mm');
xticklabels(num2cell(string(datestr(T,'yyyy/mm'))));
xlabel("Time (year/month)",'fontsize',14,'fontname','Times New Roman')
ylabel("Depth(m)",'fontsize',14,'fontname','Times New Roman')
colormap jet;
axis tight;
box on;
shading interp;
ylim([0 300]);
xtickangle(270);
set(gca,'FontName','Times New Roman','FontSize',14,'LineWidth',1.5);
"""
print("branch test")
import numpy as np
import time
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LightSource
import matplotlib.colors as mcolors
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits import mplot3d
from matplotlib import cm

A = pd.read_excel("竹塘(matlab).xlsx")
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
#try to trans to np
V=np.array(V)
T=np.array(T)
S_depth=np.array(S_depth)


#colorbar=plt.colorbar('YLim',[-1.5,2])


print(np.shape(V),np.shape(np.transpose(V)),V.dtype)
print(np.shape(T),T.dtype)
print(np.shape(S_depth),S_depth.dtype)
print(np.shape([1,2,3]))

import matplotlib
import matplotlib.colors as colors
import matplotlib.dates as mdates
import matplotlib.ticker
import matplotlib as mpl
import datetime as dt
x_lims = []
for dat in T:
    dat=str(dat)
    x_lims.append(dt.datetime.strptime(dat, "%Y%m"))

fig, ax = plt.subplots()
x_lims = mdates.date2num(x_lims)

y = np.arange(len(S_depth) )
x = np.arange(len(T) )
print(x)
#ax.pcolormesh( x,y,V, shading='gouraud', vmin=V.min(), vmax=V.max())
plt.gca().xaxis.set_major_formatter(mdates.DateFormatter('%Y/%m')) 

methods = [None, 'none', 'nearest', 'bilinear', 'bicubic', 'spline16',
           'spline36', 'hanning', 'hamming', 'hermite', 'kaiser', 'quadric',
           'catrom', 'gaussian', 'bessel', 'mitchell', 'sinc', 'lanczos']

cmap = 'jet_r'
colors_map = plt.get_cmap(cmap) 
cNorm = colors.Normalize(vmin=0, vmax=5)
print(cNorm)
scalarMap = matplotlib.cm.ScalarMappable(norm=cNorm, cmap=colors_map)

plt.imshow(V, extent=( np.amin(x_lims), np.amax(x_lims), np.amax(S_depth)+1,np.amin(S_depth)),  
           aspect = 'auto',cmap=cmap,interpolation='lanczos',vmax=0.3,vmin=-0.6)


ax.xaxis.set_major_locator(mdates.MonthLocator(interval=1))
fig.autofmt_xdate(rotation=270)
plt.ylabel('Deaph(m)')
plt.xlabel('Time(year/month)')
bar = plt.colorbar()

#bar = plt.colorbar(norm=colors.PowerNorm(gamma=0.5))
#plt.colorbar(V,fraction=0.046, pad=0.04)




"""
colors_map = plt.get_cmap('seismic')     # 选择色条方案

cNorm = colors.Normalize(vmin=0, vmax=5)    # 选择对应色条的最小值和最大值

print(cNorm)         # 保存为一个色彩Normalize的d对象

scalarMap = cmx.ScalarMappable(norm=cNorm, cmap=colors_map)  # 映射色彩对应值到具体的色条

print(scalarMap.get_clim())     # 查看色条对应的数值范围



from scipy import interpolate
#import cv2 as cv
kernel = np.ones((5,5),np.float32)/25
#V = cv.filter2D(V, -1, kernel)
f=interpolate.interp2d(T,S_depth, V, kind='quintic')
V2 = f(T,S_depth)
print("V2:",np.shape(V2),np.shape(np.transpose(V2)))
V=V2
##init
mul=2
fig = plt.figure(figsize=(27*mul,36*mul))
#gca為回傳座標軸，若無則創建
ax = fig.gca(projection='3d')
# 使figure間隔變多
plt.xticks(np.arange(min(T), max(T), 10.0))
plt.yticks(np.arange(min(S_depth), max(S_depth)+1, 50.0))# 以50為間隔，+1會多尾巴300的部分

##camera init
ax.view_init(elev=45,azim=45)
#ax.view_init(elev=90,azim=0)
##create
T,S_depth = np.meshgrid(T,S_depth)
surf = ax.plot_surface(T, S_depth, V ,cmap='jet')
#plt.pcolor(T, S_depth, V,ax=ax, cmap='RdBu', shading='gouraud')
#plt.xticks(np.arange(len(T))[::30],T[::30])
#ax.xticks(np.arange(len(T))[::30],T[::30])

# 使figure間隔變多
#plt.xticks(np.arange(min(T), max(T), 10.0))
#plt.yticks(np.arange(min(S_depth), max(S_depth)+1, 50.0))# 以50為間隔，+1會多尾巴300的部分
# 繪製 Colorbar 圖形

#mesh = ax.pcolormesh(V, cmap = 'jet')
#vmin=-0.6
#vmax=0.3
vmin = np.min(V)
vmax = np.max(V)
#mesh.set_clim(vmin,vmax)
fig.colorbar(surf, ax=ax, shrink=0.5, aspect=24)
#fig.set_clim(vmin,vmax)
#plt.pcolormesh(T, S_depth, V, vmin=-2, vmax=1, cmap="RdBu_r")
#plt.colorbar(ax=None)

#ax.set_aspect('equal')
#ax.imshow(plt,interpolation='hermite' )
plt.show()
#plt.draw()



###bar
#fig.colorbar(surf, ax=ax, shrink=0.5)#, aspect=5

#mycmap = plt.get_cmap('gist_earth')
#ax.set_title('gist_earth color map')
#surf = ax.plot_surface(X, Y, Z, cmap=mycmap)
#plt.box(on=True)
#ls = LightSource(azdeg=315, altdeg=45)
#sha = ls.shade(V,cmap='viridis', blend_mode='overlay')
#sha = ls.shade(V,cmap=plt.cm.copper, blend_mode='overlay')
#surf = plt.surf(T,S_depth,V,'edgecolor','none')
#h = plt.colorbar(surf)
#plt.ylim(-0.5,2.5)

#h = colorbar
#plt.ylabel(h, 'Displacement(cm)','fontsize',16,'fontname','Times New Roman','LineWidth',1.5)
#set(gca,'YDir','reverse'); 
#plt.gca().set(xlim=(0.0, 0.1), ylim=(-0.5,2.5),
 #             xlabel=("Time (year/month)",'fontsize',14,'fontname','Times New Roman'), ylabel='reverse')
#plt.set(plt.gca,'YDir','reverse')
#plt.xticks(T)

#plt.xlabel(plt.num2cell(str(time.strftime('yyyy/mm',T))))
#plt.xlabel(T)

#
#ax.pcolormesh(T, S_depth, V[:-1, :-1], shading='interp', vmin=V.min(), vmax=V.max())
#plt.draw()

#plt.xticklabels(num2cell(str(time.strftime('yyyy/mm',T))));
#%datetick('x','yyyy/mm')

#S_depth=
#T=A.head
#print(S_depth)
#print(T)
#print(V)
#print(np.array(V).tolist())
"""