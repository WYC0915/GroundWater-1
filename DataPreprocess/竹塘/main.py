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

import numpy as np
import time
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.colors import LightSource
"""from matplotlib import colorbar
from matplotlib import surf
from matplotlib import ylabel
from matplotlib import xticks
from matplotlib import xticklabels
from matplotlib import num2cell"""
#read = pd.read_excel("竹塘(matlab2).xlsx")
#A = np.array(read,dtype=np.float16)
A = pd.read_excel("竹塘(matlab2).xlsx")
#print(A)
S_depth = A.iloc[0:, 0]
S_depth = S_depth.values.round(3).tolist()
T = list(A)
T.pop(0)
for i, t in enumerate(T):
    t=str(t)
    t = time.strptime(t, "%Y%m")
    t = time.strftime("%Y/%m",t)
    #t = (time.mktime(t))
    #T[i]=float(t)

V=A.iloc[:,1:]
V=V.values.round(3)
V=V.tolist()
#try to trans to np
V=np.array(V)
#V=np.transpose(V)
T=np.array(T)
S_depth=np.array(S_depth)


#colorbar=plt.colorbar('YLim',[-1.5,2])
print(np.shape(V),np.shape(np.transpose(V)),V.dtype)
print(np.shape(T),T.dtype)
print(np.shape(S_depth),S_depth.dtype)
print(np.shape([1,2,3]))

#print(T)
#print(S_depth)
#print(V)

fig = plt.figure()
ax = fig.gca(projection='3d')
T,S_depth = np.meshgrid(T,S_depth)
ax.plot_surface(T, S_depth, V)#, edgecolor='none'
ls = LightSource(azdeg=315, altdeg=45)
sha = ls.shade(V,cmap=plt.cm.copper, blend_mode='overlay')
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
plt.xlabel(T)

plt.box(on=True)
#ax.pcolormesh(T, S_depth, V[:-1, :-1], shading='interp', vmin=V.min(), vmax=V.max())
plt.draw()

#plt.xticklabels(num2cell(str(time.strftime('yyyy/mm',T))));
#%datetick('x','yyyy/mm')

#S_depth=
#T=A.head
#print(S_depth)
#print(T)
#print(V)
#print(np.array(V).tolist())