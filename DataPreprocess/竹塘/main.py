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
    t = time.strftime("%Y-%m",t)
    T[i]=str(t)

V=A.iloc[:,1:]
V=V.values.round(3)
V=V.tolist()
#try to trans to np
V=np.array(V)
V=np.transpose(V)
T=np.array(T)
S_depth=np.array(S_depth)


#colorbar=plt.colorbar('YLim',[-1.5,2])
print(np.shape(V),np.shape(np.transpose(V)),V.dtype)
print(np.shape(T),T.dtype)
print(np.shape(S_depth),S_depth.dtype)
print(np.shape([1,2,3]))
#print(T)
fig = plt.figure()
ax = plt.axes(projection='3d')
ax.plot_surface(T,S_depth, V,cmap='viridis', edgecolor='none')
#surf = plt.surf(T,S_depth,V,'edgecolor','none')
#h = plt.colorbar(surf)
plt.ylim(-0.5,2.5)

#h = colorbar
#plt.ylabel(h, 'Displacement(cm)','fontsize',16,'fontname','Times New Roman','LineWidth',1.5)
plt.set(plt.gca,'YDir','reverse')
plt.xticks(T)
plt.xlabel(plt.num2cell(str(time.strftime('yyyy/mm',T))))

plt.draw()

#plt.xticklabels(num2cell(str(time.strftime('yyyy/mm',T))));
#%datetick('x','yyyy/mm')

#S_depth=
#T=A.head
#print(S_depth)
#print(T)
#print(V)
#print(np.array(V).tolist())