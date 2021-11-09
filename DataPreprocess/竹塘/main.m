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


