clear;clc;
%load data
A=load('Bayer_close_0216.txt');
B=load('Bmw_close_0216.txt');
C=load('Siemens_close_0216.txt');
v=load('VaR_0216_Matlab.txt');

%Size of window
h = 250;
v = -v;
V = A + B + C;
D = length(V);
L = V(2:D)-V(1:(D-1));
T = length(L);

outlier = zeros(T-h,1);
exceedVaR = zeros(D-h-1,1);
yplus = zeros(T-h,1);

%Check for exceedances
for l=1:(T-h)
    if L(h+l,1)<v(l,1)
        exceedVaR(l,1)=1;
    end
end

%Find exceedances
for m=1:(T-h)
    if exceedVaR(m,1)==1
        outlier(m,1)=L(m+h,1);
        yplus(m,1)=min(L((h + 1):(D - 1),1)) - 2;
    else outlier(m,1)=NaN;
        yplus(m,1)=NaN;
    end
end


%Calculate the exceedance ratio
p_hat = sum(exceedVaR)/(T - h);

%Plot the values, VaR estimation and the exceedances
plot(L((h+1):(D-1),1),'b.','MarkerSize',10)
grid on
hold on
plot(v,'r-')
plot(outlier,'m.','MarkerSize',10);
plot(yplus,'g+');
hold off
legend('Profit/Loss','VaR','Exceedences')
title('Block Maxima Model')
xlim([-100,3535])
ylim([-25,25])
set(gca,'XTick',[0 480 960 1440 1920 2400 2880 3260])
set(gca,'XTickLabel',{'2002' '2004' '2006'  '2008' '2010' '2012' '2014' '2016'})

