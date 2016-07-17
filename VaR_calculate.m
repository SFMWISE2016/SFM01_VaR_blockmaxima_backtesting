clear;clc;
%load data
A=load('Bayer_close_0216.txt');
B=load('Bmw_close_0216.txt');
C=load('Siemens_close_0216.txt');
%create portfolio
E=A+B+C;
n=length(E);
X=E(2:n)-E(1:(n-1));
P=-X;
T1=length(P);
h=250;
p=0.95;
n=16;

%define some useful vectors
n1=T1-h;
shape=zeros(n1,1);%shape parameter
scale=zeros(n1,1);%scale parameter
position=zeros(n1,1);%position parameter
pt=zeros(h,1);

%estimate parameters of GEV
for i=1:n1
    pt=P(i:i+249,1);
    c=block(pt,16,'max');%block sample
    parmhat=gevfit(c);
    shape(i,1)=parmhat(1);
    scale(i,1)=parmhat(2);
    position(i,1)=parmhat(3);
    warning off
end

%calculate VaR
var=zeros(n1,1);
for j=1:n1
   var(j,1)=position(j,1)+scale(j,1)/shape(j,1)*((-log(1-p^n))^(-shape(j,1))-1);
end

 fid = fopen('VaR_0216_Matlab.txt','wt');
 fprintf(fid,'%g\n',var);       
 fclose(fid);