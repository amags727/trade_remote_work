clear all; clc; close all;

tic;

s = 2;
r = 0.045;
rho = 0.05;
y = .1;
kappa = 0.25;
p0 = 0.2; %buying price
p1 = 0.1; %selling price


I=500;
amin = -0.02;
amax = 3;
a = linspace(amin,amax,I)';
da = (amax-amin)/(I-1);

maxit=10000;
crit = 10^(-6);

Delta = 1000;

dVf = zeros(I,2);
dVb = zeros(I,2);
c = zeros(I,2);
Vstar = zeros(I,2);

aa = [a,a];
yy = y*ones(I,2);

%INITIAL GUESS
v0 = (yy + r.*aa).^(1-s)/(1-s)/rho;
v = v0;


for n=1:maxit
    V = v;
    % forward difference
    dVf(1:I-1,:) = (V(2:I,:)-V(1:I-1,:))/da;
    dVf(I,:) = (y + r.*amax).^(-s); %will never be used, but impose state constraint a<=amax just in case
    % backward difference
    dVb(2:I,:) = (V(2:I,:)-V(1:I-1,:))/da;
    dVb(1,:) = (y + r.*amin).^(-s); %state constraint boundary condition
    
    %consumption and savings with forward difference
    cf = dVf.^(-1/s);
    ssf = yy + r.*aa - cf;
    %consumption and savings with backward difference
    cb = dVb.^(-1/s);
    ssb = yy + r.*aa - cb;
    %consumption and derivative of value function at steady state
    c0 = yy + r.*aa;
    
    %Upwind method makes a choice of forward or backward differences based on the sign of the drift    
    If = ssf > 0; %positive drift --> forward difference
    Ib = ssb < 0; %negative drift --> backward difference
    I0 = (1-If-Ib); %at steady state
       
    c = cf.*If + cb.*Ib + c0.*I0;
    util(:,1) = c(:,1).^(1-s)/(1-s); %utility without car
    util(:,2) = c(:,2).^(1-s)/(1-s) + kappa; %utility with car

    %CONSTRUCT MATRIX
    X = -min(ssb,0)/da;
    Y = -max(ssf,0)/da + min(ssb,0)/da;
    Z = max(ssf,0)/da;
   
    A1=spdiags(Y(:,1),0,I,I)+spdiags(X(2:I,1),-1,I,I)+spdiags([0;Z(1:I-1,1)],1,I,I);
    A2=spdiags(Y(:,2),0,I,I)+spdiags(X(2:I,2),-1,I,I)+spdiags([0;Z(1:I-1,2)],1,I,I);
    A = [A1,sparse(I,I);sparse(I,I),A2];
        
    %Check transition matrix conditions 
    if max(abs(sum(A,2)))>10^(-12)
        disp('Improper Transition Matrix')
        break
    end
    
    B = (rho + 1/Delta)*speye(2*I) - A;
    
    u_stacked = [util(:,1);util(:,2)];
    V_stacked = [V(:,1);V(:,2)];

    %Outside Option
    i_buy = ceil(p0/da); %p0 equals i_buy grid points
    %Value of buying car if currently, don't own car d=0
    Vstar(i_buy+1:I,1) = V(1:I - i_buy,2);
    %Instead of setting Vstar(1:i_buy)=-Inf, do something smoother
    slope = (Vstar(i_buy+2,1)-Vstar(i_buy+1,1))/da;
    Vstar(1:i_buy,1) = Vstar(i_buy+1,1) + slope*(a(1:i_buy) - a(i_buy+1));

    %Value of selling car if currently own car, d=1
    i_sell = ceil(p1/da); %p1 equals i_sell grid points
    Vstar(1:I-i_sell,2) = V(i_sell+1:I,1);
    Vstar(I-i_sell+1:I,2) = V(I,1); %assume p = min(p,amax - a), i.e. selling car cannot take you above amax
    Vstar_stacked = [Vstar(:,1);Vstar(:,2)];
    
    vec = u_stacked + V_stacked/Delta;
    q = -vec + B*Vstar_stacked; 
    
    %using Yuval Tassa's Newton-based LCP solver, download from http://www.mathworks.com/matlabcentral/fileexchange/20952
    z0 = V_stacked-Vstar_stacked; l = zeros(2*I,1); u = Inf*ones(2*I,1);
    z = LCP(B,q,l,u,z0,0);
   
    LCP_error = max(abs(z.*(B*z + q)));
    if LCP_error > 10^(-5)
        disp('LCP not solved, Iteration =')
        disp(n)
        break
    end
    
    V_stacked = z+Vstar_stacked; %calculate value function
    V = [V_stacked(1:I),V_stacked(I+1:2*I)];
    
    Vchange = V - v;
    v = V;
    dist(n) = max(max(abs(Vchange)));
    if dist(n)<crit
        disp('Value Function Converged, Iteration = ')
        disp(n)
        break
    end
end
toc;


plot(a,v(:,1),'b',a,Vstar(:,1),'g--',a,v(:,2),'r',a,Vstar(:,2),'k--','LineWidth',2)
set(gca,'FontSize',16)
legend('v_0(a), having no car','v_0^*(a), buying car','v_1(a), having car','v_1^*(a),selling car','Location','SouthEast')
set(findobj('color','g'),'Color',[0 0.5 0]);
print -depsc car_v.eps

action = (V == Vstar); %=1 if buy or sell the car

plot(a,action(:,1),'b',a,action(:,2),'r--','LineWidth',2)
set(gca,'FontSize',16)
legend('Indicator for buying car','Indicator for selling car')
ylim([-0.05 1.2])
print -depsc car_act.eps

c_plot = c;
c_plot(action == 1)=NaN;

plot(a,c_plot(:,1),'b',a,c_plot(:,2),'r--','LineWidth',2)
set(gca,'FontSize',16)
legend('c_0(a)','c_1(a)','Location','SouthEast')
print -depsc car_c.eps

sav = yy + r.*aa - c;
sav(action == 1)=NaN;

plot(a,sav(:,1),'b',a,sav(:,2),'r--','LineWidth',2)
set(gca,'FontSize',16)
legend('s_0(a)','s_1(a)','Location','SouthWest')
print -depsc car_s.eps

util(action == 1)=NaN;
plot(a,util(:,1),'b',a,util(:,2),'r--','LineWidth',2)
set(gca,'FontSize',16)
legend('u(c_0(a))','u(c_1(a))+\kappa','Location','SouthEast')
print -depsc car_u.eps