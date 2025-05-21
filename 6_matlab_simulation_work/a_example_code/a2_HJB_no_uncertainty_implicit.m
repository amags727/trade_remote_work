%% This is alpha fixed point iteration loop to solve the Hamilton Jacobi BellmanPDE
% for the Neoclassical Growth Model
% Written by Benjamin Moll

% functional form assumptions 
   % u(c) = c^(1-sigma) / (1- sigma) 
   % F(k) = A * k^alpha
   % \dot{F} = F(k) - delta*k - c

% HJB
  % rho * v(k) = max u(c) + v'(k)* (F(k) - delta*k - c) 

clear all; clc;

tic;

sigma = 2; %sigma
alpha = 0.3; %alpha
delta = 0.05; %delta 
rho = 0.05; %rho = discount rate
Aprod = 1;

% find the steady using our analytical solution and the build out around it
kss = (alpha*Aprod/(rho+delta))^(1/(1-alpha));
I=10;
kmin = 0.001*kss;
kmax = 2*kss;
k = linspace(kmin,kmax,I)';
dk = (kmax-kmin)/(I-1);

maxit=10000;
crit = 10^(-6);
Delta = 1000;

dVf = zeros(I,1);
dVb = zeros(I,1);
c = zeros(I,1);

%INITIAL GUESS (our initial guess for the valution function is just the
%result of spending everything 
v0 = (Aprod.*k.^alpha).^(1-sigma)/(1-sigma)/rho;
v = v0;

maxit=10;
for n=1:maxit
    V = v;
    % forward difference
    dVf(1:I-1) = (V(2:I)-V(1:I-1))/dk;
    %For last row; can't violate the state constraint so it is necessary
    %that v' = u'(c | k state is constant) 
    dVf(I) = (Aprod.*kmax.^alpha - delta.*kmax)^(-sigma); %state constraint, for stability
    
    % backward difference
    dVb(2:I) = (V(2:I)-V(1:I-1))/dk;
    dVb(1) = (Aprod.*kmin.^alpha - delta.*kmin)^(-sigma); %state constraint, for stability
        
    %consumption determined by c_i = u'^-1(v_i') 
        %consumption and savings with forward difference
        cf = dVf.^(-1/sigma);
        muf = Aprod.*k.^alpha - delta.*k - cf;
        %consumption and savings with backward difference
        cb = dVb.^(-1/sigma);
        mub = Aprod.*k.^alpha - delta.*k - cb;
        %consumption and derivative of value function at steady state
        c0 = Aprod.*k.^alpha - delta.*k;
        dV0 = c0.^(-sigma);
    
    % dV_upwind makes alpha choice of forward or backward differences based on
    % the sign of the drift    
    If = muf > 0; %below steady state
    Ib = mub < 0; %above steady state
    I0 = (1-If-Ib); %at steady state

    dV_Upwind = dVf.*If + dVb.*Ib + dV0.*I0; %important to include third term
    c = dV_Upwind.^(-1/sigma);
    u = c.^(1-sigma)/(1-sigma);
    
    %CONSTRUCT MATRIX
    X = -min(mub,0)/dk;
    Y = -max(muf,0)/dk + min(mub,0)/dk;
    Z = max(muf,0)/dk;
    
    %full matrix: slower
    %     for i=2:I-1
    %         A(i,i-1) = x(i);
    %         A(i,i) = y(i);
    %         A(i,i+1) = z(i);
    %     end
    %     A(1,1)=y(1); A(1,2) = z(1);
    %     A(I,I)=y(I); A(I,I-1) = x(I);
   
    %sparse matrix: faster
    A =spdiags(Y,0,I,I)+spdiags(X(2:I),-1,I,I)+spdiags([0;Z(1:I-1)],1,I,I);
    
    if max(abs(sum(A,2)))>10^(-12)
        disp('Improper Transition Matrix')
        break
    end
    
    B = (rho + 1/Delta)*speye(I) - A;
    
    b = u + V/Delta;
    V = B\b; %SOLVE SYSTEM OF EQUATIONS
    Vchange = V - v;
    v = V;   

    dist(n) = max(abs(Vchange));
    if dist(n)<crit
        disp('Value Function Converged, Iteration = ')
        disp(n)
        break
    end
end
toc;
J= 1;
b = zeros(I*J,1);
AT = A';
i_fix = 1;
b(i_fix)=.1;
row = [zeros(1,i_fix-1),1,zeros(1,I*J-i_fix)];
AT(i_fix,:) = row;

%Solve linear system
gg = AT\b

%% Graphs
set(gca,'FontSize',14)
plot(dist,'LineWidth',2)
grid
xlabel('Iteration')
ylabel('||V^{n+1} - V^n||')

kdot = Aprod.*k.^alpha - delta.*k - c;
Verr = c.^(1-sigma)/(1-sigma) + dV_Upwind.*kdot - rho.*V;

set(gca,'FontSize',14)
plot(k,Verr,'LineWidth',2)
grid
xlabel('k')
ylabel('Error in HJB Equation')
xlim([kmin kmax])

set(gca,'FontSize',12)
plot(k,V,'LineWidth',2)
grid
xlabel('k')
ylabel('V(k)')
xlim([kmin kmax])

set(gca,'FontSize',14)
plot(k,c,'LineWidth',2)
grid
xlabel('k')
ylabel('c(k)')
xlim([kmin kmax])

set(gca,'FontSize',14)
plot(k,kdot,k,zeros(1,I),'--','LineWidth',2)
grid
xlabel('$k$','FontSize',16,'interpreter','latex')
ylabel('$sigma(k)$','FontSize',16,'interpreter','latex')
xlim([kmin kmax])
print -depsc HJB_NGM.epsdr