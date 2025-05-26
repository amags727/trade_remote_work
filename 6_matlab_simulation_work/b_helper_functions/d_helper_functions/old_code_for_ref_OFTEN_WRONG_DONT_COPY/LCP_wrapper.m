function [V_stacked,inaction_region, best_alternative] = LCP_wrapper(env, home_country, phi_idx)

% unpack the environment 
structures = {'env'};
for idx = 1:length(structures)
    fields = fieldnames(eval(structures{idx})); % Get the field names of the structure
    for idx2 = 1:length(fields)
        fieldName = fields{idx2};
        eval([fieldName ' = ' structures{idx} '.' fieldName ';']);
    end
end

% define other necessary inputs 
profit_stacked = zeros(stack_length,1);
V_stacked = zeros(stack_length,1);
Vstar_stacked = zeros(stack_length,1);
B = sparse(stack_length,stack_length);
best_alternative = Vstar_stacked;

for network = 1:num_networks
    % Construct the inputs for the linear complementarity problem
    current_indeces = (1:num_states) + num_states*(network-1);
    B(current_indeces, current_indeces) = (rho + env.beta + dt)*speye(num_states) - A_matrices{phi_idx,home_country,network};
    profit_stacked(current_indeces) = profits_inner(:, phi_idx, home_country,  network);
    V_stacked(current_indeces) = V(:, phi_idx, home_country, network);

    % generate V_star_stacked = best choice after accounting for adjustment
    % costs
    temp_V = squeeze(V(:, phi_idx, home_country,:));
    temp_V(:,network) = -inf;
    for alt_network = 1:num_networks
        if alt_network ~= network
         adjustment_costs = sum(max(network_list{alt_network} - network_list{network}, 0))*c_entry +...
             sum(max(network_list{network}- network_list{alt_network}, 0))*.001;
         temp_V(:,alt_network)= temp_V(:,alt_network)-repmat(adjustment_costs, num_states,1);
        end
    end
    [Vstar_stacked(current_indeces), best_alternative(current_indeces)] = max(temp_V, [],2);
end

% solve the linear complementarity problem for each country
vec = profit_stacked + V_stacked*dt;
q = -vec + B*Vstar_stacked;
z0 = V_stacked - Vstar_stacked;
l = zeros(stack_length,1); u = Inf*ones(stack_length,1);
%z0 = LCP(B,q,l,u,z0,0); % have the outside LCP solver get us close 
%z = LCP_solver(B,q,z0); % use fmincon to get us the rest of the way 
z = LCP(B,q,l,u,z0,0);

LCP_error = max(abs(z.*(B*z + q)));
    if LCP_error > 10^(-5)
       % disp('LCP not solved')
    end
    
V_stacked = z+Vstar_stacked;
inaction_region = z > 0; % region for which exists additional value above outside options 
c_V = squeeze(V(:,phi_idx,home_country,:));

%there's some slight fuzziness in our z results due to the second LCP solver --> to solve if z VERY small and 
% alternative gives higher utility (w/o accounting for adjustment costs, i say you switch 
for index = 1:stack_length
[state_num, network] = ind2sub([num_states,num_networks],index);
    if z(index) < 1e-15 && z(index) > 0 && c_V(state_num,best_alternative(index)) > c_V(state_num, network)
    inaction_region(index) = 0;
    end
end
end

%% HELPER FUNCTIONS LCP and FB were directly imported, LCP_SOLVER wrriten by me 
function x = LCP(M,q,l,u,x0,display)
%LCP Solve the Linear Complementarity Problem.
%
% USAGE
%   x = LCP(M,q) solves the LCP
%
%           x >= 0
%      Mx + q >= 0 
%   x'(Mx + q) = 0  
%
%   x = LCP(M,q,l,u) solves the generalized LCP (a.k.a MCP)
%
%   l < x < u   =>   Mx + q = 0
%       x = u   =>   Mx + q < 0
%   l = x       =>   Mx + q > 0
%
%   x = LCP(M,q,l,u,x0,display) allows the optional initial value 'x0' and
%   a binary flag 'display' which controls the display of iteration data.
%
%   Parameters:
%   tol       -   Termination criterion. Exit when 0.5*phi(x)'*phi(x) < tol.
%   mu        -   Initial value of Levenberg-Marquardt mu coefficient.
%   mu_step   -   Coefficient by which mu is multiplied / divided.
%   mu_min    -   Value below which mu is set to zero (pure Gauss-Newton).
%   max_iter  -   Maximum number of (succesful) Levenberg-Marquardt steps.
%   b_tol     -   Tolerance of degenerate complementarity: Dimensions where
%                 max( min(abs(x-l),abs(u-x)) , abs(phi(x)) ) < b_tol
%                 are clamped to the nearest constraint and removed from
%                 the linear system.
%   
% ALGORITHM
%   This function implements the semismooth algorithm as described in [1],
%   with a least-squares minimization of the Fischer-Burmeister function using
%   a Levenberg-Marquardt trust-region scheme with mu-control as in [2].
%
%   [1] A. Fischer, A Newton-Type Method for Positive-Semidefinite Linear
%   Complementarity Problems, Journal of Optimization Theory and
%   Applications: Vol. 86, No. 3, pp. 585-608, 1995.
%
%   [2] M. S. Bazarraa, H. D. Sherali, and C. M. Shetty, Nonlinear
%   Programming: Theory and Algorithms. John Wiley and Sons, 1993.
%
%   Copyright (c) 2008, Yuval Tassa
%   tassa at alice dot huji dot ac dot il

tol            = 1.0e-12;
mu             = 1e-3;
mu_step        = 5;
mu_min         = 1e-5;
max_iter       = 10;
b_tol          = 1e-6;

n              = size(M,1);

if nargin < 3 || isempty(l)
   l = zeros(n,1);
   if nargin < 4 || isempty(u)
      u = inf(n,1);
      if nargin < 5 || isempty(x0)
         x0 = min(max(ones(n,1),l),u);
         if nargin < 6
            display   = false;
         end
      end
   end
end

lu             = [l u];
x              = x0;

[psi,phi,J]    = FB(x,q,M,l,u);
new_x          = true;
warning off MATLAB:nearlySingularMatrix
for iter = 1:max_iter
   if new_x
      [mlu,ilu]      = min([abs(x-l),abs(u-x)],[],2);
      bad            = max(abs(phi),mlu) < b_tol;
      psi            = psi - 0.5*phi(bad)'*phi(bad);
      J              = J(~bad,~bad);
      phi            = phi(~bad); 
      new_x          = false;
      nx             = x;
      nx(bad)        = lu(find(bad)+(ilu(bad)-1)*n);
   end
   H              = J'*J + mu*speye(sum(~bad));
   Jphi           = J'*phi;
   
   d              = -H\Jphi;

   nx(~bad)       = x(~bad) + d;
   [npsi,nphi,nJ] = FB(nx,q,M,l,u);
   r              = (psi - npsi)  / -(Jphi'*d + 0.5*d'*H*d);  % actual reduction / expected reduction
   if r < 0.3           % small reduction, increase mu
      mu = max(mu*mu_step,mu_min);
   end
   if r > 0             % some reduction, accept nx
      x     = nx;
      psi   = npsi;
      phi   = nphi;
      J     = nJ;
      new_x = true;
      if r > 0.8       % large reduction, decrease mu
         mu = mu/mu_step * (mu > mu_min);
      end      
   end
   if display
      disp(sprintf('iter = %2d, psi = %3.0e, r = %3.1f, mu = %3.0e',iter,psi,r,mu));
   end
   if psi < tol 
      break;
   end
end
warning on MATLAB:nearlySingularMatrix
x = min(max(x,l),u);
end

function [psi,phi,J] = FB(x,q,M,l,u)
n     = length(x);
Zl    = l >-inf & u==inf;
Zu    = l==-inf & u <inf;
Zlu   = l >-inf & u <inf;
Zf    = l==-inf & u==inf;

a     = x;
b     = M*x+q;

a(Zl) = x(Zl)-l(Zl);

a(Zu) = u(Zu)-x(Zu);
b(Zu) = -b(Zu);

if any(Zlu)
   nt     = sum(Zlu);
   at     = u(Zlu)-x(Zlu);
   bt     = -b(Zlu);
   st     = sqrt(at.^2 + bt.^2);
   a(Zlu) = x(Zlu)-l(Zlu);
   b(Zlu) = st -at -bt;
end

s        = sqrt(a.^2 + b.^2);
phi      = s - a - b;
phi(Zu)  = -phi(Zu);
phi(Zf)  = -b(Zf);

psi      = 0.5*phi'*phi;

if nargout == 3
   if any(Zlu)
      M(Zlu,:) = -sparse(1:nt,find(Zlu),at./st-ones(nt,1),nt,n) - sparse(1:nt,1:nt,bt./st-ones(nt,1))*M(Zlu,:);
   end
   da       = a./s-ones(n,1);
   db       = b./s-ones(n,1);
   da(Zf)   = 0;
   db(Zf)   = -1;   
   J        = sparse(1:n,1:n,da) + sparse(1:n,1:n,db)*M;
end
end

function z_final = LCP_solver(B,q,z0)
options = optimoptions('fmincon','Algorithm','sqp',...
    'OptimalityTolerance',1e-11,'StepTolerance',1e-11, ...
    'FunctionTolerance',1e-8,'ConstraintTolerance',1e-11, ...
    'MaxIterations',10000, 'MaxFunctionEvaluations',numel(z0)*1e3, 'Display','off');

[z_final, ~, exitflag,   ~] = fmincon(@objective, ...
    z0, [], [], [], [], [], [], @constraints, options);
 if exitflag < 0
        disp(['   LCP failed'])
 elseif  exitflag ==0
       disp(['   LCP cutoff'])
 end

    function[c, ceq] = constraints(z)
        c = [-(B*z+q)', -z'];
        ceq = [];
    end
    function output = objective(z)
        output = z'*(B*z+q);
    end
end


 

