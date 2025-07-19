function x = LCP_partial(M,q,l,u,x0,display,free_idx)
%LCP Solve the Linear Complementarity Problem with optional partial optimization.

% Default parameters
tol            = 1.0e-6;
mu             = 1e-3;
mu_step        = 5;
mu_min         = 1e-5;
max_iter       = 10;
b_tol          = 1e-6;

n              = size(M,1);

if nargin < 3 || isempty(l)
   l = zeros(n,1);
end
if nargin < 4 || isempty(u)
   u = inf(n,1);
end
if nargin < 5 || isempty(x0)
   x0 = min(max(ones(n,1),l),u);
end
if nargin < 6
   display = false;
end
if nargin < 7 || isempty(free_idx)
   free_idx = true(n,1); % default: optimize all variables
elseif isnumeric(free_idx)
   tmp = false(n,1);
   tmp(free_idx) = true;
   free_idx = tmp;
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
   
   d_sub          = -H\Jphi;
   d_full         = zeros(n,1);

   % Only update free and active variables
   update_idx     = find(~bad);
   active_free    = update_idx(free_idx(update_idx));
   d_full(active_free) = d_sub(free_idx(update_idx));
   
   nx(~bad)       = x(~bad) + d_full(~bad);
   
   % Clamp fixed variables to original values
   nx(~free_idx)  = x0(~free_idx);

   [npsi,nphi,nJ] = FB(nx,q,M,l,u);
   r              = (psi - npsi) / -(Jphi'*d_sub + 0.5*d_sub'*H*d_sub);

   if r < 0.3
      mu = max(mu*mu_step,mu_min);
   end
   if r > 0
      x     = nx;
      psi   = npsi;
      phi   = nphi;
      J     = nJ;
      new_x = true;
      if r > 0.8
         mu = mu/mu_step * (mu > mu_min);
      end
   end

   if display
      fprintf('iter = %2d, psi = %3.0e, r = %3.1f, mu = %3.0e\n', iter, psi, r, mu);
   end
   if psi < tol
      break;
   end
end

warning on MATLAB:nearlySingularMatrix
x = min(max(x,l),u);
x(~free_idx) = x0(~free_idx); % Final clamping

end

function [psi,phi,J] = FB(x,q,M,l,u)
n     = length(x);
Zl    = l > -inf & u == inf;
Zu    = l == -inf & u < inf;
Zlu   = l > -inf & u < inf;
Zf    = l == -inf & u == inf;

a     = x;
b     = M*x + q;

a(Zl) = x(Zl) - l(Zl);
a(Zu) = u(Zu) - x(Zu);
b(Zu) = -b(Zu);

if any(Zlu)
   nt     = sum(Zlu);
   at     = u(Zlu) - x(Zlu);
   bt     = -b(Zlu);
   st     = sqrt(at.^2 + bt.^2);
   a(Zlu) = x(Zlu) - l(Zlu);
   b(Zlu) = st - at - bt;
end

s        = sqrt(a.^2 + b.^2);
phi      = s - a - b;
phi(Zu)  = -phi(Zu);
phi(Zf)  = -b(Zf);

psi      = 0.5*phi'*phi;

if nargout == 3
   if any(Zlu)
      M(Zlu,:) = -sparse(1:nt,find(Zlu),at./st - 1,nt,n) - sparse(1:nt,1:nt,bt./st - 1)*M(Zlu,:);
   end
   da       = a./s - 1;
   db       = b./s - 1;
   da(Zf)   = 0;
   db(Zf)   = -1;   
   J        = sparse(1:n,1:n,da) + sparse(1:n,1:n,db)*M;
end
end