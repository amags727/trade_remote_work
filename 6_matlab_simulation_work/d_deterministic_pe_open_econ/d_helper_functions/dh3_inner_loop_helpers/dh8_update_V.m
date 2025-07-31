function [v,int_indices, dist] = dh8_update_V(v,V,params, dist, n)

% unpack params 
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end
[~,max_idx] = max(abs(V(:)-v(:))); max_change = V(max_idx) - v(max_idx);
Vchange = abs(V - v);
baseline = median(Vchange(:));
dist(n) = max(Vchange(:));

% Classify indices
problem_indices  = Vchange > 1e2 * baseline & Vchange < 1e4 * baseline;
garbage_indices  = Vchange > 1e4 * baseline;

% Check for steady downward trend if applicable
steady_down = false;
if n > 50
    idx = (n-49):(n-1);  % 99 steps: from n-99 to n-1
    steady_down = all(diff(dist(idx)) < 0);
end

% Set relaxation parameters and update v
if size(networks,1) == 1 & ~isequal(networks,[1,1] )
    relax = 0;
else
    relax = .9 * ones(size(V));
    if steady_down
        relax(problem_indices) = .99;
        relax(garbage_indices) = .995;
    else
        relax(problem_indices) = .995;
        relax(garbage_indices) = .999;
    end
end
v = relax .* v + (1 - relax) .* V;

% Determine int indices for next round 
int_indices = find(any(Vchange > 1e-4,2));
int_indices = setdiff(unique(reshape(adjacency_matrix(int_indices,:),[],1)),0);
if mod(n,10) == 0; int_indices = 1:len_Sigma; end


% report progress
if mod(n, 25) == 0
    mode = "HJB";
    if size(V,2) > 1, mode = "LCP"; end
    fprintf('%s: %g: max %g, 99th pctile %g, median %g\n', ...
        mode, n, max_change, prctile(Vchange(:),99), prctile(Vchange(:),50));
end