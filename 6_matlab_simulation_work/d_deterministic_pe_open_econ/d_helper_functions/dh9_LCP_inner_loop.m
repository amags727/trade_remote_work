function [v, optim, z, best_alt,converged] = dh9_LCP_inner_loop(v, params)

% unpack params 
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end


end