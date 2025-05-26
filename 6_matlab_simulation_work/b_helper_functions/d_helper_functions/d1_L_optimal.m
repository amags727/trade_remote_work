function L_output = d1_L_optimal(dv_current,dim,Sigma_comp_optimal_L,xi, alpha_1)
L_output = zeros(dim.len_Sigma, dim.num_mkts, dim.num_networks);
mkt_combos = combvec(1:dim.num_mkts, 1:dim.num_mkts, 1:dim.num_mkts)'; 
num_combos = size(mkt_combos, 1);
L_output_temp = cell(num_combos, 1);

parfor mkt_combo_idx = 1:num_combos
   i = mkt_combos(mkt_combo_idx, 1);
   j = mkt_combos(mkt_combo_idx, 2);
   k = mkt_combos(mkt_combo_idx, 3);

   ik = dim.index_matrix(i, k);
   Sigma_comp_current = squeeze(repmat(Sigma_comp_optimal_L(:,i,j,k), [ones(1,4), dim.num_networks]));

   % Preallocate temp results
   tmp = zeros(dim.len_Sigma, 1, dim.num_networks);

   % Compute
   tmp(:, 1, :) = -squeeze(dv_current(:,ik,:)) .* Sigma_comp_current;
   L_output_temp{mkt_combo_idx} = struct('j', j, 'val', tmp);
end
for idx = 1:num_combos
    j = L_output_temp{idx}.j;
    L_output(:, j, :) = L_output(:, j, :) + L_output_temp{idx}.val;
end
L_output = L_output.^(1/(1-alpha_1)); L_output(L_output<0) = 0;  L_output = L_output.*xi;
end