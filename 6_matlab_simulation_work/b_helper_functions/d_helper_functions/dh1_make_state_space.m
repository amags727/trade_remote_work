function [Q,D,Sigma] = d1_make_state_space(num_mkts,I, lambda, sigma_z, theta)

%% Construct D and Q
Q = zeros(num_mkts,num_mkts); 
for index = 1:num_mkts^2
    [i, j] = ind2sub([num_mkts,num_mkts], index);
    if i == j
        Q(index) = sigma_z(i)^2;
    else
        Q(index) =  sigma_z(i)*sigma_z(j)*lambda;
    end
end

D =  diag(repmat(-theta,1,num_mkts)); % mean reversion parameter term


%% Construct standard version of Sigma 
Sigma_ub = fake_layp(D, Q); % sets drift of Sigma to zero when we're in no markets
Sigma_lb = 1e-2;
state_space_ub =  Sigma_ub(triu(true(size(Sigma_ub)), 0));
vecs = arrayfun(@(x) linspace(Sigma_lb, x, I), state_space_ub, 'UniformOutput', false);
[nvecs{1:numel(vecs)}] = ndgrid(vecs{:});
Sigma = cell2mat(cellfun(@(x) x(:), nvecs, 'UniformOutput', false));


end

