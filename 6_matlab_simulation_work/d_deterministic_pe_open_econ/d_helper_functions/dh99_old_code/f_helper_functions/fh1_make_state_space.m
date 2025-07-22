function [Q,D,state_space,Sigma] = fh1_make_state_space(num_mkts,I, lambda_tilde, sigma_z, theta)

%% Construct D and Q
Q = zeros(num_mkts,num_mkts); 
for index = 1:num_mkts^2
    [i, j] = ind2sub([num_mkts,num_mkts], index);
    if i == j
        Q(index) = sigma_z(i)^2;
    else
        Q(index) =  sigma_z(i)*sigma_z(j)*lambda_tilde;
    end
end

D =  diag(repmat(-theta,1,num_mkts)); % mean reversion parameter term


%% Construct standard version of Sigma 
Sigma_ub = fake_layp(D, Q); % sets drift of Sigma to zero when we're in no markets
Sigma_lb = [repmat(1e-2,1,2),0];
state_space_ub =  [diag(Sigma_ub)',1];
vecs = arrayfun(@(lb, ub) linspace(lb, ub, I), Sigma_lb, state_space_ub, 'UniformOutput', false);
[nvecs{1:numel(vecs)}] = ndgrid(vecs{:});
state_space = cell2mat(cellfun(@(x) x(:), nvecs, 'UniformOutput', false));
Sigma = [state_space(:,1), ...
         state_space(:,3).*sqrt(state_space(:,1).*state_space(:,2)),...
         state_space(:,2)];

end

