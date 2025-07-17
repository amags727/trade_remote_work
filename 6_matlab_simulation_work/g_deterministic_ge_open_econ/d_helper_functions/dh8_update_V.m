function [V,z,best_alt] = dh8_update_V(LCP_yn,profit,v, A_matrix,A_matrix_cells, rho, Delta, networks, ec, rev_ec)
%===SETUP ====
[len_Sigma, num_networks] = size(v);

   
% If we're just doing value func iteration 
if ~LCP_yn
    V = zeros(size(v)); z = 0; best_alt = 0;
    for network = 1:num_networks
        B = (rho + 1/Delta)*speye(len_Sigma) - A_matrix_cells{network};
        b = profit(:,network) + v(:,network)./Delta;
        V(:,network) = B\b;
    end

% If we're solving the LCP  
else
    B = (rho + 1/Delta)*speye(len_Sigma.*num_networks) - A_matrix;
    pi_stacked = reshape(profit, [],1);
    v_stacked = reshape(v, [], 1);
    vstar = zeros(size(v)); best_alt = vstar;
    for network = 1:num_networks
        temp = v; temp(:,network) = -inf;
        temp = temp - sum(max(0, networks - networks(network,:)).*ec,2)'...
            -  sum(max(0, networks(network,:)- networks).*rev_ec,2)';
        [vstar(:,network), best_alt(:,network)] = max(temp, [],2);
    end
    vstar_stacked = reshape(vstar, [], 1);
    vec = pi_stacked + v_stacked/Delta;
    q = -vec + B*vstar_stacked;
    z0 = v_stacked - vstar_stacked;
    l = zeros(size(v_stacked));
    u = Inf*ones(size(v_stacked));
    z = LCP(B,q,l,u,z0,0);
    LCP_error = max(abs(z.*(B*z + q)));
    if LCP_error > 10^(-5)
        z = LCP(B,q,l,u,z,0);
        %fprintf('LCP not solved: error = %g\n', LCP_error);
    end
    % update the value function
    V= reshape(z+ vstar_stacked, [], num_networks);
    z = reshape(z, [], num_networks);
end
end