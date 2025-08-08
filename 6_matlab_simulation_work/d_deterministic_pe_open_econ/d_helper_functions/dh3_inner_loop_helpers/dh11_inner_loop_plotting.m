function [graph_output, v_ss, A_tilde_out, network_ss]= dh11_inner_loop_plotting(optim, params)

%% UNPACK PARAMS 
fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end

disp('hi')
preferred_network = repmat(1:num_networks, len_Sigma,1).*(reshape(z,[],num_networks)>0) +...
        best_alt.*(reshape(z,[],num_networks) ==0);

    % set the initial values of Sigma and network
    Sigma_t = Sigma(len_Sigma,:); network_t = 1; drift_mag = Inf; drift_crit = 7e-3;
    [indices, weights] = dh4_interp_box(Sigma_t, Sigma, 2); max_tsteps = 1e4;
    graph_output = zeros(max_tsteps,14); t =1;
    while drift_mag > drift_crit & t < max_tsteps
        graph_output(t,:) = [network_t,Sigma_t([1,3]),1 - Sigma_t([1,3])./Sigma(len_Sigma,[1,3]),...
            sum(weights.*optim.profit_w_actions(indices,network_t)), ...
            sum(weights.*(E_pi(indices,:,network_t) - optim.L(indices, :,network_t)*w)) - fc.*networks(network_t,:),... % profits w/o entry costs
            sum(E_x(indices,:,network_t).*weights),... % output
            sum(optim.L(indices,:,network_t).*weights),... % L
            sum(optim.R_vec(indices, :,network_t).*weights)]; % R

        if t > 40
            old_range = t-39:t-20;
            new_range = t-19: t;
            if max(abs(mean(graph_output(old_range, [2,3])) - mean(graph_output(new_range, [2,3])))) < 1e-4
                break
            end
        end 

        if t ~=1 && network_t == 2 && graph_output(t-1, 1) == 1
            graph_output(t,6) = graph_output(t,6) - ec(2);
        end

        drift_t = sum(optim.drift(indices,:,network_t).*weights);
        drift_mag = sum(abs(drift_t(:,[1,3])));
        Sigma_t = Sigma_t + drift_t*1/Delta;
        [indices, weights] = dh4_interp_box( Sigma_t, Sigma, 2);
        best_score = -inf; best_network = 1; pref_base = [preferred_network(indices,network_t),weights];
        for network = 1:num_networks
            score = sum(pref_base(pref_base(:,1) == network, 2));
            if score > best_score
                best_score = score; best_network = network;
            end
        end
        network_t = best_network;
        t = t+1;
    end
    if t == max_tsteps; disp('firm never reached a ss'); end
    network_ss = network_t;
    graph_output = graph_output(1:(find(graph_output(:,1)==0,1,"first")-1),:);
    v_ss = sum(v(indices,network_t).*weights);
    A_tilde_out = sum(A_tilde(indices,:).*weights).*networks(network_ss,:);
    market_2_entrance_t = find(graph_output(:,1) ==2,1,'first');
    output_names = {'graph_output','network_ss','A_tilde_out', 'v_ss', 'market_2_entrance_t', 'p','P'};
    for i = 1:length(output_names); name = output_names{i}; output.(name) = eval(name); end
end


