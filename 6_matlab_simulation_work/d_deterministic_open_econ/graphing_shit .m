%% 
z_mat = reshape(z,[],num_networks);
preferred_network = repmat(1:num_networks, len_Sigma,1).*(reshape(z,[],num_networks)>0) +...
                    best_alt.*(reshape(z,[],num_networks) ==0);

time_periods = Delta*10;
state = len_Sigma*ones(time_periods,1); arrival_network = ones(time_periods, 1);
departure_network = arrival_network;
L_over_time = zeros(time_periods, num_mkts);
position = repmat(Sigma(len_Sigma, :), time_periods,1);

for t = 1:(time_periods-1)
    departure_network(t) =  preferred_network(state(t),arrival_network(t));
    L_over_time(t,:) = L(state(t),:,  departure_network(t));
    position(t+1,:) = position(t,:) + drift(state(t),:,departure_network(t))./Delta; 
    [~,min_row] =  min(sqrt(sum((Sigma - position(t+1,:)).^2, 2))); state(t+1) = min_row;
    arrival_network(t+1) = departure_network(t);
end 
%plot(1:time_periods, position(:,3))
[min(preferred_network);max(preferred_network)]