 function graphical_analysis(exog,PE, P, output_name)

% if output_stub(1)=='I' sub_folder = 'results/PE/graphs/' 
structures = {'exog', 'PE'};
for idx = 1:length(structures)
    fields = fieldnames(eval(structures{idx})); % Get the field names of the structure
    for idx2 = 1:length(fields)
        fieldName = fields{idx2};
        eval([fieldName ' = ' structures{idx} '.' fieldName ';']);
    end
end


% find equilibrium path 
num_periods = 10/dt;
state_t = zeros(num_periods, num_state_vars, phi_options, N_only);  state_t(1,:,:,:) = state_space(end,:);
state_num_t = zeros(num_periods, phi_options,N_only);
arrival_network_t = ones(num_periods, phi_options, N_only);
quantities = zeros(num_periods, N, phi_options, N_only);
p_t = zeros(num_periods, N, phi_options, N_only);
scraped_data = zeros(num_periods, N, phi_options, N_only);
purchased_data = scraped_data;
scraped_impact = scraped_data;
purchased_imapct = scraped_data;
jovanovic_impact = scraped_data;

for idx = 1:phi_options*N_only
    [phi_idx, home_country] = ind2sub([phi_options, N_only], idx);
    for t = 1:num_periods
        % establish what state we're in and what network we've chosen
        [~,state_num] = min(sum((state_t(t,:,phi_idx, home_country) - state_space).^2,2));
        state_num_t(t,phi_idx,home_country) = state_num;
        arrival_network = arrival_network_t(t, phi_idx, home_country);
        if inaction_region(state_num,phi_idx,home_country,arrival_network)
            departure_network = arrival_network;
        else
            departure_network = best_alternative(state_num,phi_idx,home_country,arrival_network);
        end

        % establish choice variables
        choice_vector = choices_up{state_num, phi_idx, home_country, departure_network};
        p = choice_vector(1:N); zo = choice_vector((N+1):(2*N)); zi = choice_vector(end);
        [purchased_impact(t,:, phi_idx, home_country),scraped_impact(t,:, phi_idx, home_country), jovanovic_impact(t,:, phi_idx, home_country)]=...
            drift_decomp(exog,choice_vector,P,departure_network,state_num);
        

        % update our choices
        p_t(t,:, phi_idx, home_country) = network_list{departure_network} .*p;
        quantities(t,:, phi_idx, home_country) = network_list{departure_network} .* (A(state_num, :) .* Y .* p.^(-exog.sigma)) ./ P.^(1-exog.sigma);
        scraped_data(t, :,phi_idx,home_country) = sigmoid(zi,ci,zi_max, zi_min) .* quantities(t,:, phi_idx, home_country);
        purchased_data(t,:, phi_idx,home_country) = zo ./ co;
        
        if t~= num_periods
            state_t(t+1,:,phi_idx, home_country) = state_t(t,:,phi_idx, home_country) + dt.*drift_inner{state_num,phi_idx,home_country,departure_network};
            arrival_network_t(t+1, phi_idx, home_country) = departure_network;
        end
    end
end
networks_reached = strjoin(string(unique(arrival_network_t(:))), ', ');
disp('   networks reached: ' + networks_reached)
certainty_level_t = 1 - state_t ./ repmat(max(state_space),size(state_t,1),1);
certainty_level_t = certainty_level_t(:,diag_state_vars);
state_market_t = state_t(:,diag_state_vars);

lb_indeces = 1:num_states;
lb_indeces = lb_indeces((any(state_space(:,exog.diag_state_vars)<=0,2)));
excluded_times = ismember(state_num_t,lb_indeces);

destination = 2; number = 2;
for destination = 1:N
    data_1 = scraped_impact(:,destination, 1,1);
    data_2 = data_1 + purchased_impact(:,destination,1,1);
    data_3 = data_2 + jovanovic_impact(:,destination,1,1);
    data_1(excluded_times) = nan;data_2(excluded_times) = nan; data_3(excluded_times) = nan;

    clf
    hold on 
    plot(data_1 ./ data_3); %./data_3
    plot(data_2 ./ data_3); %
    plot(data_3 ./ data_3); %./data_3
    area(data_3 ./ data_3, 'FaceColor', 'y');
    area(data_2 ./ data_3, 'FaceColor','r');
    area(data_1 ./ data_3, 'FaceColor','b');
     saveas(gcf, ['results/PE/graphs/impact_graphs/',output_name,'_',num2str(destination) , '.png']);
end

%% Make the Graphs
input = {state_market_t, quantities, scraped_data, purchased_data, p_t};
graph_type = {'state','quantities', 'data scraped', 'data purchased', 'prices'};
for number = 1:size(input,2)
    clf; hold on; 
    for destination = 1:N
        time_series = input{number}; time_series = time_series(:,destination,1,1);
        time_series(excluded_times) = nan;
        plot(1:(10/dt),time_series,'-o','MarkerSize', 1)
    end
    title([graph_type{number}, ' over time'])
    legend('market 1', 'market 2');
    saveas(gcf, ['results/PE/graphs/',graph_type{number},'/',output_name, '.png']); 
end

input_names = {'state_market_t', 'quantities', 'scraped_data', 'purchased_data',...
    'p_t','excluded_times', 'purchased_impact', 'scraped_impact', 'jovanovic_impact'};
graph_output = struct(); 
for i = 1:size(input_names,2)
    eval('graph_output.' + string(input_names{i}) + ' = ' +string(input_names{i}) +';')  
end
save(['results/PE/graphs/mat_files/',output_name, '.mat'],'graph_output');




