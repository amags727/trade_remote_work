% for each index find optimal values and the perform upwind procedure 
function output = inner_loop_optimization_calcs(exog,starting_point,division_num) 
v_size = size(starting_point.v);
drift_b  =  cell(v_size); drift_f = cell(v_size);
choices_inner = cell(v_size); drift_inner = cell(v_size); profits_inner = zeros(v_size);
choices_b = choices_inner; choices_f = choices_inner; approx_used = cell(v_size);

%index = sub2ind(v_size, 151,1,1,2);
parfor index = exog.divisions{division_num}
        [state_num, phi_idx, home_country, network] = ind2sub(v_size,index);
        env = exog;
        env.current_min = Inf; env.best_profit = 0; env.best_state_drift = 0; env.best_choice_vector = 0;
        env.home_country = home_country; env.network = network; env.state_num = state_num;

        env.phi = env.phi_distrib(phi_idx);
        env.lb = -(env.state_minus_1(state_num,:)==0); env.ub = env.state_plus_1(state_num,:)==0;
        env.stationary = env.state_plus_1(state_num,:)==0 & env.state_minus_1(state_num,:)==0;
       
        % prepare for the upwind scheme
        If = env.lb~=0;
        Ib = env.ub~=0;
        num_undecided_vars = inf;
        undecided_vars = ~If & ~Ib;
        temp_dv_b = squeeze(starting_point.dv_b(state_num, phi_idx, :, home_country, network));
        temp_dv_f = squeeze(starting_point.dv_b(state_num, phi_idx, :, home_country, network));

        while num_undecided_vars ~= sum(undecided_vars)
        num_undecided_vars = sum(undecided_vars);

        %backwards approximation
        env.dv = temp_dv_b; env.dv(If) = temp_dv_f(If);
        init_val =  starting_point.choices_b{index};
        [choices_b{index}, drift_b{index}, profits_b] = optim_finder(env,starting_point.P,init_val);


        %forwards approximation
        env.dv = temp_dv_f; env.dv(Ib) = temp_dv_b(Ib);
        init_val =  starting_point.choices_f{index};
        [choices_f{index}, drift_f{index}, profits_f] = optim_finder(env,starting_point.P,init_val);

        temp_drift_f = drift_f{index}; temp_drift_b = drift_b{index};
       
        %choose which approx to use via upwind procedure
        Ham_f = profits_f + temp_drift_f * temp_dv_f;
        Ham_b = profits_b + temp_drift_b * temp_dv_b;

        Iboth = (temp_drift_b<0).*(temp_drift_f>0);
        Iunique = (temp_drift_b<0).*(1-(temp_drift_f>0)) + (1-(temp_drift_b<0)).*(temp_drift_f>0);
        Ib = Iunique.*(temp_drift_b<0) | Iboth.*(Ham_b>=Ham_f);
        If = Iunique.*(temp_drift_f>0) | Iboth.*(Ham_f>=Ham_b);
        
        % adjust for the state space boundaries
         If(env.lb~=0) = 1; If(env.ub~=0) = 0;
         Ib(env.lb~=0) = 0; Ib(env.ub~=0) = 1;

        
        if  min(Ib(:)) % can use b results
            choices_inner{index} = choices_b{index};
            profits_inner(index) = profits_b;
            drift_inner{index} = drift_b{index};
        

        elseif min(If(:))  % can use f results
            choices_inner{index} = choices_f{index};
            profits_inner(index) = profits_f;
            drift_inner{index} = drift_f{index};
         
        else % re-calc if necessary
            env.dv = zeros( env.num_state_vars, 1);
            env.dv(Ib) = temp_dv_b(Ib); env.dv(If) = temp_dv_f(If); 
            dv_avg = (Ham_f > Ham_b)*temp_dv_f + (Ham_f <= Ham_b)*temp_dv_b; %.5*(temp_dv_b + temp_dv_f)
            env.dv(~Ib & ~If) = dv_avg(~Ib & ~If);
           
            env.stationary =  ~Ib & ~If;   % requirement that equal zero
            [choices_inner{index}, drift_inner{index}, profits_inner(index)] =...
                optim_finder(env,starting_point.P,init_val);
        end
        undecided_vars = ~Ib & ~If;
        if sum(undecided_vars) == 0; break; end
        end
end
    %save everything as output for export;
    output.choices_inner = choices_inner; output.profits_inner = profits_inner;
    output.drift_b = drift_b; output.drift_f = drift_f; output.drift_inner= drift_inner;
    output.choices_b = choices_b; output.choices_f = choices_f; 
end


function [cvars_out, out_state_drift, out_profit] = optim_finder(env, P,init_choice)
no_viable_point = 1;
options = optimoptions('fmincon','Algorithm','sqp',...
   'StepTolerance',1e-11, ...
    'OptimalityTolerance',1e-7,'ConstraintTolerance',1e-11, ...
    'MaxIterations',1e3,'MaxFunctionEvaluations',env.num_choice_vars*5e2,'Display','off'); %, 
% run the optimization 
 [cvars_out, ~, exitflag,   ~] = fmincon(@objective, ...
            init_choice, [], [], [], [], [], [], @constraints, options);

% output additional results 
    out_drift = drift_calc(env,cvars_out, P, env.network, env.state_num);
    for j = 1:env.num_state_vars
        out_state_drift(j) = out_drift(env.state_keys(j,1),env.state_keys(j,2));
    end
    out_profit = profit_calc(env,cvars_out,P, env.home_country, env.network, env.state_num, env.phi);

% deal with failure 
%  if exitflag ~=1  && exitflag~=2
%      disp(['exit flag =', num2str(exitflag),' network ', num2str(env.network), ' state ', num2str(env.state_num)])
%    out_profit = env.best_profit;
%    out_state_drift = env.best_state_drift;
%    cvars_out = env.best_choice_vector;
%    if no_viable_point
%        disp(['no viable point: network ', num2str(env.network), ' state ', num2str(env.state_num)])
%    end
%  end

% define helper functions 
    function[c, ceq] = constraints(choice_vector)
        drift = drift_calc(env,choice_vector, P, env.network, env.state_num);
        for i = 1:env.num_state_vars
            state_drift(i) = drift(env.state_keys(i,1),env.state_keys(i,2));
        end
        c =[-choice_vector,...
            state_drift(env.lb~=0) .* env.lb(env.lb~=0),...
            state_drift(env.ub~=0) .* env.ub(env.ub~=0)];
        ceq = state_drift(env.stationary~=0) .* env.stationary(env.stationary~=0);
    end

    function output = objective(choice_vector)
        drift = drift_calc(env,choice_vector, P, env.network, env.state_num);
        for i = 1:env.num_state_vars
            state_drift(i) = drift(env.state_keys(i,1),env.state_keys(i,2));
        end
        profit = profit_calc(env,choice_vector,P, env.home_country, env.network, env.state_num, env.phi);
        output = -(profit + sum(state_drift .* env.dv'));
%         [c_temp_1, c_temp_2] = constraints(choice_vector);
%         c_temp = [c_temp_1, c_temp_2];
%         if max(c_temp)<= 0+1e-10 && output <= env.current_min
%             no_viable_point = 0;
%             env.current_min = output;
%             env.best_profit = profit;
%             env.best_state_drift = state_drift;
%             env.best_choice_vector = choice_vector;
%          end
    end
end

 

% for each index find optimal values and the perform upwind procedure 
function output = inner_loop_optimization_calcs(exog,starting_point,division_num) 
v_size = size(starting_point.v);
drift_b  =  cell(v_size); drift_f = cell(v_size);
choices_inner = cell(v_size); drift_inner = cell(v_size); profits_inner = zeros(v_size);
choices_b = choices_inner; choices_f = choices_inner; approx_used = cell(v_size);

%index = sub2ind(v_size, 151,1,1,2);
parfor index = exog.divisions{division_num}
        [state_num, phi_idx, home_country, network] = ind2sub(v_size,index);
        env = exog;
        env.current_min = Inf; env.best_profit = 0; env.best_state_drift = 0; env.best_choice_vector = 0;
        env.home_country = home_country; env.network = network; env.state_num = state_num;

        env.phi = env.phi_distrib(phi_idx);
        env.lb = -(env.state_minus_1(state_num,:)==0); env.ub = env.state_plus_1(state_num,:)==0;
        env.stationary = env.state_plus_1(state_num,:)==0 & env.state_minus_1(state_num,:)==0;
       
        % prepare for the upwind scheme
        If = env.lb~=0;
        Ib = env.ub~=0;
        num_undecided_vars = inf;
        undecided_vars = ~If & ~Ib;
        temp_dv_b = squeeze(starting_point.dv_b(state_num, phi_idx, :, home_country, network));
        temp_dv_f = squeeze(starting_point.dv_b(state_num, phi_idx, :, home_country, network));

        while num_undecided_vars ~= sum(undecided_vars)
        num_undecided_vars = sum(undecided_vars);

        %backwards approximation
        env.dv = temp_dv_b; env.dv(If) = temp_dv_f(If);
        init_val =  starting_point.choices_b{index};
        [choices_b{index}, drift_b{index}, profits_b] = optim_finder(env,starting_point.P,init_val);


        %forwards approximation
        env.dv = temp_dv_f; env.dv(Ib) = temp_dv_b(Ib);
        init_val =  starting_point.choices_f{index};
        [choices_f{index}, drift_f{index}, profits_f] = optim_finder(env,starting_point.P,init_val);

        temp_drift_f = drift_f{index}; temp_drift_b = drift_b{index};
       
        %choose which approx to use via upwind procedure
        Ham_f = profits_f + temp_drift_f * temp_dv_f;
        Ham_b = profits_b + temp_drift_b * temp_dv_b;

        Iboth = (temp_drift_b<0).*(temp_drift_f>0);
        Iunique = (temp_drift_b<0).*(1-(temp_drift_f>0)) + (1-(temp_drift_b<0)).*(temp_drift_f>0);
        Ib = Iunique.*(temp_drift_b<0) | Iboth.*(Ham_b>=Ham_f);
        If = Iunique.*(temp_drift_f>0) | Iboth.*(Ham_f>=Ham_b);
        
        % adjust for the state space boundaries
         If(env.lb~=0) = 1; If(env.ub~=0) = 0;
         Ib(env.lb~=0) = 0; Ib(env.ub~=0) = 1;

        
        if  min(Ib(:)) % can use b results
            choices_inner{index} = choices_b{index};
            profits_inner(index) = profits_b;
            drift_inner{index} = drift_b{index};
        

        elseif min(If(:))  % can use f results
            choices_inner{index} = choices_f{index};
            profits_inner(index) = profits_f;
            drift_inner{index} = drift_f{index};
         
        else % re-calc if necessary
            env.dv = zeros( env.num_state_vars, 1);
            env.dv(Ib) = temp_dv_b(Ib); env.dv(If) = temp_dv_f(If); 
            dv_avg = (Ham_f > Ham_b)*temp_dv_f + (Ham_f <= Ham_b)*temp_dv_b; %.5*(temp_dv_b + temp_dv_f)
            env.dv(~Ib & ~If) = dv_avg(~Ib & ~If);
           
            env.stationary =  ~Ib & ~If;   % requirement that equal zero
            [choices_inner{index}, drift_inner{index}, profits_inner(index)] =...
                optim_finder(env,starting_point.P,init_val);
        end
        undecided_vars = ~Ib & ~If;
        if sum(undecided_vars) == 0; break; end
        end
end
    %save everything as output for export;
    output.choices_inner = choices_inner; output.profits_inner = profits_inner;
    output.drift_b = drift_b; output.drift_f = drift_f; output.drift_inner= drift_inner;
    output.choices_b = choices_b; output.choices_f = choices_f; 
end


function [cvars_out, out_state_drift, out_profit] = optim_finder(env, P,init_choice)
no_viable_point = 1;
options = optimoptions('fmincon','Algorithm','sqp',...
   'StepTolerance',1e-11, ...
    'OptimalityTolerance',1e-7,'ConstraintTolerance',1e-11, ...
    'MaxIterations',1e3,'MaxFunctionEvaluations',env.num_choice_vars*5e2,'Display','off'); %, 
% run the optimization 
 [cvars_out, ~, exitflag,   ~] = fmincon(@objective, ...
            init_choice, [], [], [], [], [], [], @constraints, options);

% output additional results 
    out_drift = drift_calc(env,cvars_out, P, env.network, env.state_num);
    for j = 1:env.num_state_vars
        out_state_drift(j) = out_drift(env.state_keys(j,1),env.state_keys(j,2));
    end
    out_profit = profit_calc(env,cvars_out,P, env.home_country, env.network, env.state_num, env.phi);

% deal with failure 
%  if exitflag ~=1  && exitflag~=2
%      disp(['exit flag =', num2str(exitflag),' network ', num2str(env.network), ' state ', num2str(env.state_num)])
%    out_profit = env.best_profit;
%    out_state_drift = env.best_state_drift;
%    cvars_out = env.best_choice_vector;
%    if no_viable_point
%        disp(['no viable point: network ', num2str(env.network), ' state ', num2str(env.state_num)])
%    end
%  end

% define helper functions 
    function[c, ceq] = constraints(choice_vector)
        drift = drift_calc(env,choice_vector, P, env.network, env.state_num);
        for i = 1:env.num_state_vars
            state_drift(i) = drift(env.state_keys(i,1),env.state_keys(i,2));
        end
        c =[-choice_vector,...
            state_drift(env.lb~=0) .* env.lb(env.lb~=0),...
            state_drift(env.ub~=0) .* env.ub(env.ub~=0)];
        ceq = state_drift(env.stationary~=0) .* env.stationary(env.stationary~=0);
    end

    function output = objective(choice_vector)
        drift = drift_calc(env,choice_vector, P, env.network, env.state_num);
        for i = 1:env.num_state_vars
            state_drift(i) = drift(env.state_keys(i,1),env.state_keys(i,2));
        end
        profit = profit_calc(env,choice_vector,P, env.home_country, env.network, env.state_num, env.phi);
        output = -(profit + sum(state_drift .* env.dv'));
%         [c_temp_1, c_temp_2] = constraints(choice_vector);
%         c_temp = [c_temp_1, c_temp_2];
%         if max(c_temp)<= 0+1e-10 && output <= env.current_min
%             no_viable_point = 0;
%             env.current_min = output;
%             env.best_profit = profit;
%             env.best_state_drift = state_drift;
%             env.best_choice_vector = choice_vector;
%          end
    end
end

 

