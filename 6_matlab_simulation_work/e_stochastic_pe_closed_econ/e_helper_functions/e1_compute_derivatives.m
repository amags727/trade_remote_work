function [dv_f, dv_b, dv_2] = e1_compute_derivatives(v, d_state_space,I, num_state_vars, len_state)
% compute_derivatives
% Computes forward, backward, and central (second) differences
%
% INPUTS:
%   d_state_space    - vector of grid step sizes (1 x num_state_vars)
%   I                - grid mesh

% OUTPUTS:
%   dv_f             - [len_state x num_state_vars] forward differences
%   dv_b             - [len_state x num_state_vars] backward differences
%   dv_2             - [len_state x num_state_vars] second derivatives (central diff.)
v_reshaped = reshape(v, [repmat(I, 1, num_state_vars)]);
dv_f = zeros(len_state, num_state_vars);
dv_b = zeros(len_state, num_state_vars);
dv_2 = zeros(len_state, num_state_vars);

parfor state_num = 1:num_state_vars
    idx_all = repmat({':'}, 1, num_state_vars);
    
    %% Forward difference
    idx_f = idx_all;      idx_f{state_num} = 1:I-1;
    idx_next = idx_all;   idx_next{state_num} = 2:I;
    temp = zeros(size(v_reshaped));
    temp(idx_f{:}) = v_reshaped(idx_next{:}) - v_reshaped(idx_f{:});
    
    % Boundary: backward diff
    idx_last = idx_all;   idx_last{state_num} = I;
    idx_prev = idx_all;   idx_prev{state_num} = I-1;
    temp(idx_last{:}) = v_reshaped(idx_last{:}) - v_reshaped(idx_prev{:});
    
    dv_f(:, state_num) = reshape(temp, len_state, 1) / d_state_space(state_num);
    
    %% Backward difference
    idx_b = idx_all;      idx_b{state_num} = 2:I;
    idx_prev = idx_all;   idx_prev{state_num} = 1:I-1;
    temp2 = zeros(size(v_reshaped));
    temp2(idx_b{:}) = v_reshaped(idx_b{:}) - v_reshaped(idx_prev{:});
    
    % Boundary: forward diff
    idx_first = idx_all;  idx_first{state_num} = 1;
    idx_next = idx_all;   idx_next{state_num} = 2;
    temp2(idx_first{:}) = v_reshaped(idx_next{:}) - v_reshaped(idx_first{:});
    
    dv_b(:, state_num) = reshape(temp2, len_state, 1) / d_state_space(state_num);
    
    %% Second derivative (central difference)
    idx_c = idx_all;      idx_c{state_num} = 2:I-1;
    idx_next = idx_all;   idx_next{state_num} = 3:I;
    idx_prev = idx_all;   idx_prev{state_num} = 1:I-2;
    temp = zeros(size(v_reshaped));
    temp(idx_c{:}) = v_reshaped(idx_next{:}) - 2 * v_reshaped(idx_c{:}) + v_reshaped(idx_prev{:});
    
    % Boundary: 3-point forward/backward stencil
    idx_last = idx_all;    idx_last{state_num} = I;
    idx_prev = idx_all;    idx_prev{state_num} = I-1;
    idx_prev2 = idx_all;   idx_prev2{state_num} = I-2;
    temp(idx_last{:}) = -v_reshaped(idx_prev2{:}) + 4 * v_reshaped(idx_prev{:}) - 3 * v_reshaped(idx_last{:});
    
    idx_first = idx_all;   idx_first{state_num} = 1;
    idx_next = idx_all;    idx_next{state_num} = 2;
    idx_next2 = idx_all;   idx_next2{state_num} = 3;
    temp(idx_first{:}) = -v_reshaped(idx_next2{:}) + 4 * v_reshaped(idx_next{:}) - 3 * v_reshaped(idx_first{:});
    
    dv_2(:, state_num) = reshape(temp, len_state, 1) / (d_state_space(state_num)^2);
end
end