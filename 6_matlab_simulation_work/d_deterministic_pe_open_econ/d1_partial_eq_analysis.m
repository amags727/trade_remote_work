clear all; close all; clc;
addpath(genpath('../b_helper_functions'));
addpath(genpath('d_helper_functions'))
addpath(genpath('d_output'))
dbstop if error

% set invariant parameters 
params = dh0_set_invariant_params();

fields = fieldnames(params); % Get the field names of the structure
for idx = 1:length(fields); eval([fields{idx} ' = params.' fields{idx} ';']); end


%% === PE setup ====
networks = [1,0;1,1];
x_scale_factor = 14;
fc = fc_base *[1,foreign_cost_scaling];
ec = ec_base *[1,foreign_cost_scaling];
x_bar = x_scale_factor*phi_g^gamma; % base demand 
pi_bar = x_bar*w_g*phi_g^-1*(gamma-1)^-1; % base profits 

E_x = x_bar.*A_tilde.* permute(networks, [3 2 1]);
E_pi = pi_bar.*A_tilde.* permute(networks, [3 2 1]); % Expected working profits (not accounting for fixed costs; data labor)
xi = Sigma_pen*alpha_1*phi_d.*E_x.^alpha_2;
v0 = zeros(len_Sigma, num_networks);

pe_vars = {'x_scale_factor','networks' 'fc', 'ec', 'E_x', 'E_pi', 'xi'};
for i = 1:length(pe_vars); name = pe_vars{i}; params.(name) = eval(name); end
params.crit = 1e-2;
v_hjb_init = dh9_HJB_inner_loop(v0,params);
output  = dh10_LCP_inner_loop(v_hjb_init, params);
fprintf('v_end - ec = %g\n',output.v(len_Sigma,1)-ec(1));
fprintf('network_ss = %g\n',output.network_ss);




function scatter_shape_color(A, xLabelStr, yLabelStr, savePath)
%SCATTER_SHAPE_COLOR Creates a scatter plot with shapes (1=circle, 2=square) and color
%
% Inputs:
%   A          - n x 4 matrix: [x, y, shape_idx, color]
%   xLabelStr  - string for x-axis label
%   yLabelStr  - string for y-axis label
%   savePath   - string, full path to save the figure (e.g., 'output/plot.png')

    % Define fixed shape mapping
    shapeMap = containers.Map({1, 2}, {'o', 's'});

    % Get unique shape indices
    shapeIDs = unique(A(:,3));
    
    % Setup figure
    figure; hold on;
    
    % Loop over each shape
    for i = 1:length(shapeIDs)
        sID = shapeIDs(i);
        if isKey(shapeMap, sID)
            shape = shapeMap(sID);
        else
            warning('Unrecognized shape index %d. Defaulting to circle.', sID);
            shape = 'o';
        end
        
        idx = A(:,3) == sID;
        scatter(A(idx,1), A(idx,2), 50, A(idx,4), shape, 'filled');
    end
    
    % Axis labels
    xlabel(xLabelStr, 'Interpreter', 'none');
    ylabel(yLabelStr, 'Interpreter', 'none');
    
    % Aesthetics
    colorbar;
    grid on;
    box on;
    
    % Save figure
    [~,~,ext] = fileparts(savePath);
    if any(strcmpi(ext, {'.pdf', '.png', '.jpg'}))
        saveas(gcf, savePath);
    else
        warning('Unrecognized file extension. Saving as .png');
        saveas(gcf, [savePath '.png']);
    end
    
    close;
end

