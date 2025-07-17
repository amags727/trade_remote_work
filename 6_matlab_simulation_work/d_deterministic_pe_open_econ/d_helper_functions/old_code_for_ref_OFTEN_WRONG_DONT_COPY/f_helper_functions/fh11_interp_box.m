function [indices, weights] = fh11_interp_box(state_t, state_space, p)
    % state_t: 1 x d
    % state_space: N x d (assumed structured grid)
    % p: power for inverse-distance weighting (default 2)

    if nargin < 3
        p = 2;
    end

    d = size(state_space, 2);
    box_vals = cell(1, d);

    for j = 1:d
        grid_vals = unique(state_space(:, j));
        below = grid_vals(grid_vals <= state_t(j));
        above = grid_vals(grid_vals >= state_t(j));

        if isempty(below)
            box_vals{j} = above(1);
        elseif isempty(above)
            box_vals{j} = below(end);
        else
            lb = below(end);
            ub = above(1);
            if lb == ub
                box_vals{j} = lb;
            else
                box_vals{j} = [lb, ub];
            end
        end
    end

    % Cartesian product of all enclosing grid values
    [gridpts{1:d}] = ndgrid(box_vals{:});
    box_points = cellfun(@(x) x(:), gridpts, 'UniformOutput', false);
    box_mat = [box_points{:}];  % (up to 2^d) x d

    % Find corresponding rows in state_space
    [~, indices] = ismember(box_mat, state_space, 'rows');
    valid = indices > 0;
    indices = indices(valid);
    box_mat = box_mat(valid, :);

    % Compute distances and weights
    diffs = box_mat - state_t;
    dists = sqrt(sum(diffs.^2, 2));

    % Handle exact match
    if any(dists == 0)
        weights = zeros(size(dists));
        weights(dists == 0) = 1;
    else
        unnorm_w = 1 ./ (dists.^p);
        weights = unnorm_w / sum(unnorm_w);
    end
    if isscalar(indices)
        indices = [indices;1];
        weights = [weights;0];
    end
end