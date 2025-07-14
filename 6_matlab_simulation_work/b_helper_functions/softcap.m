function y = softcap(x, z, s, method)
%SOFTCAP Apply a smooth soft cap to values in x, approaching asymptote z.
%
%   y = softcap(x, z, s) applies a soft cap using the 'rational' method.
%   y = softcap(x, z, s, method) allows alternative methods: 'rational', 'exp', 'tanh'
%
%   Inputs:
%     x      - input value(s), scalar or array
%     z      - soft cap target (scalar, > 0)
%     s      - softness parameter (> 0); higher means slower approach to z
%     method - (optional) 'rational' (default), 'exp', or 'tanh'
%
%   Output:
%     y - soft-capped version of x
%
%   Notes:
%     - The cap only affects values above z; values much smaller than z are returned ~unchanged.
%     - For symmetric soft-capping (both upper and lower bounds), combine two calls to softcap:
%         y = softcap(-softcap(-x, z, s), z, s)

    if nargin < 4
        method = 'rational';
    end
    if nargin < 3 || isempty(s)
        error('You must specify a softness parameter s > 0.');
    end
    if s <= 0
        error('Softness parameter s must be positive.');
    end
    if z <= 0
        error('Cap value z should be positive for expected behavior.');
    end

    switch lower(method)
        case 'rational'
            % z * x / (x + s)
            y = z .* x ./ (x + s);
        case 'exp'
            % z * (1 - exp(-x/s))
            y = z .* (1 - exp(-x ./ s));
        case 'tanh'
            % z * tanh(x/s)
            y = z .* tanh(x ./ s);
        otherwise
            error('Unknown method "%s". Use "rational", "exp", or "tanh".', method);
    end
end