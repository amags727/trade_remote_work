function unpack_struct(s)
    f = fieldnames(s);
    for i = 1:numel(f)
        assignin('caller', f{i}, s.(f{i}));
    end
end