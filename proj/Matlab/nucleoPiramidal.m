D = 256
L = log2(D) + 1
d = 1

files = strsplit(ls,' ');
m = length(files);
K = zeros(m);
for i=1:m
    X = csvread(files{i});
    s1 = sum(X);
    h = s1/sum(s1);
    H_x{1} = h;
    for k=1:L-1
        H_x{k+1} = sum(reshape(h, 2^k, (D/(2^k))^d));
    end    
    for j=i:m
        Y = csvread(files{j});        
        s1 = sum(Y);
        h = s1/sum(s1);
        H_y{1} = h;
        for k=1:L-1
            H_y{k+1} = sum(reshape(h, 2^k, (D/(2^k))^d));
        end
        K(i,j) = 0;
        for k=2:L
            K(i,j) = K(i,j) + (1/(2^k))*(sum(min([H_x{k}; H_y{k}])) - sum(min([H_x{k-1}; H_y{k-1}])));
        end
    end
end




