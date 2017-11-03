%% Tutorial 1 : Basic Operations %%%

1 ~= 2
1 && 2
1 || 2
xor(1,1);   % semi colon supresses o/p

% variables
a = pi
disp(a)
disp(sprintf('A=%0.2f', a))

% vectors
V = [1, 2, 3]
V = 1 : 0.1 : 2
V = 1 : 6

% MATRIX
M = [1, 2; 3, 4; 5, 6]
V = [1; 2; 3]
ONES = ones(4,3)    % also for zeros()
RAND = rand(1,3)    % 1x3 matrix of random numbers from uniform distribution
I = eye(4)          % 4x4 Identity Matrix

help eye



%% Tutorial 2 : Moving Data Around %%%
size(M)
size(M,1)
length(V)

ls
% load <file_name>
% load('<file_name>')

who     % variables in workspace
whos    % more detailed info

first_two = V(1:2)
% save <file_name> variable

M(2, 2)
M(2, :)
M([1 3], :)

M(:,2) = [1, 1, 1]      % updating a column. NOTE ';' means goto next line.
                        % [A B] is same as [A, B]
M = [ M [2; 2; 2] ]     % adding column
M = [ M; [4 4 4]]       % adding row

M(:)                    % getting all elements



%%% Tutorial 3 : Computing on Data %%%

% M * N  : Matrix Multiplication
% M .* N : Matrix element-wise operation
% M .^ 2 : Element wise squaring
M'      %: Matrix Transpose
[rows, cols] = find(M>3)
max(M, [], 1) % Column-wise maximum
max(M, [], 2) % Row-wise maximum
max(M)        % Also the same
pinv(M)       % Matrix Inverse

[val, index] = max(V)  %: Maximum Value and its index
V < 3
find(V<3)
sum(V)
prod(V)



%% Tutorial 4 : Plotting Data %%%
x = [ 0.01 : 0.001 : 0.99 ]
plot(x, sin(2*x*pi*4))
xlabel('X-axis')
title('Plot')
% print -dpng 'file_name.png'
% subplot(1, 2, 1) % 1x2 grid for plotting. Selecting 1 for further operations



%% Tutorial 5: Control Statements %%%
for i=1:10,
    disp(i);
end

i = 1;
while i <= 10,
    disp(i);
    i = i + 1;
    if i >= 5,
        break;
    end;
end

disp(f(2))
function A = f(a)
    A = a^2;
end



%% Tutorial 6 : Vectorization %%
% Vectorize as much as possible. Its efficient.
