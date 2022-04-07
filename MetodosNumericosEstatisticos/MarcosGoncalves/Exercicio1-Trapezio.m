%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% METODO DO TRAPEZIO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% função
f = @(x) exp(-x)./x;

% número de trapézios
N = 10;

% intervalo inicial
a = 1;

% intervalo final
b = 2;

h = (b-a)/N;

sum = 0;
for i=1 : N-1
    sum = sum + f(a+i*h);
end
TR = (h/2)*(f(a) + 2*sum + f(b));
