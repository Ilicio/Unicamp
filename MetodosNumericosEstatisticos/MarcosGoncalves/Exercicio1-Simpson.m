%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% METODO DO SIMPSON
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% função
f=@(x) exp(-x)./x;

% número de trapézios
M=10; % Número par

% intervalo inicial
x1=1;

% intervalo final
x2=2;

integral=simpsons(f,x1,x2,M)
function I = simpsons(f,a,b,n)
h=(b-a)/n; xi=a:h:b;
 I= h/3*(f(xi(1))+2*sum(f(xi(3:2:end-2)))+4*sum(f(xi(2:2:end)))+f(xi(end)));
end