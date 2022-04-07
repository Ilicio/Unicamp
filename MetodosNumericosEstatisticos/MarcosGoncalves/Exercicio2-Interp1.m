% valores de x
x=[-2 0 2];

% valores de F(x)
y=[18 4 6];

% valores para interpolação
xq=[-1.8 -1 -0.5 0 0.5 1 1.8];

% função interp1
interp = interp1(x,y,xq,'pchip');

% gráfico da interpolação
plot(interp);

