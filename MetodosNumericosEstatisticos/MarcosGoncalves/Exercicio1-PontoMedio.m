%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% METODO DO PONTO MÉDIO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% calculando valor do intervalo com 5 discretizações
h=(2-1)/5;

% calculando função exponencial e somando todos os termos
I=0.2*((exp(-1.1)/1.1)+(exp(-1.3)/1.3)+(exp(-1.5)/1.5)+(exp(-1.7)/1.7)+(exp(-1.9)/1.9));


% calculando valor do intervalo com 10 discretizações
h=(2-1)/10;

% calculando função exponencial e somando todos os termos
I=0.1*((exp(-1.05)/1.05)+(exp(-1.15)/1.15)+(exp(-1.25)/1.25)+(exp(-1.35)/1.35)+(exp(-1.45)/1.45)+(exp(-1.55)/1.55)+(exp(-1.65)/1.65)+(exp(-1.75)/1.75)+(exp(-1.85)/1.85)+(exp(-1.95)/1.95));
