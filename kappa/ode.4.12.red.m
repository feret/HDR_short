function main=main()
% command line:
%      'KaDE' '--count' 'Occurrences' 'ode.4.12.red.ka' '--output' 'ode.4.12.red''--output-plot''../data/ode.4.12.red.data'
%% THINGS THAT ARE KNOWN FROM KAPPA FILE AND KaSim OPTIONS:
%%
%% init - the initial abundances of each species and token
%% tinit - the initial simulation time (likely 0)
%% tend - the final simulation time
%% initialstep - initial time step at the beginning of numerical integration
%% maxstep - maximal time step for numerical integration
%% reltol - relative error tolerance;
%% abstol - absolute error tolerance;
%% period - the time period between points to return
%%
%% variables (init(i),y(i)) denote numbers occurrences
%% rule rates are corrected by the number of automorphisms in the lhs of rules


tinit=0;
tend=1;
initialstep=1e-05;
maxstep=0.02;
reltol=0.001;
abstol=0.001;
period=0.01;
nonnegative=false;

global nodevar
nodevar=3;
global max_stoc_coef
max_stoc_coef=0;
nvar=3;
nobs=3;
nrules=2;

global var
var=zeros(nvar,1);
global init
init=sparse(nodevar,1);
stoc=zeros(nrules,max_stoc_coef);
global k
global kd
global kun
global kdun
global stoc

k=zeros(nrules,1);
kd=zeros(nrules,1);
kun=zeros(nrules,1);
kdun=zeros(nrules,1);
global jacvar
jacvar=sparse(nvar,nodevar);
global jack
global jackd
global jackun
global jackund
global jacstoc

jack=zeros(nrules,nodevar);
jackd=zeros(nrules,nodevar);
jackun=zeros(nrules,nodevar);
jackund=zeros(nrules,nodevar);

t = 0.000000;

init(3)=t;
var(2)=init(2); % A.x-x
init(1)=6; % A(x[.])
var(1)=init(1); % A

k(1)=4; % A(x[.]), A(x[.]) -> A(x[1]), A(x[1])
k(2)=2; % A(x[1]), A(x[1]) -> A(x[.]), A(x[.])

uiIsOctave = false;
uiIsMatlab = false;
LIC = license('inuse');
for elem = 1:numel(LIC)
    envStr = LIC(elem).feature
    if strcmpi(envStr,'octave')
       LICname=envStr;
       uiIsOctave = true;
       break
    end
    if strcmpi(envStr,'matlab')
       LICname=envStr
       uiIsMatlab = true;
       break
    end
end


if nonnegative
   options = odeset('RelTol', reltol, ...
                    'AbsTol', abstol, ...
                    'InitialStep', initialstep, ...
                    'MaxStep', maxstep, ...
                    'Jacobian', @ode_jacobian, ...
                   'NonNegative', [1:1:2]);
else
   options = odeset('RelTol', reltol, ...
                    'AbsTol', abstol, ...
                    'InitialStep', initialstep, ...
                    'MaxStep', maxstep, ...
                    'Jacobian', @ode_jacobian);
end


if nonnegative
   if uiIsMatlab
      soln =  ode15s(@ode_aux,[tinit tend],ode_init(),options);
      soln.y=soln.y';
      vt = soln.x;
      vy = soln.y;
   elseif uiIsOctave
      [vt,vy] = ode23s(@ode_aux,[tinit tend],ode_init(),options);
   end
else
   if uiIsMatlab
      soln =  ode15s(@ode_aux,[tinit tend],ode_init(),options);
      soln.y=soln.y';
      vt = soln.x;
      vy = soln.y;
   elseif uiIsOctave
      soln = ode23(@ode_aux,[tinit tend],ode_init(),options);
      vt = soln.x;
      vy = soln.y;
   end
end;


nrows = length(vt);

tmp = zeros(nodevar,1);

n_points = floor ((tend-tinit)/period)+1;
t = linspace(tinit, tend, n_points);
obs = zeros(nrows,nobs);

for j=1:nrows
    for i=1:nodevar
        z(i)=vy(i,j);
    end
    h=ode_obs(z);
    for i=1:nobs
        obs(j,i)=h(i);
    end
end
if nobs==1
   y = interp1(vt, obs, t, 'pchip')';
else
   y = interp1(vt, obs, t, 'pchip');
end


filename = '../data/ode.4.12.red.data';
fid = fopen (filename,'w');
fprintf(fid,'# KaDE --count Occurrences ode.4.12.red.ka --output ode.4.12.red --output-plot ../data/ode.4.12.red.data\n')
fprintf(fid,'# ')
fprintf(fid,'[T]')
fprintf(fid,' A')
fprintf(fid,' |A.x-x|')
fprintf(fid,'\n')
for j=1:n_points
    for i=1:nobs
        fprintf(fid,'%f ',y(j,i));
    end
    fprintf(fid,'\n');
end
fclose(fid);


end



function Init=ode_init()

global nodevar
global init
Init=zeros(nodevar,1);

Init(1) = init(1); % A(x[.])
Init(2) = init(2); % A(x[1]), A(x[1])
Init(3) = init(3); % t
end


function dydt=ode_aux(t,y)

global nodevar
global max_stoc_coef
global var
global k
global kd
global kun
global kdun
global stoc

var(2)=y(2); % A.x-x
var(1)=y(1); % A


dydt=zeros(nodevar,1);

% rule    : A(x[1]), A(x[1]) -> A(x[.]), A(x[.])
% reaction: A(x[1]).A(x[1]) -> A(x[.]) + A(x[.])

dydt(2)=dydt(2)-1/2*k(2)*y(2);
dydt(1)=dydt(1)+1/2*k(2)*y(2);
dydt(1)=dydt(1)+1/2*k(2)*y(2);

% rule    : A(x[1]), A(x[1]) -> A(x[.]), A(x[.])
% reaction: A(x[1]).A(x[1]) -> A(x[.]) + A(x[.])

dydt(2)=dydt(2)-1/2*k(2)*y(2);
dydt(1)=dydt(1)+1/2*k(2)*y(2);
dydt(1)=dydt(1)+1/2*k(2)*y(2);

% rule    : A(x[.]), A(x[.]) -> A(x[1]), A(x[1])
% reaction: A(x[.]) + A(x[.]) -> A(x[1]).A(x[1])

dydt(1)=dydt(1)-1/2*k(1)*y(1)*y(1);
dydt(1)=dydt(1)-1/2*k(1)*y(1)*y(1);
dydt(2)=dydt(2)+1/2*k(1)*y(1)*y(1);
dydt(3)=1;

end


function jac=ode_jacobian(t,y)

global nodevar
global max_stoc_coef
global jacvar
global var
global k
global kd
global kun
global kdun
global stoc

global jack
global jackd
global jackun
global jackund
global jacstoc

var(2)=y(2); % A.x-x
var(1)=y(1); % A

jacvar(2,2)=2;
jacvar(1,1)=1;


jac=sparse(nodevar,nodevar);

% rule    : A(x[1]), A(x[1]) -> A(x[.]), A(x[.])
% reaction: A(x[1]).A(x[1]) -> A(x[.]) + A(x[.])

jac(2,2)=jac(2,2)-1/2*k(2);
jac(1,2)=jac(1,2)+1/2*k(2);
jac(1,2)=jac(1,2)+1/2*k(2);

% rule    : A(x[1]), A(x[1]) -> A(x[.]), A(x[.])
% reaction: A(x[1]).A(x[1]) -> A(x[.]) + A(x[.])

jac(2,2)=jac(2,2)-1/2*k(2);
jac(1,2)=jac(1,2)+1/2*k(2);
jac(1,2)=jac(1,2)+1/2*k(2);

% rule    : A(x[.]), A(x[.]) -> A(x[1]), A(x[1])
% reaction: A(x[.]) + A(x[.]) -> A(x[1]).A(x[1])

jac(1,1)=jac(1,1)-1/2*k(1)*y(1);
jac(1,1)=jac(1,1)-1/2*k(1)*y(1);
jac(1,1)=jac(1,1)-1/2*k(1)*y(1);
jac(1,1)=jac(1,1)-1/2*k(1)*y(1);
jac(2,1)=jac(2,1)+1/2*k(1)*y(1);
jac(2,1)=jac(2,1)+1/2*k(1)*y(1);
end


function obs=ode_obs(y)

global nobs
global var
obs=zeros(nobs,1);

t = y(3);
var(2)=y(2); % A.x-x
var(1)=y(1); % A

obs(1)=t; % [T]
obs(2)=var(1); % A
obs(3)=var(2); % |A.x-x|

end


main();
