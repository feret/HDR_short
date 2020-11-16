% THINGS THAT ARE KNOWN FROM KAPPA FILE AND COMPLX OPTIONS;
%
% init - the initial abundances of each fragment
% tinit - the initial simulation time (likely 0)
% tend - the final simulation time
% initialstep - initial time step at the beginning of numerical integration
% num_t_point - the number of time points to return

tinit=0;
tend=6;
initialstep=1e-06;
num_t_point=1000;

options = odeset('RelTol', 1e-3, ...
                 'AbsTol', 1e-3, ...
                 'InitialStep', initialstep, ...
                 'MaxStep', tend);

uiIsOctave = false;
uiIsMatlab = false;
LIC = license('inuse');
for elem = 1:numel(LIC)
   envStr = LIC(elem).feature;
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
if uiIsMatlab
   soln =  ode15s(@sym_system_aux,[tinit tend],sym_system_init(),options);
soln.y=soln.y';
elseif uiIsOctave
   soln = ode23(@sym_system_aux,[tinit tend],sym_system_init(),options);
end
t = linspace(tinit, tend, num_t_point+1);
nrows = length(soln.x);
nobs = 4;
nfragments = 4;
tmp = zeros(nfragments,1);
obs = zeros (nrows,nobs);

for j=1:nrows
   for i=1:nfragments
      z(i)=soln.y(i,j);
   end
   for i=1:4
      obs(j,i)=z(i);
  end
end

if nobs==1
   y = interp1(soln.x, obs, t, 'pchip')';
else   y = interp1(soln.x, obs, t, 'pchip');
end


filename = 'sym.data';
fid = fopen (filename,'w');
for j=1:num_t_point+1
    fprintf(fid,'%f',t(j));
    for i=1:nobs
      fprintf(fid,' %f',y(j,i));
    end
    fprintf(fid,'\n');
end
fclose(fid);
filename = 'sym.xml';
fid = fopen (filename,'w');
fprintf(fid,'<?xml version=''1.0'' encoding=''utf-8''?>\n<!-- Automaticaly generated by Complx 4.329..10578-->\n');
fprintf(fid,'<!-- and postprocessed by %s -->\n',LICname);
fprintf(fid,'<ComplxSession Timestamp="07/10/2014 (11:10:50)" CommandLine="complx --final-time 6 --do-ODE --output-scheme egfr_frag --output-ODE-matlab essai.m egfr-compressed.ka" InputFile="egfr-compressed.ka" \nxsi:schemaLocation=\"http://plectix.synthesisstudios.com\nKappaSession.xsd\"\nxmlns=\"http://plectix.synthesisstudios.com/schemas/kappasession\"\nxmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" >\n');
fprintf(fid,'<Simulation TotalTime="%f" InitTime="%f">\n',tend,tinit);
fprintf(fid,'<Plot Type="OBSERVABLE" Text="%s"/>\n','[EGFR(Y48!0),SHC(Y7!1,pi!0),GRB2(a!1,b!2),SOS(d!2)]');
fprintf(fid,'<Plot Type="OBSERVABLE" Text="%s"/>\n','[EGFR(Y68!0),GRB2(a!0,b!1),SOS(d!1)]');
fprintf(fid,'<CSV>\n<![CDATA[\n');
for j=1:num_t_point+1
    fprintf(fid,'%f ',t(j));
    for i=1:nobs-1
      fprintf(fid,' %f ',y(j,i));
    end
    fprintf(fid,' %f',y(j,nobs));
    fprintf(fid,'\n');
end
fprintf(fid,']]>\n</CSV>\n</Simulation>\n');
fprintf(fid,'</ComplxSession>\n');
fclose(fid);
