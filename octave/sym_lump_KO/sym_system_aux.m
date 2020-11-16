function dydt=sym_system_aux(t,y)


k=4;
kd=3;


dydt=zeros(4,1);

dydt(1) = kd*y(2) - 15* k*y(1) ; 
dydt(2) = 2*kd*y(3) + 15 * k*y(1) -(6*k+kd)*y(2) ;
dydt(3) = 3*kd*y(4) + 6 * k*y(2) - (k+2*kd)*y(3) ;
dydt(4) = k*y(3) - 3*kd*y(4); 



