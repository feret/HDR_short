function dydt=sym_system_aux(t,y)


kxx=1;
kxy=4;
kyy=2;
kdxx=2;
kdxy=2;
kdyy=2;


dydt=zeros(20,1);

dydt(1)= kdxx*y(2) + kdyy*y(3) + kdxy*y(4) - 15*(kxx+kyy+kxy)*y(1);
dydt(2)= 15*kxx*y(1) + 2*kdxx*y(5) + kdyy*y(8) + kdxy*y(10) - (kdxx+6*(kxx+kyy+kxy))*y(2);
dydt(3)= 15*kyy*y(1) + 2*kdyy*y(6) + kdxx*y(8) + kdxy*y(9) - (kdyy+6*(kxx+kyy+kxy))*y(3);
dydt(4)= 15*kxy*y(1) + 2*kdxy*y(7) + kdxx*y(10) + kdyy*y(9) - (kdxy+6*(kxx+kyy+kxy))*y(4);
dydt(5)= 6*kxx*y(2) + 3*kdxx*y(11) + kdyy*y(14) + kdxy*y(15) - (2*kdxx+kxx+kxy+kyy)*y(5);
dydt(6)= 6*kyy*y(3) + 3*kdyy*y(12) + kdxx*y(16) + kdxy*y(17) - (2*kdyy+kxx+kxy+kyy)*y(6);
dydt(7)= 6*kxy*y(4) + 3*kdxy*y(13) + kdxx*y(19) + kdyy*y(18) - (2*kdxy+kxx+kxy+kyy)*y(7);
dydt(8)= 6*kxx*y(3) + 6*kyy*y(2) + 2*kdxx*y(14) + 2*kdyy*y(16) + kdxy*y(20) -(kxx+kyy+kxy+kdxx+kdyy)*y(8);
dydt(9)=  6*kxy*y(3) + 6*kyy*y(4) + 2*kdxy*y(18) + 2*kdyy*y(17) + kdxx*y(20)-(kxx+kyy+kxy+kdyy+kdxy)*y(9);
dydt(10)= 6*kxy*y(2) + 6*kxx*y(4) + 2*kdxy*y(19) + 2*kdxx*y(15) + kdyy*y(20)-(kxx+kyy+kxy+kdxx+kdxy)*y(10);
dydt(11)= kxx*y(5) - 3*kdxx*y(11);
dydt(12)= kyy*y(6) - 3*kdyy*y(12);
dydt(13)= kxy*y(7) - 3*kdxy*y(13); 
dydt(14)=kxx*y(8) + kyy*y(5) -(2*kdxx+kdyy)*y(14);
dydt(15)=kxx*y(10) + kxy*y(5)-(2*kdxx+kdxy)*y(15);
dydt(16)=kyy*y(8)  + kxx*y(6)-(2*kdyy+kdxx)*y(16);
dydt(17)=kyy*y(9) + kxy*y(6)    -(2*kdyy+kdxy)*y(17);
dydt(18)=kxy*y(9) + kyy*y(7)    -(2*kdxy+kdyy)*y(18);
dydt(19)=kxy*y(10) + kxx*y(7)    -(2*kdxy+kdxx)*y(19);
dydt(20)= kxx*y(9) + kxy*y(8) + kyy*y(10) - (kdxx+kdxy+kdyy)*y(20);



