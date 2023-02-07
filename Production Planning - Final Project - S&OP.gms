$title ProductionPlanningFinalProject
sets
       t        periods        /t0,t1,t2,t3,t4,t5,t6/
       n        products       /n1,n2,n3,n4,n5/
       j        shifts         /j1,j2,j3/
;

Scalars
         m Aggregated amount produced in a single shift in a period    /2400000/
         e Hiring and Training cost                                    /24000/
         e_prime Firing cost                                           /120000/
         lambda Increase in production cost (per kg)                   /10000/
         omega  Decrease in production cost (per kg)                   /15000/
         s Workers payment                                             /50000000/
         w fixed number of workers                                     /24/
         I_max capacity of warehouse                                   /1000000/
         B_max max amount of subcontract                               /300000/
;

table D(t,n) demand for product n in period t
         n1       n2       n3       n4       n5
t1       150819   1809827  754094   1357370  955186
t2       166374   1996482  831868   1497362  1053699
t3       181928   2183138  909641   1637353  1152212
t4       197483   2369794  987414   1777345  1250724
t5       213037   2556450  1065187  1917337  1349237
t6       228592   2743105  1142961  2057329  1447750
;

Parameters
c(n)  cost of producing product n (per kg)
                        /n1  171000,
                         n2  168000,
                         n3  160000,
                         n4  155000,
                         n5  147000/

h(n)  holding cost of product n
                        /n1  51300,
                         n2  50400,
                         n3  48000,
                         n4  46500,
                         n5  44100/

pi(n) shortage cost of product n
                        /n1  56430,
                         n2  55440,
                         n3  52800,
                         n4  51150,
                         n5  48510/

b_cost(n) subcontract cost of product n
                        /n1  239400,
                         n2  235200,
                         n3  224000,
                         n4  217000,
                         n5  205800/
;

variable
         Z
;

integer variables
         X(t,n,j),
         I_plus(t,n),
         I_minus(t,n),
         I(t,n),
         WT(t),
         WT_plus(t),
         WT_minus(t),
         delta_plus(t,n),
         delta_minus(t,n),
         B(t,n);
         X.up(t,n,j)=3000000;
         I_plus.up(t,n)=3000000;
         I.up(t,n)=30000000;
         I_minus.up(t,n)=3000000;
         WT.up(t)=72;
         WT_plus.up(t)=72;
         WT_minus.up(t)=72;
         delta_plus.up(t,n)=3000000;
         delta_minus.up(t,n)=3000000;
         B.up(t,n)=300000;
         I.fx('t6',n)=0;
         X.fx('t0',n,j)=0;
         I.fx('t0',n)=0;
         I_plus.fx('t0',n)=0;
         I_minus.fx('t0',n)=0;
         WT_plus.fx('t0')=0;
         WT_minus.fx('t0')=0;
         WT.fx('t0')=w;
         delta_plus.fx('t0',n)=0;
         delta_minus.fx('t0',n)=0;
         B.fx('t0',n)=0
;



binary variables
         y(t,j);
         y.fx('t0','j1')=1;
         y.fx('t0','j2')=0;
         y.fx('t0','j3')=0
;

equations
         obj              objective function
         InvProdBal(t,n)  Balance of inventory Production
         InvBal(t,n)      Balance of inventory
         ProdBal(t,n)     Balance of Production
         WorkerBal(t)     Balance of total workers
         ShiftProdU(t,j)  Upper bound of shift production
         ShiftProdL(t,j)  Lower bound of shift production
         ShiftProd3L(t)   Lower bound of third shift production
         TotalWorker(t)   Total Workers in period t
         SubCont(t,n)     Sub contract product
         Warehouse(t)     Capacity of warehouse



;

obj               ..   Z =e= SUM(t,SUM(n,c(n)*SUM(j,X(t,n,j)) + h(n)*I_plus(t,n) + pi(n)*I_minus(t,n) + lambda*delta_plus(t,n) + omega*delta_minus(t,n) + b_cost(n)*B(t,n)) + s*WT(t) + e*WT_plus(t) + e_prime*WT_minus(t));
InvProdBal(t,n)   ..   I(t,n) =e= I(t-1,n)$(ord(t)<>1) + B(t,n) + SUM(j,X(t,n,j)) - D(t,n);
InvBal(t,n)       ..   I(t,n) =e= I_plus(t,n) - I_minus(t,n);
ProdBal(t,n)      ..   SUM(j,X(t,n,j)) =e= SUM(j,X(t-1,n,j))$(ord(t)<>1) + delta_plus(t,n) - delta_minus(t,n);
WorkerBal(t)      ..   WT(t)$(ord(t)<>1) =e= WT(t-1) + WT_plus(t) - WT_minus(t);
ShiftProdU(t,j)   ..   SUM(n,X(t,n,j)) =l= m*y(t,j);
ShiftProdL(t,j)   ..   SUM(n,X(t,n,j)) =g= m*y(t,j+1);
ShiftProd3L(t)    ..   SUM(n,X(t,n,'j3')) =g= 0;
TotalWorker(t)    ..   WT(t) =e= w*(SUM(j,y(t,j)));
SubCont(t,n)      ..   B(t,n) =l= B_max;
Warehouse(t)      ..   SUM(n,I(t,n)) =l= I_max;


Model ProductionPlanningFinalProject /All/;
option limrow=100 , limcol=100 , optca=0 , optcr=0;
Solve ProductionPlanningFinalProject Using MIP minimizing Z;
Display Z.l,X.l,I.l,y.l,B.l,WT.l;
