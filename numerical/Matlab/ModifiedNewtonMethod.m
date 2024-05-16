function r = newton_mod(f,fd,x,m,xtol,ftol,nmax, display)
n = 0;
fx = f(x);

if display 
   disp('   n             x                    f(x)')
   disp('------------------------------------------------------')
   fprintf('%4d %23.15e %23.15e\n', n, x, fx)
end
if abs(fx) <= ftol, r = x; return, end

for n = 1: nmax
    fdx = fd(x);
    d = fx/fdx;
    x = x - m*d;
    fx = f(x);
    if display 
       fprintf('%4d %23.15e %23.15e\n', n, x, fx), end
    if abs(d) <= xtol || abs(fx) <= ftol
        r = x;
        return
    end
end
r = x;
end


