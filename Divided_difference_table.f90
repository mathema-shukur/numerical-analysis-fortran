    program Difference_table
    INTEGER::i,j,n,fact,k
    REAL::x(10),y(10),d(10,10),xr(3),yr(3),p,s,h
    read (*,*)n,(xr(k),k=1,3)
    do i=1,n
        read (*,*)x(i),y(i)
    end do
    do i=1,n-1
        d(i,1)=y(i+1)-y(i)
    end do
    do j=2,n-1
        do i=1,n-j
            d(i,j)=d(i+1,j-1)-d(i,j-1)
        end do
    end do
    write (*,*)'difference table::-'
    do i=1,n
        write (*,13)x(i),y(i),(d(i,j),j=1,n-i)
    end do
13  format (/,2x,f12.3,2x,f12.3,10(2x,f12.3))
    do k=1,3
        yr(k)=y(1)
        h=x(2)-x(1)
        p=(xr(k)-x(1))/h
        s=p
        do i=1,n-1
            yr(k)=yr(k)+s*d(1,i)/fact(i)
            s=s*(p-i)
        end do
    end do
    do k=1,3
        write (*,14)xr(k),yr(k)
    end do
14  format (2x,'value of y(x) at x=',f12.3,1x,'is',1x,f12.3)
    end program
    INTEGER function fact(l)
    fact=1
    do i=1,l
        fact=fact*i
    end do
    return
    end
