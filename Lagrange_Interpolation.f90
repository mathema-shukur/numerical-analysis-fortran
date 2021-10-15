    program Lagrange_interpolation
    INTEGER::i,j,n
    REAL::x(10),y(10),xr,yr,p
    read (*,*)n,xr
    do i=1,n
        read (*,*)x(i),y(i)
    end do
    do i=1,n
        write (*,*)x(i),y(i)
    end do
    yr=0.0
    do i=1,n
        p=1
        do j=1,n
            if (j.ne.i) then
                p=p*(xr-x(j))/(x(i)-x(j))
            end if
        end do
        yr=yr+p*y(i)
    end do
    write (*,*)'desired value=',yr
    end program
