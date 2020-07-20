module random
    implicit none
    contains
    
    ! Generate a random integer between a and b inclusive
    Function random_int(a, b)
        implicit none
        integer*8 :: random_int, a, b
        real*8 :: r
    
        call random_number(r)
        random_int = floor(a + (b-a+1) * r, 8)
        !return random_int
    End function random_int

    !=========================================================    
    ! Shuffle a given array x of length n
    ! using Fisher-Yates shuffle
    Subroutine random_shuffle(x, n)
        implicit none
        integer*8 :: i, j, n, temp, x(n)
        
        DO i = 1, n-1
            j = random_int(i, n)
            ! Swap x(i) and x(j)
            temp = x(i)
            x(i) = x(j)
            x(j) = temp
        ENDDO
    End Subroutine random_shuffle
    
    !=========================================================    
    Subroutine cdf_discrete_power_law(n, alpha, kmin, norm, cdf)
        implicit none
        integer*8 :: i, n, kmin
        real*8 :: alpha, norm, pdf(10**7), cdf(10**7)
        
        ! Calculate the CDF of the discrete power-law
        IF (kmin > 1) THEN
            pdf(1:kmin-1) = 0
        ENDIF

        cdf = 0
        DO i = kmin, 10**7
            pdf(i) = norm * real(i, 8)**(-alpha)
            cdf(i) = cdf(i-1) + pdf(i)
        ENDDO
    End subroutine cdf_discrete_power_law
    !=========================================================    
    Subroutine discrete_power_law(n, x, alpha, kmin, cdf)
        implicit none
        integer*8 :: i, j, n, kmin, x(n)
        real*8 :: r, alpha, cdf(10**7)

            x = 0
            Do j = 1, 10000
                call random_number(r)
                Do i = kmin, 10**7
                    IF (r < cdf(i))THEN
                        x(j) = i
                        EXIT
                    ENDIF
                Enddo
            Enddo
    End subroutine discrete_power_law
    !=========================================================    
    Subroutine cdf_poisson(ave, cdf)
        implicit none
        integer*8 :: i
        real*8 :: x, ave, pdf(0:int(ave*10)), cdf(0:int(ave*10))
    
        cdf = 0
        DO i = 0, int(ave*10)
            x = real(i, 8)
            pdf(i) = exp(i*log(ave)-ave-log(gamma(x+1)))
            IF (i == 0) THEN
                cdf(i) = pdf(i)
            ELSE
                cdf(i) = cdf(i-1) + pdf(i)
            ENDIF
        ENDDO    
    End subroutine cdf_poisson
    !=========================================================    
    Subroutine poisson(n, x, ave, cdf)
        implicit none
        integer*8 :: i, j, n, x(n)
        real*8 :: r, ave, cdf(0:int(ave*10))

        DO j = 1, n
            call random_number(r)
            Do i = 0, 100
                IF (r < cdf(i)) THEN
                    x(j) = i
                    EXIT            
                ENDIF
            Enddo
        ENDDO
    End subroutine poisson
end module random












