Program sf
    use random
    implicit none
    character (len = 90) :: filename
    integer*8, parameter :: n = 10000
    integer*8 :: i, j, k, ind, kmin, deg(n)
    integer*8, allocatable :: stubs(:)
    real*8 :: alpha, norm, r
    real*8 :: pdf(10**7), cdf(10**7)

    ! Parameters
    !alpha = 2.2
    !alpha = 2.4
    !alpha = 2.6
    alpha = 2.8
    kmin = 2
    ! The following normalization constants are obtained by calculating 
    ! 1./(zeta(alpha)-\sum\limits_{k=1}^{kmin-1}(k^{-alpha}))
    ! We use zeta from scipy.special for this since series convergence is 
    ! slow for direct computation
    !norm = 2.0385562062781037 ! alpha = 2.2, kmin = 2
    !norm = 2.608630829915787 ! alpha = 2.4, kmin = 2
    !norm = 3.2735602073201586 ! alpha = 2.6, kmin = 2
    norm = 4.048068017499963 ! alpha = 2.8, kmin = 2

    ! Calculate the CDF of the discrete power-law
    IF (kmin > 1) THEN
        pdf(1:kmin-1) = 0
    ENDIF

    ! Get the CDF of the discrete power-law with given parameters
    call cdf_discrete_power_law(n, alpha, kmin, norm, cdf)

    ! Loop over realizations
    DO ind = 1, 10000
        print*, ind
        ! Open the output files
        IF (ind < 10) THEN
            write(filename, '("Graphs/SF/alpha2.8_kmin2/deg",I1,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/SF/alpha2.8_kmin2/sf",I1,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 100) THEN
            write(filename, '("Graphs/SF/alpha2.8_kmin2/deg",I2,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/SF/alpha2.8_kmin2/sf",I2,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 1000) THEN
            write(filename, '("Graphs/SF/alpha2.8_kmin2/deg",I3,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/SF/alpha2.8_kmin2/sf",I3,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 10000) THEN
            write(filename, '("Graphs/SF/alpha2.8_kmin2/deg",I4,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/SF/alpha2.8_kmin2/sf",I4,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 100000) THEN
            write(filename, '("Graphs/SF/alpha2.8_kmin2/deg",I5,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/SF/alpha2.8_kmin2/sf",I5,".dat")')ind
            open(unit = 2, file = filename)
        ENDIF

        ! Generate the degree sequence
        deg = 0
        Do while (sum(deg) == 0 .or. mod(sum(deg), 2) == 1)
            call discrete_power_law(n, deg, alpha, kmin, cdf)
        Enddo
        Do i = 1, n
            write(1, *)deg(i)
        Enddo
        ! Create the stubs
        allocate(stubs(sum(deg)))
        k = 0
        DO i = 1, n
            Do j = 1, deg(i)
                k = k + 1
                stubs(k) = i
            Enddo
        ENDDO

        ! Randomly connect stubs with each other to generate the graph
        call random_shuffle(stubs, sum(deg))
        Do i = 1, sum(deg)-1, 2
            write(2, *) stubs(i), stubs(i+1)
        Enddo
        deallocate(stubs)
        close(1)
        close(2)
    ENDDO
    
End program 
!==============================================================

