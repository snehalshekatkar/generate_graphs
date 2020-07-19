Program er
    use random
    implicit none
    character (len = 100):: filename
    integer*8,  parameter :: n = 10000 ! number of vertices
    integer*8 :: i, j, k, ind, deg(n)
    real*8 :: ave_deg, r
    integer*8, allocatable :: stubs(:)
    real*8, allocatable :: pdf(:), cdf(:)

    ! average degree of the graph
    !ave_deg = 9.36019888210046 ! alpha = 2.2, kmin = 2
    !ave_deg = 5.492595543177584 ! alpha = 2.4, kmin = 2
    !ave_deg = 4.2090313191089885 ! alpha = 2.6, kmin = 2
    ave_deg = 3.57132550113324 ! alpha = 2.8, kmin = 2

    ! calculate the CDF of the Poisson distribution for given parameters
    allocate(cdf(0:int(ave_deg*10)))
    call cdf_poisson(ave_deg, cdf)

    ! Loop over realizations
    Do ind = 1, 10000

        print*, ind

        IF (ind < 10)THEN
            write(filename, '("Graphs/ER/alpha2.8_kmin2/deg",I1,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/ER/alpha2.8_kmin2/er",I1,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 100) THEN
            write(filename, '("Graphs/ER/alpha2.8_kmin2/deg",I2,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/ER/alpha2.8_kmin2/er",I2,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 1000) THEN
            write(filename, '("Graphs/ER/alpha2.8_kmin2/deg",I3,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/ER/alpha2.8_kmin2/er",I3,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 10000) THEN
            write(filename, '("Graphs/ER/alpha2.8_kmin2/deg",I4,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/ER/alpha2.8_kmin2/er",I4,".dat")')ind
            open(unit = 2, file = filename)
        ELSEIF (ind < 100000) THEN
            write(filename, '("Graphs/ER/alpha2.8_kmin2/deg",I5,".dat")')ind
            open(unit = 1, file = filename)
            write(filename, '("Graphs/ER/alpha2.8_kmin2/er",I5,".dat")')ind
            open(unit = 2, file = filename)
        ENDIF

        ! Generate the degree sequence
        deg = 0
        Do while (sum(deg) == 0 .or. mod(sum(deg), 2) == 1)
            call poisson(n, deg, ave_deg, cdf)
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
