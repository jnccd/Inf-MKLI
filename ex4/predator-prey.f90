program predatorPrey
    
    CHARACTER(LEN=15) :: arg
    real(16), allocatable :: Predator(:), Prey(:), D(:,:)
    integer :: n, bigN, i, j, sum, t0 = 0, T = 5
    real(16) :: alpha = 2, beta = 3, gamma = 1, delta = 3, lambda = 2, mu = 2, constK = 0.001, delta_T, h
    
    bigN = 200
    n = 4 * bigN*bigN * constK * T
    delta_t = Real(T - t0) / n
    h = 1.0 / Real(bigN)

    print *, n
    print *, h
    print *, (constK / (h*h)) 

    allocate(Predator(bigN))
    allocate(Prey(bigN))
    allocate(D(bigN,bigN))

    D = 0
    do i = 1, bigN
        do j = 1, bigN
            if (i == j) then
                if (i == 1 .OR. i == bigN) then
                    D(i,j) = -1
                else
                    D(i,j) = -2
                endif
            else if (i - 1 == j .OR. j - 1 == i) then
                D(i,j) = 1
            endif
        enddo
    enddo
    D = (constK / (h*h)) * D
    
    ! Init box vectors
    Predator = 0.1
    Prey = 0.2

    do i = 1, bigN
        Prey(i) = i / Real(bigN) * 0.1
        Predator(i) = i / Real(bigN) * 0.1 + 0.2
    enddo

    ! time loop
    do i = 1, n
        ! predator prey \w diffusion
        Prey = Prey + delta_T * (MATMUL(D, Prey) + Prey * (alpha - beta * Predator - lambda * Prey))
        Predator = Predator + delta_T * (MATMUL(D, Predator) + Predator * (delta * Prey - gamma - mu * Predator))
    enddo
    
    ! write results
    open(unit = 20, file = 'outfile.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey
    close(20)

    deallocate(Predator)
    deallocate(Prey)
    deallocate(D)

end program predatorPrey
