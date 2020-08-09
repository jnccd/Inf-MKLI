program predatorPrey
    
    CHARACTER(LEN=15) :: arg
    real(8), allocatable :: Predator(:), Prey(:), D(:,:)
    integer :: n, bigN, i, j, sum, t0 = 0, T = 50
    real(8) :: alpha = 1, beta = 2.5, gamma = 1, delta = 1.7, lambda = 0.000002, mu = 0.000002, constK = 0.001, delta_T, h
    
    bigN = 100
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
        if (i == 1) then
            D(i,i) = -1
            D(i,i+1) = 1
        else if (i == bigN) then
            D(i,i) = -1
            D(i,i-1) = 1
        else
            D(i,i) = -2
            D(i,i+1) = 1
            D(i,i-1) = 1
        endif
    enddo
    D = (constK / (h*h)) * D
    
    ! Init box vectors
    do i = 1, bigN
        Predator(i) = i / Real(bigN) * 0.1 + 0.2
        Prey(i) = (bigN - i) / Real(bigN) * 0.3
    enddo

    open(unit = 30, file = 'outfile3D.txt', action = 'write')
    write(30,*) Predator
    write(30,*) Prey
    close(30)

    ! time loop
    do i = 1, n
        ! predator prey \w diffusion
        Predator = Predator + delta_T * (MATMUL(D, Predator) + Predator * (delta * Prey - gamma - mu * Predator))
        Prey = Prey + delta_T * (MATMUL(D, Prey) + Prey * (alpha - beta * Predator - lambda * Prey))
        
        open(unit = 30, file = 'outfile3D.txt', action = 'write', position='append')
        write(30,*) Predator
        write(30,*) Prey
        close(30)
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
