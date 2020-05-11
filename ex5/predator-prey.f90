program predatorPrey
    use mod_precision
    
    CHARACTER(LEN=15) :: arg
    real(kind=wp), allocatable :: Predator(:), Prey(:), D(:,:)
    integer :: n, bigN, i, j, sum, t0 = 0, T = 15, cpuT1, cpuT2
    real(kind=wp) :: alpha = 2, beta = 3, gamma = 1, delta = 3, lambda = 2, mu = 2, constK = 0.001, delta_T, h

    namelist/ model_parameters/ alpha, beta, gamma, delta, lambda, mu
    namelist/ spatial_parameters/ kappaInverted, bigN
    namelist/ time_parameters/ tZero, T, dt
    
    open(unit = 22, file = 'predatorprey.nml', action = 'read')
    read(22, nml = model_parameters)
    read(22, nml = spatial_parameters)
    read(22, nml = time_parameters)
    close(22)

    constK = 1 / real(kappaInverted)
    t0 = tZero

    ! 2. Task should be satisfied by this
    n = 4 * bigN*bigN * constK * T
    delta_t = 0.001
    h = 1.0 / Real(bigN)

    print *, 'n = ', n
    print *, 'h = ', h
    print *, 'kappa = ', constK
    print *, 'D mult = ', (constK / (h*h))
    print *, 'wp = ', wp

    call cpu_time(t1)

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
    
    open(unit = 21, file = 'outfileD.txt', action = 'write')
    write (21,*) D
    close(21)

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

    call cpu_time(t2)
    write(*,*) 'cputime = ', t2-t1
    
    ! write results
    open(unit = 20, file = 'outfile.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey
    close(20)

    deallocate(Predator)
    deallocate(Prey)
    deallocate(D)

end program predatorPrey
