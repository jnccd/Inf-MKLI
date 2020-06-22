program predatorPrey
    use mod_precision
    
    CHARACTER(LEN=15) :: arg
    real(kind=wp), allocatable :: Predator(:), Prey(:), LastPredator(:), LastPrey(:)
    integer :: n, bigN, i, j, sum, t0 = 0, T = 20, cpuT1, cpuT2
    real(kind=wp) :: alpha = 2, beta = 3, gamma = 1, delta = 3, lambda = 2, mu = 2, constK = 0.001, delta_T, h
    logical :: improvedEuler = .true.

    namelist/ model_parameters/ alpha, beta, gamma, delta, lambda, mu
    namelist/ spatial_parameters/ kappaInverted, bigN
    namelist/ time_parameters/ tZero, T, dt
    
    open(unit = 30, file = 'predatorprey.nml', action = 'read')
    read(30, nml = model_parameters)
    read(30, nml = spatial_parameters)
    read(30, nml = time_parameters)
    close(30)

    constK = 1 / real(kappaInverted)
    t0 = tZero
    delta_T = dt

    n = (T - t0) / delta_T

    print *, 'n = ', n
    print *, 'delta_t = ', delta_t
    print *, 'wp = ', wp

    allocate(Predator(bigN))
    allocate(Prey(bigN))
    allocate(LastPredator(bigN))
    allocate(LastPrey(bigN))

    ! Init box vectors
    Predator = 1
    do i = 1, bigN
        Prey(i) = sin(i / Real(bigN) * PI_16)
    enddo

    ! time loop
    do i = 2, n
        LastPredator = Predator
        LastPrey = Prey

        ! predator prey
        if (improvedEuler) then
            Predator = LastPredator + (delta_T / 2) * (Predator * (delta * Prey - gamma - mu * Predator))
            Predator = LastPredator + delta_T * (Predator * (delta * Prey - gamma - mu * Predator))

            Prey = LastPrey + (delta_T / 2) * (Prey * (alpha - beta * Predator - lambda * Prey))
            Prey = LastPrey + delta_T * (Prey * (alpha - beta * Predator - lambda * Prey))
        else 
            Predator = Predator + delta_T * (Predator * (delta * Prey - gamma - mu * Predator))
            Prey = Prey + delta_T * (Prey * (alpha - beta * Predator - lambda * Prey))
        endif
    enddo

    ! write results
    open(unit = 20, file = 'outfile.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey
    close(20)

    deallocate(Predator)
    deallocate(Prey)

end program predatorPrey
