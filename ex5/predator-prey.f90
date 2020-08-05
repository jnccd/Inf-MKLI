program predatorPrey
    use mod_precision
    
    CHARACTER(LEN=15) :: arg
    real(kind=wp), allocatable :: Predator(:), Prey(:), D(:,:)
    integer :: steps, boxes, i, j, sum, t0 = 0, T = 15, cpuT1, cpuT2
    real(kind=wp) :: alpha = 2, beta = 3, gamma = 1, delta = 3, lambda = 2, mu = 2, constK = 0.001, dt, h

    namelist/ model_parameters/ alpha, beta, gamma, delta, lambda, mu
    namelist/ spatial_parameters/ kappaInverted, boxes
    namelist/ time_parameters/ tZero, T, dt
    
    open(unit = 22, file = 'predatorprey.nml', action = 'read')
    read(22, nml = model_parameters)
    read(22, nml = spatial_parameters)
    read(22, nml = time_parameters)
    close(22)

    constK = 1 / real(kappaInverted)
    t0 = tZero

    steps = (T - tZero) / dt
    h = 1.0 / Real(boxes)

    if (steps < 2 * boxes * boxes * constK * T) then
        print *, 'steps too low!'
        call EXIT(0)
    endif

    print *, 'n = ', steps
    print *, 'N = ', boxes
    print *, 't0 = ', tZero
    print *, 'T = ', T
    print *, 'dt = ', dt
    print *, 'kappa = ', constK
    print *, 'D mult = ', (constK / (h*h))
    print *, 'wp = ', wp

    call cpu_time(t1)

    allocate(Predator(boxes))
    allocate(Prey(boxes))
    allocate(D(boxes,boxes))

    D = 0
    do i = 1, boxes
        do j = 1, boxes
            if (i == j) then
                if (i == 1 .OR. i == boxes) then
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

    do i = 1, boxes
        Prey(i) = i / Real(boxes) * 0.1
        Predator(i) = i / Real(boxes) * 0.1 + 0.2
    enddo

    ! time loop
    do i = 1, steps
        ! predator prey \w diffusion
        Prey = Prey + dt * (MATMUL(D, Prey) + Prey * (alpha - beta * Predator - lambda * Prey))
        Predator = Predator + dt * (MATMUL(D, Predator) + Predator * (delta * Prey - gamma - mu * Predator))
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
