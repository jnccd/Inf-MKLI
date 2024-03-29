program predatorPrey
    
    CHARACTER(LEN=15) :: arg
    real(16), allocatable :: Predator(:), Prey(:), LastPredator(:), LastPrey(:), D(:,:), DiffPredator(:), DiffPrey(:)
    integer :: n, bigN, i, j, sum, t0 = 0, T = 45
    real(16) :: alpha = 2, beta = 3, gamma = 1, delta = 3, lambda = 1, mu = 1, constK = 0.001, delta_T, h, epsilon = 0.0001
    real(16), parameter :: PI_16 = 4 * atan (1.0_16)
    logical :: improvedEuler = .true.
    logical :: plot3D = .false.
    
    bigN = 100
    delta_T = 0.0001
    n = (T - t0) / delta_T
    h = 1.0 / Real(bigN)

    print *, n
    print *, h
    print *, (constK / (h*h)) 

    allocate(Predator(bigN))
    allocate(Prey(bigN))
    allocate(LastPredator(bigN))
    allocate(LastPrey(bigN))
    allocate(DiffPredator(n))
    allocate(DiffPrey(n))
    allocate(D(bigN, bigN))

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
    Predator = 1
    do i = 1, bigN
        Prey(i) = sin(i / Real(bigN) * PI_16)
    enddo

    if (plot3D) then
        open(unit = 30, file = 'outfile3D.txt', action = 'write')
        write(30,*) Predator
        write(30,*) Prey
        close(30)
    endif

    ! time loop
    do i = 2, n
        LastPredator = Predator
        LastPrey = Prey

        ! predator prey \w diffusion
        if (improvedEuler) then
            Predator = LastPredator + (delta_T / 2) * (MATMUL(D, Predator) + Predator * (delta * Prey - gamma - mu * Predator))
            Predator = LastPredator + delta_T * (MATMUL(D, Predator) + Predator * (delta * Prey - gamma - mu * Predator))

            Prey = LastPrey + (delta_T / 2) * (MATMUL(D, Prey) + Prey * (alpha - beta * Predator - lambda * Prey))
            Prey = LastPrey + delta_T * (MATMUL(D, Prey) + Prey * (alpha - beta * Predator - lambda * Prey))
        else 
            Predator = Predator + delta_T * (MATMUL(D, Predator) + Predator * (delta * Prey - gamma - mu * Predator))
            Prey = Prey + delta_T * (MATMUL(D, Prey) + Prey * (alpha - beta * Predator - lambda * Prey))
        endif

        if (plot3D) then
            open(unit = 30, file = 'outfile3D.txt', action = 'write', position='append')
            write(30,*) Predator
            write(30,*) Prey
            close(30)
        endif
        
        DiffPredator(i) = sqrt(sum((Predator - LastPredator)**2))
        DiffPrey(i) = sqrt(sum((Prey - LastPrey)**2))

        if (MODULO(i, 15) == 0) then
            print *, "Predator: ", DiffPredator(i), " Prey: ", DiffPrey(i)
        endif

        if (i >= 4 .and. DiffPredator(i) < epsilon .and. DiffPredator(i - 1) < epsilon) then
            print *, "DiffPredator too small after ", i, "steps"
            exit
        endif
        if (i >= 4 .and. DiffPrey(i) < epsilon .and. DiffPrey(i - 1) < epsilon) then
            print *, "DiffPrey too small after ", i, "steps"
            exit
        endif
    enddo
    
    ! write results
    open(unit = 20, file = 'outfile.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey
    close(20)

    deallocate(Predator)
    deallocate(Prey)
    deallocate(LastPredator)
    deallocate(LastPrey)
    deallocate(DiffPredator)
    deallocate(DiffPrey)
    deallocate(D)

end program predatorPrey
