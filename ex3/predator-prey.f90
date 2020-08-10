program predatorPrey
    
    CHARACTER(LEN=15) :: arg
    real(16), allocatable :: Predator(:), Prey(:)
    integer :: n, i, j, sum, t0 = 0, T = 25
    real(16) :: t1, t2, alpha = 1.2, beta = 1.2, gamma = 1.1, delta = 2.1, lambda = 0.1, mu = 0.1, delta_T
    
    CALL get_command_argument(1, arg)
    read(arg , *) n

    delta_t = Real(T - t0) / n

    print *, n
    print *, delta_T

    ! Init Vectors
    allocate(Predator(n))
    allocate(Prey(n))

    Predator(1) = 1
    Prey(1) = 2

    ! explicit euler loop
    do i = 2, n
        Prey(i) = Prey(i-1) + delta_T * (Prey(i-1) * (alpha - beta * Predator(i-1) - lambda * Prey(i-1)))
        Predator(i) = Predator(i-1) + delta_T * (Predator(i-1) * (delta * Prey(i-1) - gamma - mu * Predator(i-1)))
    enddo
    
    ! write output
    open(unit = 20, file = 'pp.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey
    close(20)

    deallocate(Predator)
    deallocate(Prey)

end program predatorPrey
