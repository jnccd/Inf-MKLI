program predatorPrey
    
    CHARACTER(LEN=15) :: arg
    real(16), allocatable :: Predator(:), Prey(:)
    integer :: n, i, j, sum, t0 = 0, T = 50
    real(16) :: t1, t2, alpha = 1.2, beta = 1.2, gamma = 1.1, delta = 2.1, half_life = 0.1, mu = 0.1, delta_T
    
    CALL get_command_argument(1, arg)
    read(arg , *) n

    delta_t = Real(T - t0) / n

    print *, n
    print *, delta_T

    allocate(Predator(n))
    allocate(Prey(n))

    Predator(1) = 1
    Prey(1) = 2

    do i = 2, n
        Predator(i) = Predator(i - 1) + delta_T * Predator(i - 1) * (alpha - beta * Prey(i - 1) - half_life * Predator(i - 1))
        
        Prey(i) = Prey(i - 1) + delta_T * Prey(i - 1) * (delta * Predator(i - 1) - gamma - mu * Prey(i - 1))
    enddo
    
    open(unit = 20, file = 'pp.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey

    close(20)
end program predatorPrey