program predatorPrey
    use mod_precision
    use omp_lib
    
    CHARACTER(LEN=15) :: arg
    real(kind=wp), allocatable :: Predator(:), Prey(:), D(:,:), tmp(:)
    integer :: n, bigN, i, j, sum, t0 = 0, T = 15
    real(kind=wp) :: alpha = 2, beta = 3, gamma = 1, delta = 3, lambda = 2, mu = 2, constK = 0.001, delta_T, h, dt
    real :: cpuT1, cpuT2, t1, t2
    INTEGER*8 :: st1, st2
    logical :: plot3D = .false.

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

    n = (T - tZero) / dt
    delta_t = dt
    h = 1.0 / Real(bigN)

    print *, 'n = ', n
    print *, 'h = ', h
    print *, 'kappa = ', constK
    print *, 'D mult = ', (constK / (h*h))
    print *, 'wp = ', wp

    allocate(Predator(bigN))
    allocate(Prey(bigN))
    allocate(tmp(bigN))
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
    Predator = 0.1
    Prey = 0.2
    do i = 1, bigN
        Prey(i) = i / Real(bigN) * 0.1
        Predator(i) = i / Real(bigN) * 0.1 + 0.2
    enddo

    if (plot3D) then
        open(unit = 30, file = 'outfile3D.txt', action = 'write')
        write(30,*) Predator
        write(30,*) Prey
        close(30)
    endif

    call cpu_time(cpuT1)
    t1 = omp_get_wtime()
    call system_clock(st1)

    ! time loop, predator prey \w diffusion
    do i = 1, n
        ! tmp = MATMUL(D, Prey)
        tmp = 0
        !$OMP PARALLEL DO num_threads(4)
        do j = 1, bigN
            do k = 1, bigN
                tmp(j) = tmp(j) + D(j, k) * Prey(j)
            enddo
        enddo
        !$OMP END PARALLEL DO

        ! Prey Update
        !$OMP PARALLEL DO num_threads(4)
            do j = 1, bigN
                Prey(j) = Prey(j) + delta_T * (tmp(j) + Prey(j) * (alpha - beta * Predator(j) - lambda * Prey(j)))
            end do
        !$OMP END PARALLEL DO
        
        ! tmp = MATMUL(D, Predator)
        tmp = 0
        !$OMP PARALLEL DO num_threads(4)
        do j = 1, bigN
            do k = 1, bigN
                tmp(j) = tmp(j) + D(j, k) * Predator(j)
            enddo
        enddo
        !$OMP END PARALLEL DO

        ! Predator Update
        !$OMP PARALLEL DO num_threads(4)
            do j = 1, bigN
                Predator(j) = Predator(j) + delta_T * (tmp(j) + Predator(j) * (delta * Prey(j) - gamma - mu * Predator(j)))
            end do
        !$OMP END PARALLEL DO

        if (plot3D) then
            open(unit = 30, file = 'outfile3D.txt', action = 'write', position='append')
            write(30,*) Predator
            write(30,*) Prey
            close(30)
        endif
    enddo

    call system_clock(st2)
    t2 = omp_get_wtime()
    call cpu_time(cpuT2)
    write(*,*) 'omp_get_wtime = ', t2-t1
    write(*,*) 'cputime = ', cpuT2-cpuT1
    write(*,*) 'time = ', st2-st1
    
    ! write results
    open(unit = 20, file = 'outfile.txt', action = 'write')
    write(20,*) Predator
    write(20,*) Prey
    close(20)

    deallocate(Predator)
    deallocate(Prey)
    deallocate(tmp)
    deallocate(D)

end program predatorPrey
