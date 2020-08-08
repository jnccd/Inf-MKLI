program mkliMatMul

    CHARACTER(LEN=15) :: arg
    real(8), allocatable :: A(:,:), x(:), R(:)
    integer :: n, i, j, sum
    real :: t1, t2
    
    CALL get_command_argument(1, arg)
    read(arg , *) n
    
    allocate(A(n,n))
    allocate(x(n))
    allocate(R(n))
    
    call random_number(A)
    call random_number(x)
    R = 0
    
    call cpu_time(t1)
    
    do i = 1, n
        do j = 1, n
            R(i) = R(i) + A(j, i) * x(j)
        enddo
    enddo
    
    !R = MATMUL(A, x)
  
    call cpu_time(t2)
    
    write(*,*) 'cputime = ', t2-t1
    
end program mkliMatMul