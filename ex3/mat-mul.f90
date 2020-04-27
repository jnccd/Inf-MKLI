program matMul

    CHARACTER(LEN=15) :: arg
    real(8), allocatable :: A(:,:), x(:), R(:,:)
    integer :: n, i, j, sum
    real :: t1, t2
    
    CALL get_command_argument(1, arg)
    read(arg , *) n
    
    allocate(A(n,n))
    allocate(x(n))
    allocate(R(n,n))
    
    call random_number(A)
    call random_number(x)
    R = 0
    
    call cpu_time(t1)
    
    R = MATMUL(A, A)
  
    call cpu_time(t2)
    
    !do i=1,size(A,1)
    !    write(*,'(20G12.4)') A(i,:)
    !end do 
    
    write(*,*) 'cputime = ', t2-t1
    
end program matMul