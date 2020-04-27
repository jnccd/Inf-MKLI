program matMul
    real(8), allocatable :: A(:,:), x(:)
    integer :: n
    n = ...
    allocate(A(n,n))
    call random_number(A)
    
end program matMul