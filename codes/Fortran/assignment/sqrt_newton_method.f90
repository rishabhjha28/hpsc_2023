program sqrt_newton_method
    implicit none
    
    real :: prevVal,guess = 1,n,tolerance = 0.00001
    character :: enterGuess = 'n'
    
    ! print *,"Enter a valid tolerance level: "
    ! read *,tolerance
    print *, "Tolerance level is ",tolerance,'\n'
    print *, "Which number's square root do you want to calculate? "
    read *,n;
    print *, "Do you want to enter any guess value?(y/n) Default is ",n
    read *, enterGuess
    
    if(enterGuess =='y') then
        print *, "Enter guess: "
        read *, guess
    else if(enterGuess == 'n') then
        guess = n
    else
        print *,"Invalid Input!!"
        stop 1
    end if

    prevVal = huge(prevVal)

    do while(prevVal - guess >= tolerance .or. prevVal-guess <= -1*tolerance)
        prevVal = guess
        guess = 0.5*(prevVal+(n/prevVal))
    end do

    print *, "The square root of ",n," is ",guess

end program sqrt_newton_method