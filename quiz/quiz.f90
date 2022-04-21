program quiz

! External modules that contain subroutines that can be called
USE rand_module
USE read_quiz

implicit none

! This will be used when the user is supposed to type a letter
character (len=1) :: input

! This will be used when the user is supposed to type a number
integer :: answer, i

! Array for descriptions that are used in study mode
character(len=75), dimension(15) :: descriptions

! Array for extended descriptions that are used in study mode
! Had to create a new array so to bypass max 75 character output limit
character(len=75), dimension(15) :: extendedDescription

! Question array
!character(len=75), dimension(15) :: questions
character*75, dimension(:), allocatable :: questions

! Array of corresponding answers (years)
!integer, dimension(15) :: yearAnswers
integer, dimension (:), allocatable :: yearAnswers

! Array containing the indices of the questions (order in which questions will be asked)
integer, dimension (15) :: questionOrder

!Call subroutine from randmodule to randomize non-duplicate ints in an array from 1 to length
call randomize_indices(questionOrder)

!Call subroutine to read from data file and populate two ararys: one with the question, the other with answers
call read_questions(questions, yearAnswers)

! Building the arrays for descriptions and extendedDescriptions
descriptions(1) = 'First document to put into writing the principle that the king and'
extendedDescription(1) = 'his government was not above the law.'
descriptions(2) = 'The confiscation of the English-held duchy of Guyenne by French King Philip'
extendedDescription(2) = 'VI.'
descriptions(3) = 'Discovery of the New World of the Americas on board his ship Santa Maria.'
extendedDescription(3) = 'Which, was the biggest ship out of the three used'
descriptions(4) = 'A fleet of Spanish ships led by Spanish commander Medina Sidonia with'
extendedDescription(4) = 'the purpose of overthrowing Queen Elizabeth I.'
descriptions(5) = 'Seven Years War is a global conflict which ran from 1756 until 1763, it'
extendedDescription(5) = 'made a coalition of Great Britain and allies against France and its allies.'
descriptions(6) = 'The French Revolution began in 1789 and lasted until 1794. King Louis XVI'
extendedDescription(6) = 'needed money, but had failed to raise taxes when he had called a meeting.'
descriptions(7) = 'The assassination of Austrian Archduke Franz Ferdinand (June 28, 1914) was'
extendedDescription(7) = 'the main catalyst for the start of the Great War (World War I).'
descriptions(8) = 'Napoleons forces were crushed in the morning of June 18th 1815, ending 23'
extendedDescription(8) = 'years of recurrent warfare between France and the other powers of Europe.'
descriptions(9) = 'For centuries tsars ruled Russia. This period came to an end during the'
extendedDescription(9) = 'Russian revolution of 1917.'
descriptions(10) = 'USSR was Founded in 1922 as a confederation of Russia, Belarus, Ukraine,'
extendedDescription(10) = 'and Transcaucasia. The USSR eventually grew to 15 republics.'
descriptions(11) = 'The Depression was the longest and deepest downturn in the history of the'
extendedDescription(11) = 'US. The Great Depression began in August 1929.'
descriptions(12) = 'WW2 was a global war that lasted from 1939 to 1945. It involved the vast'
extendedDescription(12) = 'majority of the worlds countries including all of the great powers'
descriptions(13) = 'The USSR rocketed to the lead in the Cold Wars "Space Race" with the launch'
extendedDescription(13) = 'of Sputnik on October 4, 1957.'
descriptions(14) = 'On July 20, 1969, American astronauts Neil Armstrong and Edwin Aldrin,'
extendedDescription(14) = 'became the first humans ever to land on the moon.'
descriptions(15) = 'On December 26, 1991, the process of internal disintegration within the'
extendedDescription(15) = 'Soviet Union (USSR) which ended its existence as a sovereign state.'

! Do you want to take the quiz or study first?
! nested do loops for "restart" option
big: do
   do i = 0, 0
     Print *, 'Press (q) to take the quiz or press (s) to study first. You can press (0) at any time to exit the program.' 
     Read(*,*) input

     If (input == '0') then
        Print *, 'Thank you for studying with us! Exiting program…'
        call EXIT(0)

     else if (input == 's') then
            CALL studyMode(questions, yearAnswers, descriptions, extendedDescription)

     else if (input == 'q') then
        Print *, 'Would you like easy mode (e), normal mode (n), or hard mode (h)?'
        Read(*,*) input
     if (input == 'e') then
        CALL easyMode(questions, yearAnswers, questionOrder)
     else if (input == 'n') then
        CALL normalMode(questions, yearAnswers, questionOrder)
     else if (input == 'h') then
        CALL hardMode(questions, yearAnswers, questionOrder)
        End if
     else
        Print *, 'That is an invalid option. Please restart the program and try again.'
     End if
   end do
   Print *, 'Would you like to restart? (1 for yes, 0 for no)'
   Read *, input
   if (input == '0') then
      Print *, 'Goodbye!'
      exit big
   end if
end do big
! SELECT CASE statement is a Fortran construct that allows the comparison of one variable to a list of values
! nested select statements are allowed and the can be named just like a DO construct
!big : do
 !       do i = 0, 0
  !          Print *, 'Press (q) to take the quiz or press (s) to study first. You can press (0) at any time to exit the program.'
   !         Read(*, *) input
    !        select case (input)
     !       case ('s')
      !          CALL studyMode(questions, yearAnswers, descriptions, extendedDescription)
       !     case ('q')
        !        Print *, 'Would you like easy mode (e), normal mode (n), or hard mode (h)?'
         !       Read(*, *) input
          !      inner : select case (input)
           !     case ('e')
            !        call easyMode(questions, yearAnswers, questionOrder)
             !   case ('n')
              !      call normalMode(questions, yearAnswers, questionOrder)
!                case ('h')
 !                   call hardMode(questions, yearAnswers, questionOrder)
  !              case default
   !                 Print *, 'That is an invalid option. Please restart the program and try again.'
    !                exit
     !           end select inner
      !      case ('0')
       !         Print *, 'Thank you for studying with us! Exiting program…'
        !        call EXIT(0)
         !   case default
          !      Print *, 'That is an invalid option. Please restart the program and try again.'
!
 !           end select
  !      end do
   !     Print *, 'Would you like to restart? (1 for yes, 0 for no)'
    !    Read *, input
     !   if (input == '0') then
      !      Print *, 'Goodbye!'
       !     exit big
        !end if
!    end do big
end program quiz

subroutine studyMode(questions, yearAnswers, descriptions, extendedDescription)
Implicit none

Integer :: i
Character (len = 1) :: input
character(len=75), dimension(15), intent(in) :: questions
character(len=75), dimension(15), intent(in) :: descriptions
character(len=75), dimension(15), intent(in) :: extendedDescription
integer, dimension(15), intent(in) :: yearAnswers
LOGICAL :: onQuestion
onQuestion = .TRUE.


Print *, 'Questions will be presented in a flash-card format.'
Print *, 'Press (f) to flip the card and see the correct answer.'
Print *, 'Press (n) to go to the next card.'
Print *, 'Press (e) at any time to end the study session.'
Print *, 'There will be a total of 15 questions.'
Print *, ''

do i = 1, 15
    onQuestion = .TRUE.
    Print *, questions(i)
    Read(*,*) input
    do while(input /= 'n')  
        If (input == 'f') then
            CALL studyQuestion(onQuestion, i, questions, yearAnswers, descriptions, extendedDescription)
            Read(*,*) input
        else if(input == 'e') then
                CALL EXIT(0)
        else
            Print*, 'That is not a valid character.'
            Read(*,*) input
        end if
    end do
end do

end subroutine studyMode



subroutine studyQuestion(onQuestion, i, questions, yearAnswers, descriptions, extendedDescription)
Implicit none

LOGICAL :: onQuestion
character(len=75), dimension(15), intent(in) :: questions
character(len=75), dimension(15), intent(in) :: descriptions
character(len=75), dimension(15), intent(in) :: extendedDescription
integer, dimension(15), intent(in) :: yearAnswers
integer, dimension(15) :: j
integer :: i

if (onQuestion) then
    Print *, yearAnswers(i)
    Print *, descriptions(i)
    Print *, extendedDescription(i)
    onQuestion = .FALSE.
else 
    Print *, questions(i)
    onQuestion = .TRUE.
end if

end subroutine studyQuestion



subroutine easyMode(questions, yearAnswers, questionOrder)
implicit none
!CI = Correct Index
!TC = Total Correct
character(len=75), dimension(15), intent(in) :: questions
integer, dimension(15), intent(in) :: yearAnswers
integer, dimension(15), intent(in) :: questionOrder
integer :: questionsToAsk, CI, TC, i, INPUT

questionsToAsk = 5
TC = 0

do i = 1, questionsToAsk
        Print *, 'Type 0 at any time to exit the program'
        Print *, questions(questionOrder(i))
        CALL easyModeAnswers(questionOrder(i), CI, questions, yearAnswers, questionOrder)
        Print *, 'Please type a number 1-4 according to your answer: '
        Read *, INPUT
        if (INPUT==CI) then
                Print *, 'Correct!'
                TC = TC+1
        else if (INPUT==0) then
                Print *, 'Goodbye!'
                exit
        else
                Print *, 'So close!'
        end if
end do
Print *, 'You got ',TC,'/',questionsToAsk,' correct.'

end subroutine easyMode

subroutine easyModeAnswers(qIndex, correct, questions, yearAnswers, questionOrder)
implicit none
character(len=75), dimension(15), intent(in) :: questions
integer, dimension(15), intent(in) :: yearAnswers
integer, dimension(15), intent(in) :: questionOrder
integer, intent(in) :: qIndex !index of the question being asked is the parameter
!arrays for answers to be printed and the indexes of the answers
integer, dimension(4) :: answers, indexes
integer :: flag, index, correct, i, j
real :: r

indexes(1)=qIndex !store the index of the correct answer

do i=2, 4 !generate random wrong indexes
        flag=1
        do while (flag ==1)
                call random_number(r)
                index = (r*15)+1
                flag = 0
                do j=1,i
                        if (indexes(j)==index) then
                                flag=1
                                exit
                        end if
                end do
        end do
        if (flag==0) then
                indexes(i)=index
        end if
end do
do i=1, 4 !put answers in random order and store correct answer in correct
        flag=1
        do while(flag==1)
                call random_number(r)
                index = (r*4)+1
                flag = 0
                do j=1,4
                        if(answers(j)==yearAnswers(indexes(index))) then
                                flag=1
                                exit
                        end if
                end do
        end do
        if (flag==0) then
                answers(i) = yearAnswers(indexes(index))
                if(i==1) then
                        correct = index
                end if
        end if
end do
do i=1, 4
        Print *, i,':',answers(i)
end do

end subroutine easyModeAnswers
! --------------------------------------------------------------------------------------------------------------------------!
!              HARD MODE STARTS HERE

subroutine hardMode(questions, yearAnswers, questionOrder)
    implicit none
        !CI = Correct Index
        !TC = Total Correct
        character(len=75), dimension(15), intent(in) :: questions
        integer, dimension(15), intent(in) :: yearAnswers
        integer, dimension(15), intent(in) :: questionOrder
        integer :: questionsToAsk, CI, TC, i, INPUT

        questionsToAsk = 15
        TC = 0

        do i = 1, questionsToAsk
            Print *, 'Type 0 at any time to exit the program'
            Print *, questions(questionOrder(i))
            CALL hardModeAnswers(questionOrder(i), CI, questions, yearAnswers, questionOrder)
            Print *, 'Please type a number 1-4 according to your answer: '
            Read *, INPUT
            if (INPUT==CI) then
                Print *, ' '
                TC = TC+1
            else if (INPUT==0) then
                Print *, 'Goodbye!'
                exit
            else
                Print *, ' '
            end if
        end do
        Print *, 'You got ',TC,'/',questionsToAsk,' correct.'
end subroutine hardMode


subroutine hardModeAnswers(qIndex, correct, questions, yearAnswers, questionOrder)
    implicit none
    character(len=75), dimension(15), intent(in) :: questions
    integer, dimension(15), intent(in) :: yearAnswers
    integer, dimension(15), intent(in) :: questionOrder
    integer, intent(in) :: qIndex !index of the question being asked is the parameter
    !arrays for answers to be printed and the indexes of the answers
    integer, dimension(4) :: answers, indexes
    integer :: flag, index, correct, i, j
    real :: r

    indexes(1)=qIndex !store the index of the correct answer and get closest 3 answers
       do i=2, 4
        flag=1
        do while (flag == 1)
            if (qIndex <= 10) then
                index = qIndex+i
                else
                index = qIndex-i
                end if
        flag = 0
             do j=1,i
                if (indexes(j)==index) then
                    flag=1
                    exit
                end if
            end do
            end do
        if (flag==0) then
            indexes(i)=index
        end if
        end do
        do i=1, 4 !put answers in random order and store correct answer in correct
            flag=1
            do while(flag==1)
                call random_number(r)
                index = (r*4)+1
                flag = 0
                do j=1,4
                    if(answers(j)==yearAnswers(indexes(index))) then
                        flag=1
                        exit
                    end if
                end do
            end do
        if (flag==0) then
            answers(i) = yearAnswers(indexes(index))
            if(i==1) then
                correct = index
            end if
        end if
    end do
    do i=1, 4
        Print *, i,':',answers(i)
    end do

end subroutine hardModeAnswers


! —---------------------------------------------------------------------------------------------- !

! Everything below this comment is for the implementation of normal mode !

subroutine normalMode(questions, yearAnswers, questionOrder)
        !CI = correct index
        !WI1 = wrong index 1
        !WI2 = wrong index 2
        !WI3 = wrong index 3
        !CC = correct choice
        !WC1 = wrong choice 1
        !WC2 = wrong choice 2
        !WC3 = wrong choice 3
        !RS = range start
        !RE = range end
        !AC = already correct
        integer CI, WI1, WI2, WI3, CC, WC1, WC2, WC3, RS, RE, AC, INPUT, TC
        integer, dimension(4) :: choices
        character(len=75), dimension(15) :: questions
        integer, dimension (15) :: yearAnswers, questionOrder, questionsToAsk

        !total correct counter
        TC = 0
        !num questions
        questionsToAsk = 15
        Print *, 'Type 0 at any time to exit the program'
        do i = 1, 15
        RS = 1
        RE = 4
        AC = 0
        call fourUniqueRandomNumbers(CI, WI1, WI2, WI3, RS, RE, AC)
        ! CI - The correct answer will be displayed at this index
        ! WI1-WI3 The wrong answers will be displayed at these indices
        RS = 1
        RE = 15
        !call randomNumberFromRange(RS, RE, CC)
        CC = questionOrder(i)
        ! CC - The selected question-answer pair for this iteration
        AC = 1
        IF (CC < RE / 2) THEN
                RE = RE / 2
        ELSE
                RS = (RE / 2) + 1
        END IF
        call fourUniqueRandomNumbers(CC, WC1, WC2, WC3, RS, RE, AC)
        ! WC1-WC3 The selected answers that are wrong
        choices(CI) = CC
        choices(WI1) = WC1
        choices(WI2) = WC2
        choices(WI3) = WC3
        Print *, questions(CC)
        Print *, 'Please type a number 1-4 according to your answer: '
        do n = 1, 4
                Print *, n, ':', yearAnswers(choices(n))
        end do
        read *, INPUT
        IF (INPUT == CI) THEN
                Print *, 'That is so right!'
                TC = TC+1
        ELSE IF (INPUT == 0) THEN
                Print *, 'Goodbye!'
                EXIT
        ELSE
                Print *, 'That is so wrong...'
        END IF
        end do
        Print *, 'You got ',TC,'/',15,' correct.'
end subroutine normalMode

subroutine randomNumberFromRange(RS, RE, CI)
        integer RS, RE, CI

        CALL random_number(r)
        CI = FLOOR((r * (RE - RS + 1))) + RS

end subroutine randomNumberFromRange

subroutine fourUniqueRandomNumbers(C, W1, W2, W3, RS, RE, AC)
        !C = correct
        !W1 = wrong 1
        !W2 = wrong 2
        !W3 = wrong 3
        integer C, W1, W2, W3, RS, RE, flag, AC
        IF (AC == 0) THEN
                call randomNumberFromRange(RS, RE, C)
        END IF
        call randomNumberFromRange(RS, RE, W1)
        do while (W1 == C)
                call randomNumberFromRange(RS, RE, W1)
        end do
        flag = 0
        do while (flag == 0)
                call randomNumberFromRange(RS, RE, W2)
                IF (W2 /= C) THEN
                        IF (W2 /= W1) THEN
                                flag = 1
                        END IF
                END IF
        end do
        flag = 0
        do while (flag == 0)
                call randomNumberFromRange(RS, RE, W3)
                IF (W3 /= C) THEN
                        IF (W3 /= W1) THEN
                                IF (W3 /= W2) THEN
                                        flag = 1
                                END IF
                        END IF
                END IF
        end do

end subroutine fourUniqueRandomNumbers

! Everything above this comment is for the implementation of normal mode !

! —---------------------------------------------------------------------------------------------- !



