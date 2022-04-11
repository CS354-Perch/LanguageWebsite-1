program basicStackImplementation
implicit none
	
	integer, dimension (5) :: stack
	integer :: stackPosition
	
	stackPosition = 0
	
	call askForInput(stack, stackPosition)
	
end program basicStackImplementation

subroutine askForInput (s, sp)
implicit none

	integer, dimension (5), intent (out) :: s
	integer :: sp
	integer :: newVal
	character (len=10) :: input
	
	Print *, ''
	Print *, '"push" | "pop" | "print" | "done" | "help"'
	Print *, ''
	
	read *,input
	Print *, ''
	
	IF (input == 'done') THEN
		Print *, 'Goodbye!'
	ELSE IF (input == 'push') THEN
		Print *, 'What would you like to push? Please type an integer into the console then hit the ENTER key'
		Print *, ''
		read *,newVal
		Print *, ''
		call push(s, sp, newVal)
	ELSE IF (input == 'pop') THEN
		call pop(s, sp)
	ELSE IF (input == 'print') THEN
		call printStack(s, sp)
	ELSE IF (input == 'help') THEN
		call explain(s, sp)
	ELSE
		Print *, 'Unexpected input!'
		Print *, ''
		call explain(s, sp)
	END IF

end subroutine askForInput

subroutine explain (s, sp)
implicit none

	integer, dimension (5), intent (out) :: s
	integer :: sp
	
	Print *, 'Type push to be asked for a value to push onto the top of the stack'
	Print *, 'Type pop to pop the top value off the stack'
	Print *, 'Type print to print the current contents of the stack'
	Print *, 'Type done when you want to exit the program'
	Print *, 'Type help to see this message again'
	
	call askForInput(s, sp)

end subroutine explain

subroutine push (s, sp, newVal)
implicit none

	integer, dimension (5), intent (out) :: s
	integer :: sp
	integer :: newVal
	
	Print *, 'Trying to push ',newVal,' onto the top of the stack'
	
	IF (sp < 5) THEN
		sp = sp + 1
		s(sp) = newVal
		Print *, 'Push successful!'
	ELSE
		Print *, 'Push failed! Stack is full! Pop before trying again!'
	END IF
	
	call askForInput(s, sp)

end subroutine push

subroutine pop (s, sp)
implicit none

	integer, dimension (5), intent (out) :: s
	integer :: sp
	character*36 output
	
	IF (sp > 0) THEN
		Print *, 'Pop successful! Removed ',s(sp),' from the stack'
		sp = sp - 1
	ELSE
		Print *, 'Pop failed! Stack is empty! Push before trying again!'
	END IF
	
	call askForInput(s, sp)

end subroutine pop

subroutine printStack (s, sp)
implicit none

	integer, dimension (5) :: s
	integer :: sp
	integer :: i
	
	IF (sp < 1) THEN
		Print *, 'Stack is empty! Use push to add something to the stack then try again'
	ELSE
		Print *, 'Current stack contents below:'
		do i = 1, sp
			Print *, s(i)
		end do
	END IF
	
	call askForInput(s, sp)

end subroutine printStack