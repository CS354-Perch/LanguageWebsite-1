program basicStackImplementation
implicit none
	
	integer, dimension (5) :: stack
	integer :: stackPosition
	
	stackPosition = 0
	
	call push(stack, stackPosition, 1)
	call push(stack, stackPosition, 2)
	call push(stack, stackPosition, 3)
	call push(stack, stackPosition, 4)
	call push(stack, stackPosition, 5)
	call push(stack, stackPosition, 6)
	call pop(stack, stackPosition)
	call push(stack, stackPosition, 6)
	
end program basicStackImplementation

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
	
	call printStack(s, sp)

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
	
	call printStack(s, sp)

end subroutine pop

subroutine printStack (s, sp)
implicit none

	integer, dimension (5) :: s
	integer :: sp
	integer :: i
	
	Print *, 'Current stack contents below:'
	do i = 1, sp
		Print *, s(i)
	end do

end subroutine printStack