program MAIN_TEST_SMALL
  use dictionary_mod
  implicit none
  integer, parameter :: dp=kind(1d0)
  integer, parameter :: inn=100
  character(len=mkl) :: InString
  character(len=32) :: sValue
  character(len=mkl), dimension(:), allocatable :: keys
  character(len=mlv) :: tempchar
  real(dp) :: xValue
  real(dp) :: tempy
  type(Dict) :: MyDict, MyDictInt, MyDictChar
  integer :: k, tempint

  call getarg(1, InString)
  call getarg(2, sValue)
  read(sValue,'(F32.0)') xValue

  !write(0,*)"getargs: ", trim(InString), xValue
  !write(0,*) "hash index:", GetHash(InString)

  write(6,'(A)')  "----------------------------------------------------"
  write(6,'(A)')  "DOUBLE PRECISION"
  write(6,'(A/)') "----------------------------------------------------"

  call MyDict%initialize(0d0)
  call MyDict%add("dog", 77.7d0)
  call MyDict%add(InString, xValue)
  call MyDict%add("god", 33.3d0)
  call MyDict%add("god", 44.3d0)
  call MyDict%add("ogd", 22.2d0)
  call MyDict%add("cat", 12.2d0)
  call MyDict%add("tac", huge(0d0))
  call MyDict%add("act", tiny(0d0))

  write(6,'(/A)') "testing GetValue..."

  call MyDict%value("dog", tempy)
  write(6,*) "dog :", tempy
  call MyDict%value("god", tempy)
  write(6,*) "god :", tempy
  call MyDict%value("ogd", tempy)
  write(6,*) "ogd :", tempy

  write(6,'(/A)') "testing PrintDict..."
  call MyDict%print()

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDict%get_keys()
  do k = 1, size(keys)
    call MyDict%value(keys(k), tempy)
    write(6,*) "'" // trim(keys(k)) // "' :", tempy
  end do
  call MyDict%free()


  write(6,'(/A)') "----------------------------------------------------"
  write(6,'(A)')  "INTEGERS"
  write(6,'(A)')  "----------------------------------------------------"

  call MyDictInt%initialize(0)
  call MyDictInt%add("fido", 77) 
  call MyDictInt%add("doif", 66) 
  call MyDictInt%add("ifdo", 55) 
  call MyDictInt%add("scrabble", 35) 

  write(6,'(/A)') "testing PrintDict..."
  call MyDictInt%print(6)

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDictInt%get_keys()
  do k = 1, size(keys)
    call MyDictInt%value(keys(k), tempint)
    write(6,*) "'" // trim(keys(k)) // "' :", tempint
  end do
  call MyDictInt%free()


  write(6,'(/A)') "----------------------------------------------------"
  write(6,'(A)')  "CHARACTERS"
  write(6,'(A)')  "----------------------------------------------------"
  call MyDictChar%initialize("")
  call MyDictChar%add("fido", "aaa")
  call MyDictChar%add("doif", "bbb")
  call MyDictChar%add("ifdo", "ccc")
  call MyDictChar%add("scrabble", "ddd")

  write(6,'(/A)') "testing PrintDict..."
  call MyDictChar%print(6)

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDictChar%get_keys()
  do k = 1, size(keys)
    call MyDictChar%value(keys(k), tempchar)
    write(6,*) "'" // trim(keys(k)) // "' :", trim(tempchar)
  end do
  call MyDictChar%free()

end program MAIN_TEST_SMALL
