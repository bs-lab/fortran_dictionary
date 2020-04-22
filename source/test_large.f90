program MAIN_TEST_LARGE
  use dictionary_mod
  implicit none
  integer, parameter :: dp=kind(1d0)
  integer, parameter :: inn=100, fid=200
  character(len=256) :: AllWordsFile, OutputFile
  character(len=128) :: Word
  character(len=mkl), dimension(:), allocatable :: keys
  character(len=mlv) :: tempchar
  type(Dict) :: LargeDict
  integer :: k, ios

  call getarg(1, AllWordsFile)
  call getarg(2, OutputFile)

  write(6,'(/A)') "----------------------------------------------------"
  write(6,'(A)')  "LOTS O' WORDS"
  write(6,'(A)')  "----------------------------------------------------"
  ! read every entry as a key and use that as the value also
  call LargeDict%initialize("")
  open(unit=inn, file=AllWordsFile, action='read', status='old')
  do
    read(inn, *, iostat=ios) word
    if (ios < 0) exit
    !write(0,*)"word xxx"//trim(word)//"xxx"
    call LargeDict%add(trim(word), trim(word))
  end do
  close(inn)

  open(unit=fid, file=OutputFile, status='unknown', action='write')
  write(fid,'(/A)') "testing dict%print()..."
  call LargeDict%print(fid)

  write(fid,'(/A)') "hash distribution"
  call PrintSummary(LargeDict, fid)

  write(6,'(/A)') "testing GetKeys..."
  write(fid,'(/A)') "testing GetKeys..."
  keys = LargeDict%get_keys()
  do k = 1, size(keys)
    call LargeDict%value(keys(k), tempchar)
    write(fid,*) "'" // trim(keys(k)) // "' :", trim(tempchar)
  end do

  call LargeDict%free()
  close(fid)

end program MAIN_TEST_LARGE
