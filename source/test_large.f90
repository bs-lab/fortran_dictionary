PROGRAM MAIN_TESTS
  use dictionary_mod
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp=KIND(1d0)
  INTEGER, PARAMETER :: inn=100, fid=200
  CHARACTER(LEN=256) :: AllWordsFile, OutputFile
  CHARACTER(LEN=128) :: Word
  CHARACTER(LEN=mkl), DIMENSION(:), ALLOCATABLE :: keys
  CHARACTER(LEN=mlv) :: tempchar
  TYPE(Dict) :: LargeDict
  INTEGER :: k, ios

  CALL GETARG(1, AllWordsFile)
  CALL GETARG(2, OutputFile)

  write(6,'(/A)') "----------------------------------------------------"
  write(6,'(A)')  "LOTS O' WORDS"
  write(6,'(A)')  "----------------------------------------------------"
  ! read every entry as a key and use that as the value also
  CALL LargeDict%initialize("")
  OPEN(UNIT=inn, FILE=AllWordsFile, ACTION='read', STATUS='old')
  DO
    READ(inn, *, iostat=ios) word
    IF (ios < 0) EXIT
    !write(0,*)"word xxx"//trim(word)//"xxx"
    CALL LargeDict%add(trim(word), trim(word))
  END DO
  CLOSE(inn)

  OPEN(UNIT=fid, FILE=OutputFile, STATUS='unknown', ACTION='write')
  write(fid,'(/A)') "testing dict%print()..."
  CALL LargeDict%print(fid)

  write(fid,'(/A)') "hash distribution"
  CALL PrintSummary(LargeDict, fid)

  write(6,'(/A)') "testing GetKeys..."
  write(fid,'(/A)') "testing GetKeys..."
  keys = LargeDict%get_keys()
  DO k = 1, SIZE(keys)
    CALL LargeDict%value(keys(k), tempchar)
    write(fid,*) "'" // TRIM(keys(k)) // "' :", TRIM(tempchar)
  END DO

  CALL LargeDict%free()
  CLOSE(fid)

END PROGRAM MAIN_TESTS
