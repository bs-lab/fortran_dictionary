PROGRAM MAIN_TESTS
  use dictionary_mod
  IMPLICIT NONE
  INTEGER, PARAMETER :: dp=KIND(1d0)
  CHARACTER(LEN=mkl) :: InString
  CHARACTER(LEN=32) :: sValue
  CHARACTER(LEN=mkl), DIMENSION(:), ALLOCATABLE :: keys
  CHARACTER(LEN=mlv) :: tempchar
  REAL(dp) :: xValue, tempy
  TYPE(Dict) :: MyDict, MyDictInt, MyDictChar
  INTEGER :: k, tempint

  CALL GETARG(1, InString)
  CALL GETARG(2, sValue)
  READ(sValue,'(F32.0)') xValue

  !write(0,*)"getargs: ", trim(InString), xValue
  !write(0,*) "hash index:", GetHash(InString)

  write(6,'(A)')  "----------------------------------------------------"
  write(6,'(A)')  "DOUBLE PRECISION"
  write(6,'(A/)') "----------------------------------------------------"

  CALL MyDict%initialize(0d0)
  CALL MyDict%add("dog", 77.7d0)
  CALL MyDict%add(InString, xValue)
  CALL MyDict%add("god", 33.3d0)
  CALL MyDict%add("god", 44.3d0)
  CALL MyDict%add("ogd", 22.2d0)
  CALL MyDict%add("cat", 12.2d0)
  CALL MyDict%add("tac", huge(0d0))
  CALL MyDict%add("act", tiny(0d0))

  write(6,'(/A)') "testing GetValue..."

  CALL MyDict%value("dog", tempy)
  write(6,*) "dog :", tempy
  CALL MyDict%value("god", tempy)
  write(6,*) "god :", tempy
  CALL MyDict%value("ogd", tempy)
  write(6,*) "ogd :", tempy

  write(6,'(/A)') "testing PrintDict..."
  CALL MyDict%print()

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDict%get_keys()
  DO k = 1, SIZE(keys)
    CALL MyDict%value(keys(k), tempy)
    write(6,*) "'" // TRIM(keys(k)) // "' :", tempy
  END DO


  write(6,'(/A)') "----------------------------------------------------"
  write(6,'(A)')  "INTEGERS"
  write(6,'(A)')  "----------------------------------------------------"

  CALL MyDictInt%initialize(0)
  CALL MyDictInt%add("fido", 77) 
  CALL MyDictInt%add("doif", 66) 
  CALL MyDictInt%add("ifdo", 55) 
  CALL MyDictInt%add("scrabble", 35) 

  write(6,'(/A)') "testing PrintDict..."
  CALL MyDict%print(6)

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDictInt%get_keys()
  DO k = 1, SIZE(keys)
    CALL MyDictInt%value(keys(k), tempint)
    write(6,*) "'" // TRIM(keys(k)) // "' :", tempint
  END DO


  write(6,'(/A)') "----------------------------------------------------"
  write(6,'(A)')  "CHARACTERS"
  write(6,'(A)')  "----------------------------------------------------"
  CALL MyDictChar%initialize("")
  CALL MyDictChar%add("fido", "aaa")
  CALL MyDictChar%add("doif", "bbb")
  CALL MyDictChar%add("ifdo", "ccc")
  CALL MyDictChar%add("scrabble", "ddd")

  write(6,'(/A)') "testing PrintDict..."
  CALL MyDict%print(6)

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDictChar%get_keys()
  DO k = 1, SIZE(keys)
    CALL MyDictChar%value(keys(k), tempchar)
    write(6,*) "'" // TRIM(keys(k)) // "' :", TRIM(tempchar)
  END DO

  
END PROGRAM MAIN_TESTS
