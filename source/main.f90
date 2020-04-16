PROGRAM TESTY
  use dictionary_mod
  IMPLICIT NONE
  CHARACTER(LEN=mkl) :: InString
  CHARACTER(LEN=32) :: sValue
  CHARACTER(LEN=mkl), DIMENSION(:), ALLOCATABLE :: keys
  DOUBLE PRECISION :: xValue, tempval
  TYPE(Dict) :: MyDict
  INTEGER :: k

  CALL GETARG(1, InString)
  CALL GETARG(2, sValue)
  READ(sValue,'(F32.0)') xValue

  !write(0,*)"getargs: ", trim(InString), xValue
  !write(0,*) "hash index:", GetHash(InString)

  CALL InitializeDict(MyDict)
  CALL AddToDict(MyDict, "dog", 77.7d0)
  CALL AddToDict(MyDict, TRIM(InString), xValue)
  CALL AddToDict(MyDict, "god", 33.3d0)
  CALL AddToDict(MyDict, "god", 44.3d0)
  CALL AddToDict(MyDict, "ogd", 22.2d0)
  CALL AddToDict(MyDict, "cat", 12.2d0)
  CALL AddToDict(MyDict, "tac", huge(0d0))
  CALL AddToDict(MyDict, "act", tiny(0d0))

  write(6,'(/A)') "testing GetValue..."
  write(6,*) "dog :", GetValue(MyDict, "dog")
  write(6,*) "god :", GetValue(MyDict, "god")
  write(6,*) "ogd :", GetValue(MyDict, "ogd")

  write(6,'(/A)') "testing PrintDict..."
  CALL PrintDict(MyDict)

  write(6,'(/A)') "testing GetKeys..."
  keys = GetKeys(MyDict)
  DO k = 1, SIZE(keys)
    tempval = GetValue(MyDict, keys(k))
    write(6,*) "'" // TRIM(keys(k)) // "' :", tempval
  END DO
  
END PROGRAM TESTY
