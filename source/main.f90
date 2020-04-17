PROGRAM TESTY
  use dictionary_mod
  IMPLICIT NONE
  CHARACTER(LEN=mkl) :: InString
  CHARACTER(LEN=32) :: sValue
  CHARACTER(LEN=mkl), DIMENSION(:), ALLOCATABLE :: keys
  DOUBLE PRECISION :: xValue
  TYPE(Dict) :: MyDict
  INTEGER :: k

  CALL GETARG(1, InString)
  CALL GETARG(2, sValue)
  READ(sValue,'(F32.0)') xValue

  !write(0,*)"getargs: ", trim(InString), xValue
  !write(0,*) "hash index:", GetHash(InString)

  CALL MyDict%initialize()

  CALL MyDict%add("dog", 77.7d0)
  CALL MyDict%add(InString, xValue)
  CALL MyDict%add("god", 33.3d0)
  CALL MyDict%add("god", 44.3d0)
  CALL MyDict%add("ogd", 22.2d0)
  CALL MyDict%add("cat", 12.2d0)
  CALL MyDict%add("tac", huge(0d0))
  CALL MyDict%add("act", tiny(0d0))

  write(6,'(/A)') "testing GetValue..."
  write(6,*) "dog :", MyDict%value("dog")
  write(6,*) "god :", MyDict%value("god")
  write(6,*) "ogd :", MyDict%value("ogd")

  write(6,'(/A)') "testing PrintDict..."
  CALL PrintDict(MyDict)

  write(6,'(/A)') "testing GetKeys..."
  keys = MyDict%get_keys()
  DO k = 1, SIZE(keys)
    write(6,*) "'" // TRIM(keys(k)) // "' :", MyDict%value(keys(k))
  END DO
  
END PROGRAM TESTY
