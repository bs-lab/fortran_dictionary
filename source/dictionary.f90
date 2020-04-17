MODULE DICTIONARY_MOD
  IMPLICIT NONE

  PRIVATE
  PUBLIC hash_size, mkl, Dict, PrintDict

  INTEGER, PARAMETER :: hash_size=101
  INTEGER, PARAMETER :: mkl=64         ! max key length
  INTEGER, PARAMETER :: mll=32         ! max size of linked lists

  TYPE Dict
    CHARACTER(LEN=mkl), DIMENSION(hash_size, mll) :: Keys
    DOUBLE PRECISION, DIMENSION(hash_size, mll) :: Values
    INTEGER, DIMENSION(hash_size) :: used
  CONTAINS
    PROCEDURE :: initialize
    PROCEDURE :: get_keys
    PROCEDURE :: value => GetValue
    PROCEDURE :: add => AddToDict
  END TYPE Dict

CONTAINS

  SUBROUTINE PrintDict(MyDict)
    TYPE(Dict), INTENT(IN) :: MyDict
    INTEGER :: i, j

    DO i = 1, hash_size
      DO j = 1, MyDict%used(i)
        IF (MyDIct%Keys(i,j) /= "") THEN
          WRITE(6,*) TRIM(MyDict%Keys(i,j)), " :", MyDict%Values(i,j)
        END IF
      END DO
    END DO

  END SUBROUTINE PrintDict


  ! ------------------------------------------------------------------------------------------------
  PURE FUNCTION GetHash(string_key) RESULT(hash)
    CHARACTER(LEN=*), INTENT(IN) :: string_key
    INTEGER :: hash, c

    hash = 0
    DO c = 1, LEN_TRIM(string_key)
      hash = hash + ICHAR(string_key(c:c))
    END DO
    hash = MODULO(hash, hash_size)

  END FUNCTION GetHash


  ! ------------------------------------------------------------------------------------------------
  PURE FUNCTION DoesKeyExist(MyDict, hash, xKey) RESULT(indx)
    ! returns '0' if key does not exist in dictionary, otherwise
    !   returns the index of the key in the linked list
    TYPE(Dict), INTENT(IN) :: MyDict
    CHARACTER(LEN=*), INTENT(IN) :: xKey
    INTEGER, INTENT(IN) :: hash
    INTEGER ::  u, indx

    indx = 0
    DO u = 1, MyDict%used(hash)
      IF (xKey == MyDict%Keys(hash, u)) THEN
        ! key already exists, so will overwrite its existing corresponding value
        indx = u
        !write(0,*) "going to overwrite at u=", u
        EXIT
      END IF
    END DO

  END FUNCTION DoesKeyExist


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE initialize(MyDict)
    CLASS(Dict) :: MyDict
    INTEGER :: i

    DO i = 1, hash_size
      MyDict%Keys(i,:) = ""
      MyDict%Values(i,:) = TINY(0d0)
      MyDict%used(i) = 0
    END DO

  END SUBROUTINE initialize


  ! ------------------------------------------------------------------------------------------------
  PURE FUNCTION get_keys(MyDict) RESULT(keys)
    CLASS(Dict), INTENT(IN) :: MyDict
    CHARACTER(LEN=mkl), DIMENSION(:), ALLOCATABLE :: keys, keys_sparse
    INTEGER :: i, j, c

    c = 0
    ALLOCATE(keys_sparse(hash_size*mll))
    DO i = 1, hash_size
      DO j = 1, MyDict%used(i)
        IF (MyDIct%Keys(i,j) /= "") THEN
          c = c + 1
          keys_sparse(c) = MyDict%Keys(i,j)
        END IF
      END DO
    END DO
    ALLOCATE(keys(c))
    keys(1:c) = keys_sparse(1:c)
    DEALLOCATE(keys_sparse)

  END FUNCTION get_keys


  ! ------------------------------------------------------------------------------------------------
  FUNCTION GetValue(MyDict, xKey) RESULT(xValue)
    CLASS(Dict), INTENT(IN) :: MyDict
    CHARACTER(LEN=*), INTENT(IN) :: xKey
    DOUBLE PRECISION :: xValue
    INTEGER :: hash, indx 

    hash = GetHash(xKey)
    indx = DoesKeyExist(MyDict, hash, xKey)
    IF (indx == 0) THEN
      WRITE(0,*)"ERROR: Requested key '" // TRIM(xKey) // "' is not in dict."
      WRITE(0,'(I10)') "traceback"
      STOP
    END IF
    xValue = MyDict%Values(hash, indx)

  END FUNCTION GetValue


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE AddToDict(MyDict, xKey, xValue)
    CLASS(Dict), INTENT(INOUT) :: MyDict
    CHARACTER(LEN=*), INTENT(IN) :: xKey
    DOUBLE PRECISION, INTENT(IN) :: xValue
    INTEGER :: hash, indx

    hash = GetHash(xKey)

    ! check if xKey is already in MyDict
    IF (MyDict%used(hash) == 0) THEN
      ! no collision, so key does not already exist in dictionary
      MyDict%used(hash) = 1
      !write(0,*)"no collision"
      indx = 1
    ELSE
      ! collision, so key may or may not already exist in dictionary
      !write(0,*)"collision!"
      indx = DoesKeyExist(MyDict, hash, xKey)
      IF (indx == 0) THEN
        ! collision, but not from the same key, just coincidence
        IF (MyDict%used(hash) == mll) THEN
          WRITE(0,*)"ERROR: Too many collisions.  Recompile with larger 'mll' or 'hash_size' " // &
                    "parameter or alter the hash function."
          WRITE(0,'(I10)') "traceback"
          STOP
        END IF
        MyDict%used(hash) = MyDict%used(hash)+1
        indx = MyDict%used(hash)
      !ELSE
        ! collision because key already existed, so overwrite key's value
        !MyDict%used(hash) = indx 
      END IF
    END IF

    !write(0,*)"used, key, value:", MyDict%used(hash), indx, xKey, xValue
    MyDict%Keys(hash, indx) = xKey
    MyDict%Values(hash, indx) = xValue

  END SUBROUTINE AddToDict

END MODULE DICTIONARY_MOD
