MODULE DICTIONARY_MOD
  IMPLICIT NONE

  PRIVATE
  PUBLIC mkl, mlv, Dict

  INTEGER, PARAMETER :: dp=KIND(1d0)

  INTEGER, PARAMETER :: hash_size=101
  INTEGER, PARAMETER :: mkl=64         ! max key length
  INTEGER, PARAMETER :: mll=32         ! max size of linked lists
  INTEGER, PARAMETER :: mlv=4096       ! max length of dictionary value (for char array values)

  TYPE Dict
    CHARACTER(LEN=mkl), DIMENSION(hash_size, mll) :: Keys
    INTEGER, DIMENSION(hash_size, mll) :: Values_int
    REAL(dp), DIMENSION(hash_size, mll) :: Values_dble
    CHARACTER(LEN=mlv), DIMENSION(hash_size, mll) :: Values_char
    INTEGER, DIMENSION(hash_size) :: used
    INTEGER :: vtype   ! 0: undefined;  1: integer;  2: double precision;  3: char array
  CONTAINS
    PROCEDURE :: print => PrintDict
    PROCEDURE :: initialize
    PROCEDURE :: get_keys
    PROCEDURE :: value => GetValue
    PROCEDURE :: add => AddToDict
  END TYPE Dict

CONTAINS

  SUBROUTINE PrintDict(MyDict, fid_arg)
    CLASS(Dict), INTENT(IN) :: MyDict
    INTEGER, OPTIONAL :: fid_arg
    INTEGER :: i, j, fid

    IF (PRESENT(fid_arg)) THEN
      fid = fid_arg
    ELSE
      fid = 6
    END IF

    IF (MyDict%vtype == 1) THEN
      DO i = 1, hash_size
        DO j = 1, MyDict%used(i)
          IF (MyDict%Keys(i,j) /= "") THEN
            WRITE(fid,*) TRIM(MyDict%Keys(i,j)), " :", MyDict%Values_int(i,j)
          END IF
        END DO
      END DO
    ELSEIF (MyDict%vtype == 2) THEN
      DO i = 1, hash_size
        DO j = 1, MyDict%used(i)
          IF (MyDict%Keys(i,j) /= "") THEN
            WRITE(fid,*) TRIM(MyDict%Keys(i,j)), " :", MyDict%Values_dble(i,j)
          END IF
        END DO
      END DO
    ELSEIF (MydIct%vtype == 3) THEN
      DO i = 1, hash_size
        DO j = 1, MyDict%used(i)
          IF (MyDict%Keys(i,j) /= "") THEN
            WRITE(fid,*) TRIM(MyDict%Keys(i,j)), " :", TRIM(MyDict%Values_char(i,j))
          END IF
        END DO
      END DO
    END IF

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
  SUBROUTINE initialize(MyDict, kind_val)
    CLASS(Dict) :: MyDict
    CLASS(*) :: kind_val
    INTEGER :: i

    SELECT TYPE(kind_val)
    TYPE IS (integer)
        MyDict%vtype = 1
      TYPE IS (real(8))
        MyDict%vtype = 2
      TYPE IS (character(len=*))
        MyDict%vtype = 3
      CLASS DEFAULT
        MyDict%vtype = 0
    END SELECT

    DO i = 1, hash_size
      MyDict%Keys(i,:) = ""
      MyDict%Values_dble(i,:) = TINY(0d0)
      MyDict%Values_int(i,:) = 0
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
  SUBROUTINE GetValue(MyDict, xKey, xValue)
    CLASS(Dict), INTENT(IN) :: MyDict
    CLASS(*) :: xValue
    CHARACTER(LEN=*), INTENT(IN) :: xKey
    INTEGER :: hash, indx 

    hash = GetHash(xKey)
    indx = DoesKeyExist(MyDict, hash, xKey)
    IF (indx == 0) THEN
      WRITE(0,*)"ERROR: Requested key '" // TRIM(xKey) // "' is not in dict."
      WRITE(0,'(I10)') "traceback"
      STOP
    END IF
    SELECT TYPE(xValue)
      TYPE IS (integer)
        xValue = MyDict%Values_int(hash, indx)
      TYPE IS (real(dp))
        xValue = MyDict%Values_dble(hash, indx)
      TYPE IS (character(len=*))
        xValue = MyDict%Values_char(hash, indx)
      CLASS DEFAULT
    END SELECT

  END SUBROUTINE GetValue


  ! ------------------------------------------------------------------------------------------------
  SUBROUTINE AddToDict(MyDict, xKey, xValue)
    CLASS(Dict), INTENT(INOUT) :: MyDict
    CLASS(*), INTENT(IN) :: xValue
    CHARACTER(LEN=*), INTENT(IN) :: xKey
    INTEGER :: hash, indx, xValue_int
    CHARACTER(LEN=mlv) :: xValue_char
    REAL(dp) :: xValue_dble

    SELECT TYPE(xValue)
      TYPE IS (integer)
        xValue_int = xValue
      TYPE IS (real(dp))
        xValue_dble = xValue
      TYPE IS (character(len=*))
        xValue_char = xValue
      CLASS DEFAULT
    END SELECT

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
    IF (MyDict%vtype == 1) THEN
      MyDict%Values_int(hash, indx) = xValue_int
    ELSEIF (MyDict%vtype == 2) THEN
      MyDict%Values_dble(hash, indx) = xValue_dble
    ELSEIF (MyDict%vtype == 3) THEN
      MyDict%Values_char(hash, indx) = xValue_char
    END IF

  END SUBROUTINE AddToDict

END MODULE DICTIONARY_MOD
