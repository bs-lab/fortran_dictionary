module dictionary_mod
  implicit none

  private
  public mkl, mlv, Dict, PrintSummary

  integer, parameter :: dp=kind(1d0)

  ! hash_size of 4001 and mll of 256 works well for the "words.txt" test case
  integer, parameter :: hash_size=4001
  integer, parameter :: mkl=64         ! max key length
  integer, parameter :: mll=256        ! max size of linked lists
  integer, parameter :: mlv=4096       ! max length of dictionary value (for char array values)

  type Dict
    character(len=mkl), dimension(mll, hash_size) :: Keys
    integer, dimension(:, :), allocatable :: Values_int
    real(dp), dimension(:, :), allocatable :: Values_dble
    character(len=mlv), dimension(:, :), allocatable :: Values_char
    integer, dimension(hash_size) :: used
    integer :: vtype   ! 0: undefined;  1: integer;  2: double precision;  3: char array
  contains
    procedure :: print => PrintDict
    procedure :: initialize
    procedure :: free => FreeDict
    procedure :: get_keys
    procedure :: value => GetValue
    procedure :: add => AddToDict
  end type Dict

contains
  subroutine PrintSummary(MyDict, fid)
    class(Dict), intent(in) :: MyDict 
    integer, intent(in) :: fid
    integer :: i
    do i = 1, hash_size
      write(fid, *) i, MyDict%used(i)
    end do
  end subroutine PrintSummary


  ! ------------------------------------------------------------------------------------------------
  subroutine PrintDict(MyDict, fid_arg)
    class(Dict), intent(in) :: MyDict
    integer, optional :: fid_arg
    integer :: i, j, fid

    if (present(fid_arg)) then
      fid = fid_arg
    else
      fid = 6
    end if

    if (MyDict%vtype == 1) then
      do j = 1, hash_size
        do i = 1, MyDict%used(j)
          if (MyDict%Keys(i,j) /= "") then
            write(fid,*) trim(MyDict%Keys(i,j)), " :", MyDict%Values_int(i,j)
          end if
        end do
      end do
    elseif (MyDict%vtype == 2) then
      do j = 1, hash_size
        do i = 1, MyDict%used(j)
          if (MyDict%Keys(i,j) /= "") then
            write(fid,*) trim(MyDict%Keys(i,j)), " :", MyDict%Values_dble(i,j)
          end if
        end do
      end do
    elseif (MydIct%vtype == 3) then
      do j = 1, hash_size
        do i = 1, MyDict%used(j)
          if (MyDict%Keys(i,j) /= "") then
            write(fid,*) trim(MyDict%Keys(i,j)), " :", trim(MyDict%Values_char(i,j))
          end if
        end do
      end do
    end if

  end subroutine PrintDict


  ! ------------------------------------------------------------------------------------------------
  pure function GetHash(string_key) result(hash)
    integer, parameter :: ncs=4   ! number of characters to convert to a long integer at a time
    character(len=*), intent(in) :: string_key
    integer :: hash, c
    integer(8) :: mul, isum

    isum = 0
    do c = 1, len_trim(string_key)
      if (modulo(c,ncs) == 1) then
        mul = 1
      else
        mul = mul * 256
      end if
      isum = isum + ichar(string_key(c:c)) * mul
    end do
    hash = int(modulo(isum, hash_size) + 1)

  end function GetHash


  ! ------------------------------------------------------------------------------------------------
  pure function DoesKeyExist(MyDict, hash, xKey) result(indx)
    ! returns 0 if key does not exist in dictionary, otherwise
    !   returns the index of the key in the linked list
    type(Dict), intent(in) :: MyDict
    character(len=*), intent(in) :: xKey
    integer, intent(in) :: hash
    integer ::  u, indx

    indx = 0
    do u = 1, MyDict%used(hash)
      if (xKey == MyDict%Keys(u, hash)) then
        !write(0,*) "going to overwrite at u=", u
        ! key already exists, so will overwrite its existing corresponding value
        indx = u
        exit
      end if
    end do

  end function DoesKeyExist


  ! ------------------------------------------------------------------------------------------------
  subroutine initialize(MyDict, kind_val)
    class(Dict) :: MyDict
    class(*) :: kind_val
    integer :: i

    select type(kind_val)
    type is (integer)
        MyDict%vtype = 1
        allocate(MyDict%Values_int(mll, hash_size))
        !MyDict%Values_int(:,:) = 0
      type is (real(8))
        MyDict%vtype = 2
        allocate(MyDict%Values_dble(mll, hash_size))
        MyDict%Values_dble(:,:) = tiny(0d0)
      type is (character(len=*))
        MyDict%vtype = 3
        allocate(MyDict%Values_char(mll, hash_size))
        !MyDict%Values_char(:,:) = ""
      class default
        MyDict%vtype = 0
    end select

    do i = 1, hash_size
      MyDict%Keys(:, i) = ""
      MyDict%used(i) = 0
    end do

  end subroutine initialize


  ! ------------------------------------------------------------------------------------------------
  subroutine FreeDict(MyDict)
    class(Dict) :: MyDict
      if (MyDict%vtype == 1) then
        deallocate(MyDict%Values_int)
      elseif (MyDict%vtype == 2) then
        deallocate(MyDict%Values_dble)
      elseif (MyDict%vtype == 3) then
        deallocate(MyDict%Values_char)
      end iF
     
  end subroutine FreeDict


  ! ------------------------------------------------------------------------------------------------
  pure function get_keys(MyDict) result(keys)
    class(Dict), intent(in) :: MyDict
    character(len=mkl), dimension(:), allocatable :: keys, keys_sparse
    integer :: i, j, c

    c = 0
    allocate(keys_sparse(hash_size*mll))
    do j = 1, hash_size
      do i = 1, MyDict%used(j)
        if (MyDict%Keys(i,j) /= "") then
          c = c + 1
          keys_sparse(c) = MyDict%Keys(i,j)
        end if
      end do
    end do
    allocate(keys(c))
    keys(1:c) = keys_sparse(1:c)
    deallocate(keys_sparse)

  end function get_keys


  ! ------------------------------------------------------------------------------------------------
  subroutine GetValue(MyDict, xKey, xValue)
    class(Dict), intent(in) :: MyDict
    class(*) :: xValue
    character(len=*), intent(in) :: xKey
    integer :: hash, indx 

    hash = GetHash(xKey)
    indx = DoesKeyExist(MyDict, hash, xKey)
    if (indx == 0) then
      write(0,*)"ERROR: Requested key '" // trim(xKey) // "' is not in dict."
      write(0,'(I10)') "traceback"
      stop
    end if

    select typE(xValue)
      type is (integer)
        xValue = MyDict%Values_int(indx, hash)
      type is (real(dp))
        xValue = MyDict%Values_dble(indx, hash)
      type is (character(len=*))
        xValue = MyDict%Values_char(indx, hash)
      class default
    end select

  end subroutine GetValue


  ! ------------------------------------------------------------------------------------------------
  subroutine AddToDict(MyDict, xKey, xValue)
    class(Dict), intent(inout) :: MyDict
    class(*), intent(in) :: xValue
    character(len=*), intent(in) :: xKey
    integer :: hash, indx, xValue_int
    character(len=mlv) :: xValue_char
    real(dp) :: xValue_dble

    select type(xValue)
      type is (integer)
        xValue_int = xValue
      type is (real(dp))
        xValue_dble = xValue
      type is (character(len=*))
        xValue_char = xValue
      class default
    end select

    hash = GetHash(xKey)

    ! check if xKey is already in MyDict
    if (MyDict%used(hash) == 0) then
      ! no collision, so key does not already exist in dictionary
      MyDict%used(hash) = 1
      !write(0,*)"no collision"
      indx = 1
    else
      ! collision, so key may or may not already exist in dictionary
      !write(0,*)"collision!"
      indx = DoesKeyExist(MyDict, hash, xKey)
      if (indx == 0) then
        ! collision, but not from the same key, just coincidence
        if (MyDict%used(hash) == mll) then
          write(0,*)"ERROR: Too many collisions.  Recompile with larger 'mll' or 'hash_size' " // &
                    "parameter or alter the hash function."
          write(0,'(I10)') "traceback"
          stop
        end if
        MyDict%used(hash) = MyDict%used(hash)+1
        indx = MyDict%used(hash)
      !else
        ! collision because key already existed, so overwrite key's value
        !MyDict%used(hash) = indx 
      end if
    end if

    !write(0,*)"used, key, value:", MyDict%used(hash), indx, xKey, xValue
    MyDict%Keys(indx, hash) = xKey
    if (MyDict%vtype == 1) then
      MyDict%Values_int(indx, hash) = xValue_int
    elseif (MyDict%vtype == 2) then
      MyDict%Values_dble(indx, hash) = xValue_dble
    elseif (MyDict%vtype == 3) then
      MyDict%Values_char(indx, hash) = xValue_char
    end if

  end subroutine AddToDict

end module dictionary_mod
