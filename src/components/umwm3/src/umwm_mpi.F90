MODULE UMWM_mpi
!======================================================================+
!                                                                      !
! DESCRIPTION: Provides MPI communication routines.                    !
!                                                                      !
! CONTAINS: haloCommBlocking                                           !
!           haloComm                                                   !
!           gatherArrayRank1                                           !
!           gatherArrayRank2                                           !
!           gatherArrayRank3                                           !
!                                                                      !
!======================================================================+
#ifdef MPI
USE mpi
USE UMWM_module
!======================================================================!
IMPLICIT NONE

INTEGER :: status(MPI_STATUS_SIZE)
INTEGER :: im_mod
INTEGER :: ilen

INTEGER,DIMENSION(:),ALLOCATABLE :: istart_,iend_,ilen_
INTEGER,DIMENSION(:),ALLOCATABLE :: iistart_,iiend_

INTEGER :: sendcount,recvcount
INTEGER :: sendtag,recvtag
INTEGER :: src,dest

!======================================================================!
CONTAINS



SUBROUTINE haloCommBlocking
!======================================================================!
!                                                                      !
! DESCRIPTION: Exchange halo points between processes.                 !
!              This version valid only for 1-cell halo width.          !
!                                                                      !
!======================================================================!

IF(nproc<size-1)THEN ! Communicate with process above:

  sendcount = om*pm*(iend-iistart_(nproc+1)+1) ; dest = nproc+1 ; sendtag = nproc
  recvcount = om*pm*(iiend-iend)               ; src  = nproc+1 ; recvtag = src

  CALL MPI_Sendrecv(e(:,:,iistart_(nproc+1):iend),sendcount,&
                    MPI_REAL,dest,sendtag,                  &
                    e(:,:,iend+1:iiend),recvcount,          &
                    MPI_REAL,src,recvtag,                   &
                    mpi_comm_world,status,ierr)
ENDIF

IF(nproc>0)THEN ! Communicate with process below:

  sendcount = om*pm*(iiend_(nproc-1)-istart+1) ; dest = nproc-1 ; sendtag = nproc
  recvcount = om*pm*(istart-iistart)           ; src  = nproc-1 ; recvtag = src

  CALL MPI_Sendrecv(e(:,:,istart:iiend_(nproc-1)),sendcount,&
                    MPI_REAL,dest,sendtag,                  &
                    e(:,:,iistart:istart-1),recvcount,      &
                    MPI_REAL,src,recvtag,                   &
                    mpi_comm_world,status,ierr)
ENDIF

!======================================================================!

IF(isGlobal)THEN ! If periodic domain

  IF(nproc==0)THEN

    sendcount = om*pm*first_col_len ; dest = size-1 ; sendtag = nproc
    recvcount = om*pm*last_col_len  ; src  = size-1 ; recvtag = src

    CALL MPI_Sendrecv(e(:,:,i_exchange_indices),sendcount,&
                      MPI_REAL,dest,sendtag,              &
                      e(:,:,iistart:istart-1),recvcount,  &
                      MPI_REAL,src,recvtag,               &
                      mpi_comm_world,status,ierr)

  ENDIF

  IF(nproc==size-1)THEN

    sendcount = om*pm*last_col_len  ; dest = 0 ; sendtag = nproc
    recvcount = om*pm*first_col_len ; src = 0  ; recvtag = src

    CALL MPI_Sendrecv(e(:,:,i_exchange_indices),sendcount,&
                      MPI_REAL,dest,sendtag,              &
                      e(:,:,iend+1:iiend),recvcount,      &
                      MPI_REAL,src,recvtag,               &
                      mpi_comm_world,status,ierr)

  ENDIF

ENDIF

ENDSUBROUTINE haloCommBlocking
!=======================================================================



SUBROUTINE haloComm(array)
!======================================================================+
!                                                                      !
! DESCRIPTION: Exchange halo points between processes.                 !
!              Valid for 1-cell halo width.                            !
!              Non-blocking implementation.                            !
!                                                                      !
!======================================================================+

REAL,DIMENSION(:,:,:),INTENT(INOUT) :: array

INTEGER,DIMENSION(2)                 :: requests
INTEGER,DIMENSION(MPI_STATUS_SIZE,2) :: statuses

INTEGER :: nn

!=======================================================================

requests = MPI_REQUEST_NULL

IF(nproc < size-1)THEN ! Communicate with process above:

  sendcount = om*pm*(iend-iistart_(nproc+1)+1) ; dest = nproc+1 ; sendtag = nproc
  recvcount = om*pm*(iiend-iend)               ; src  = nproc+1 ; recvtag = src

  CALL MPI_Irecv(array(:,:,iend+1:iiend),recvcount,&
                 MPI_REAL,src,recvtag,mpi_comm_world,requests(2),ierr)

  CALL MPI_Isend(array(:,:,iistart_(nproc+1):iend),sendcount,&
                 MPI_REAL,dest,sendtag,mpi_comm_world,requests(2),ierr)

  !CALL MPI_Wait(sendRequests(2),statuses(:,2),ierr)

ENDIF

IF(nproc > 0)THEN ! Communicate with process below:

  sendcount = om*pm*(iiend_(nproc-1)-istart+1) ; dest = nproc-1 ; sendtag = nproc
  recvcount = om*pm*(istart-iistart)           ; src  = nproc-1 ; recvtag = src

  CALL MPI_Irecv(array(:,:,iistart:istart-1),recvcount,&
                 MPI_REAL,src,recvtag,mpi_comm_world,requests(1),ierr)

  CALL MPI_Isend(array(:,:,istart:iiend_(nproc-1)),sendcount,&
                 MPI_REAL,dest,sendtag,mpi_comm_world,requests(1),ierr)

  !CALL MPI_Wait(sendRequests(1),statuses(:,1),ierr)

ENDIF

CALL MPI_Barrier(mpi_comm_world,ierr)

!IF(nproc<size-1)THEN
!  CALL MPI_Wait(recvRequests(2),statuses(:,2),ierr)
!ENDIF

!IF(nproc>0)THEN
!  CALL MPI_Wait(recvRequests(1),statuses(:,1),ierr)
!ENDIF

IF(nproc>0.AND.nproc<size-1)THEN
  CALL MPI_WaitAll(2,requests,statuses,ierr)
ELSEIF(nproc==0)THEN
  CALL MPI_Wait(requests(2),statuses(:,2),ierr)
ELSEIF(nproc==size-1)THEN
  CALL MPI_Wait(requests(1),statuses(:,1),ierr)
ENDIF

!CALL MPI_Wait(requests(1),statuses(:,1),ierr)
!CALL MPI_Wait(requests(2),statuses(:,2),ierr)

IF(isGlobal)THEN ! If periodic domain

  IF(nproc==0)THEN

    sendcount = om*pm*first_col_len ; dest = size-1 ; sendtag = nproc
    recvcount = om*pm*last_col_len  ; src  = size-1 ; recvtag = src

    CALL MPI_Isend(array(:,:,istart:(istart+first_col_len-1)),sendcount,&
                   MPI_REAL,dest,sendtag,mpi_comm_world,requests(1),ierr)

    CALL MPI_Irecv(array(:,:,iistart:istart-1),recvcount,&
                   MPI_REAL,src,recvtag,mpi_comm_world,requests(1),ierr)
   
    CALL MPI_Wait(requests(1),statuses(:,1),ierr)

  ENDIF

  IF(nproc==size-1)THEN

    sendcount = om*pm*last_col_len  ; dest = 0 ; sendtag = nproc
    recvcount = om*pm*first_col_len ; src = 0  ; recvtag = src

    CALL MPI_Isend(array(:,:,iend-last_col_len+1:iend),sendcount,&
                   MPI_REAL,dest,sendtag,mpi_comm_world,requests(2),ierr)

    CALL MPI_Irecv(array(:,:,iend+1:iiend),recvcount,&
                   MPI_REAL,src,recvtag,mpi_comm_world,requests(2),ierr)

    CALL MPI_Wait(requests(2),statuses(:,2),ierr)

  ENDIF

ENDIF

ENDSUBROUTINE haloComm
!=======================================================================



SUBROUTINE gatherArrayRank1(srcArray,tgtArray)
!======================================================================+
!                                                                      !
! DESCRIPTION: Gathers an array of shape (im) to root processor.       !
!              Non-blocking implementation.                            !
!                                                                      !
!======================================================================+

REAL,DIMENSION(istart:iend),INTENT(IN)  :: srcArray
REAL,DIMENSION(im),         INTENT(OUT) :: tgtArray

INTEGER,DIMENSION(size-1)                 :: requests
INTEGER,DIMENSION(MPI_STATUS_SIZE,size-1) :: statuses

INTEGER :: nn

!=======================================================================

IF(nproc == 0)THEN
  tgtArray(istart:iend) = srcArray(istart:iend)
  DO nn=1,size-1
    CALL MPI_Irecv(tgtArray(istart_(nn):iend_(nn)),ilen_(nn),&
                   MPI_REAL,nn,nn,mpi_comm_world,requests(nn),ierr)
  ENDDO
ENDIF

CALL MPI_Barrier(mpi_comm_world,ierr)
  
IF(nproc == 0)THEN
  CALL MPI_WaitAll(size-1,requests,statuses,ierr)
ELSE
  CALL MPI_Isend(srcArray(istart:iend),ilen,&
                 MPI_REAL,0,nproc,mpi_comm_world,requests(nproc),ierr)
ENDIF

ENDSUBROUTINE gatherArrayRank1
!=======================================================================



SUBROUTINE gatherArrayRank2(srcArray,tgtArray)
!=======================================================================
!                                                                      !
! DESCRIPTION: Gathers an array of shape (om,im) to root processor.    !
!              Non-blocking implementation.                            !
!                                                                      !
!=======================================================================

REAL,DIMENSION(om,istart:iend),INTENT(IN)  :: srcArray
REAL,DIMENSION(om,im),         INTENT(OUT) :: tgtArray

INTEGER,DIMENSION(size-1)                 :: requests
INTEGER,DIMENSION(MPI_STATUS_SIZE,size-1) :: statuses

INTEGER :: nn

!=======================================================================

IF(nproc == 0)THEN
  tgtArray(:,istart:iend) = srcArray(:,istart:iend)
  DO nn=1,size-1
    CALL MPI_Irecv(tgtArray(:,istart_(nn):iend_(nn)),om*ilen_(nn),&
                   MPI_REAL,nn,nn,mpi_comm_world,requests(nn),ierr)
  ENDDO
ENDIF

CALL MPI_Barrier(mpi_comm_world,ierr)

IF(nproc == 0)THEN
  CALL MPI_WaitAll(size-1,requests,statuses,ierr)
ELSE
  CALL MPI_Isend(srcArray(:,istart:iend),om*ilen,&
                 MPI_REAL,0,nproc,mpi_comm_world,requests(nproc),ierr)
ENDIF

ENDSUBROUTINE gatherArrayRank2
!=======================================================================



SUBROUTINE gatherArrayRank3(srcArray,tgtArray)
!=======================================================================
!                                                                      !
! DESCRIPTION: Gathers an array of shape (om,pm,im) to root processor. !
!              Non-blocking implementation.                            !
!                                                                      !
!=======================================================================

REAL,DIMENSION(om,pm,istart:iend),INTENT(IN)  :: srcArray
REAL,DIMENSION(om,pm,im),         INTENT(OUT) :: tgtArray

INTEGER,DIMENSION(size-1)                 :: requests
INTEGER,DIMENSION(MPI_STATUS_SIZE,size-1) :: statuses

INTEGER :: nn

!=======================================================================

IF(nproc == 0)THEN
  tgtArray(:,:,istart:iend) = srcArray(:,:,istart:iend)
  DO nn=1,size-1
    CALL MPI_Irecv(tgtArray(:,:,istart_(nn):iend_(nn)),om*pm*ilen_(nn),&
                   MPI_REAL,nn,nn,mpi_comm_world,requests(nn),ierr)
  ENDDO
ENDIF

CALL MPI_Barrier(mpi_comm_world,ierr)

IF(nproc == 0)THEN
  CALL MPI_WaitAll(size-1,requests,statuses,ierr)
ELSE
  CALL MPI_Isend(srcArray(:,:,istart:iend),om*pm*ilen,&
                 MPI_REAL,0,nproc,mpi_comm_world,requests(nproc),ierr)
ENDIF

ENDSUBROUTINE gatherArrayRank3
!=======================================================================
#endif
ENDMODULE UMWM_mpi
