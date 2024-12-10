module WIND_RADII 

implicit none

contains

subroutine get_thresh_radii(x, y, field, xc, yc, thresh, minmax, rad1, rad2, rad3, rad4)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! Get the 4-quadrant exceedence radii of "field" above (minmax=0) or below (minmax=1) the "thresh" value.
!!
!!INPUT
!! x, y  :  2-D arrays giving the x and y grid (lon,lat) of the "field" values.  Does NOT need to be a regular grid.
!!           BUT it is assummed that consecutive x,y points are adjacent in space.
!! field :  values of the field of interest (2-D array)
!! cx, cy : center to base the radii on.  Most sensibly, this is the storm center.
!! minmax:  = 0 ==> the threshold is a minimum value (e.g., winds and rain)
!!          = 1 ==> the threshold is a maximum value (e.g., pressure)
!!
!!OUTPUT
!! rad1, rad2, rad3, rad4  :  the 4-quadrant radii (real).   Mathematical convention, Q1 is NE, Q2 is NW, ect.
!!     NOTE: Units are in the same units as x, y (probably lon/lat degrees).
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  BK 2010.07.09
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!implicit none


real, dimension(:,:)::x,y,field
real, intent(in)::xc, yc, thresh
integer, intent(in)::minmax

real, intent(out)::rad1, rad2, rad3, rad4




!!! LOCAL VARIABLES

integer::imax,jmax,ii,jj
integer, dimension(:,:), allocatable::feature,check   !!! feature is 0 or 1 ; quads is 1,2,3,4.
real, dimension(:,:), allocatable::dist   !! keep distance from (cx, cy)
integer, dimension(2)::mindistloc
integer::minlocx, minlocy
real::thismindist, lastmindist
logical::keeplooking



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

imax=size(x,1)
jmax=size(x,2) 


rad1=0.
rad2=0.
rad3=0.
rad4=0.


!!!!  Calculate distance array (dist)
allocate(dist(imax,jmax))
dist=0.0


do ii=1,imax
   do jj=1,jmax
      dist(ii,jj) = sqrt(  (x(ii,jj)-xc)**2 + (y(ii,jj)-yc)**2  )
   enddo
enddo



!!!!  We start the "feature" detection with a seed point.
!!!!  The seed point is all points within 3 deg. that satisfy the criteria.
!!!!  (If no points satisfy the criteria, then quit and return zeros.)
!!!!  Once the point is found, then look for contiguous points that satisfy.


allocate(feature(imax,jmax))
allocate(check(imax,jmax))


feature = 0
check=0


where ( dist .lt. 2.0 .and. field .gt. thresh-0.001 ) feature=1 

!!!   Now, expand to find the feature.
!!!   Loop over entire grid.  Whenever a point is adjacent to a "1," give the point a "1."
!!!     repeat until no more new "1" points found.

keeplooking=.true.

do while ( keeplooking )

   keeplooking=.false.

   do ii=2,imax-1
      do jj=2,jmax-1


         if ( minmax .eq. 0 ) then

            if ( feature(ii,jj) .eq. 0 .and. field(ii,jj) .gt. thresh-0.001 .and.    &
                 &   (feature(ii-1,jj) .eq. 1 .or. feature(ii+1,jj) .eq. 1 .or. &
                 &   feature(ii,jj-1) .eq. 1 .or. feature(ii,jj+1) .eq. 1 ) ) then
               
               feature(ii,jj) = 1
               keeplooking=.true.               

            endif

         else
  
            if ( feature(ii,jj) .eq. 0 .and. field(ii,jj) .lt. thresh+0.001 .and.    &
                 &   (feature(ii-1,jj) .eq. 1 .or. feature(ii+1,jj) .eq. 1 .or. &
                 &   feature(ii,jj-1) .eq. 1 .or. feature(ii,jj+1) .eq. 1 ) ) then
               
               feature(ii,jj) = 1
               keeplooking=.true.     

            endif

         endif

      enddo
   enddo

end do



!!! Find the maximum distance by quadrant.  Quadrant is determined by using the x, y data.


if (count(  feature .eq. 1 .and. x .gt. xc .and. y .gt. yc ) .gt. 0) then
   rad1=maxval(dist,  feature .eq. 1 .and. x .gt. xc .and. y .gt. yc )   
endif

if (count(  feature .eq. 1 .and. x .lt. xc .and. y .gt. yc ) .gt. 0)  then
   rad2=maxval(dist,  feature .eq. 1 .and. x .lt. xc .and. y .gt. yc )   
endif

if (count(  feature .eq. 1 .and. x .lt. xc .and. y .lt. yc ) .gt. 0)  then
   rad3=maxval(dist,  feature .eq. 1 .and. x .lt. xc .and. y .lt. yc )   
endif

if (count(  feature .eq. 1 .and. x .gt. xc .and. y .lt. yc ) .gt. 0)   then
   rad4=maxval(dist,  feature .eq. 1 .and. x .gt. xc .and. y .lt. yc )   
endif


deallocate(feature)
deallocate(dist)


return

end subroutine get_thresh_radii


end module WIND_RADII 

