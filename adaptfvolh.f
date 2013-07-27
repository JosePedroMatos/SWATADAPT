      function adaptfvolh(vol,res_id)

          use parm

          real :: vol, adaptfvolh
          integer :: cntr, res_id

          cntr = 0.
          adaptfvolh = -1.

          if (adapt_hv(res_id,1,1)/=0. .or.                             &
     &          adapt_hv(res_id,2,1)/=0.) then
            !specified height - volume curve
            do
                cntr = cntr + 1
                if (adapt_hv(res_id,cntr,2)>vol .or.                    &
     &              adapt_hv(res_id,cntr+1,2)==0.) exit
            end do
            adaptfvolh = adapt_hv(res_id,cntr-1,1)+(adapt_hv(res_id,cntr&
     &              ,1)-adapt_hv(res_id,cntr-1,1))/(adapt_hv(res_id,cntr&
     &              ,2)-adapt_hv(res_id,cntr-1,2))*(vol-adapt_hv(res_id,&
     &              cntr-1,2))
          elseif (adapt_b0(res_id)/=0. .and. adapt_b1(res_id)/=0. .and. &
     &              adapt_res_ph(res_id)/=0) then
            !height - volume linear relationship
            if (vol<=res_pvol(res_id)) then
                adaptfvolh=adapt_res_ph(res_id)-(1/adapt_b0(res_id)/    &
     &                10000.)*(res_pvol(res_id)-vol)
            else
                adaptfvolh=adapt_res_ph(res_id)+(1/adapt_b1(res_id)/    &
     &                10000.)*(vol-res_pvol(res_id))
            end if
          else
            !default
            if (vol<=res_pvol(res_id)) then
                adaptfvolh=1-1/res_pvol(res_id)*(res_pvol(res_id)-vol)
            else
                adaptfvolh=1+1/(res_evol(res_id)-res_pvol(res_id))*     &
     &              (vol-res_pvol(res_id))
            end if
          end if

          return
      end

