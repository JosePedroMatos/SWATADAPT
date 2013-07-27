      function adaptfsurh(vol, h, res_id)

          use parm

          real :: h, adaptfsurh
          integer :: cntr, res_id

          cntr = 0.
          adaptfsurh = -1.
          !!vol = vol/10000 !! m**3 => 10**4 m**3

          if (adapt_ha(res_id,1,1)/=0. .or.                             &
     &          adapt_ha(res_id,2,1)/=0.) then
            !! there is a volume-area curve
            do
                cntr = cntr + 1
                !!print *, cntr, "  ", adapt_ha(res_id,cntr,1), "  ", vol
                if (adapt_ha(res_id,cntr,2)>vol .or.                    &
     &              adapt_ha(res_id,cntr+1,2)==0.) exit
            end do
            adaptfsurh = adapt_ha(res_id,cntr-1,2)+(adapt_ha(res_id,cntr&
     &              ,2)-adapt_ha(res_id,cntr-1,2))/(adapt_ha(res_id,cntr&
     &              ,1)-adapt_ha(res_id,cntr-1,1))*(h-adapt_ha(res_id,  &
     &              cntr-1,1))
          else
            !! there is no volume-area curve
            adaptfsurh = br1(res_id) * vol ** br2(res_id)
            !!print *, vol, "  ", br1(res_id), "  ", br2(res_id), "  ",   &
     !!&          adaptfsurh
          endif

          return
      end
