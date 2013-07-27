      function adaptfmaxd(h, res_id)

          use parm

          real :: h, adaptfmaxd
          integer :: cntr, res_id

          cntr = 0.
          adaptfmaxd = 0.

          if (adapt_md(res_id,1,1)/=0. .or.                             &
     &          adapt_md(res_id,2,1)/=0.) then
            !specified height - area curve
            do
                cntr = cntr + 1
                !!print *, cntr, "  ", adapt_md(res_id,cntr,1), "  ",     &
     !!&              adapt_md(res_id,cntr,2)
                if (adapt_md(res_id,cntr,2)>h .or.                      &
     &              adapt_md(res_id,cntr+1,2)==0.) exit
            end do

            adaptfmaxd = adapt_md(res_id,cntr-1,2)+(adapt_md(res_id,cntr&
     &              ,2)-adapt_md(res_id,cntr-1,2))/(adapt_md(res_id,cntr&
     &              ,1)-adapt_md(res_id,cntr-1,1))*(h-adapt_md(res_id,  &
     &              cntr-1,1))
          endif

          if (oflowmx(i_mo, res_id) > 0. .and. adaptfmaxd > 0.) then
            adaptfmaxd = min(adaptfmaxd, oflowmx(i_mo, res_id))
          elseif (oflowmx(i_mo, res_id) > 0.) then
            adaptfmaxd = oflowmx(i_mo, res_id)
          endif

          return
      end
