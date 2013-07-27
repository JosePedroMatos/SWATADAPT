# Variables
objectsMain = main.o \
caps.o hruallo.o getallo.o \
zero0.o zero1.o zero2.o zeroini.o zero_urbn.o allocate_parms.o \
gcycl.o aunif.o readfile.o readfileadapt.o\
ascrv.o readbsn.o \
readwwq.o \
readfcst.o \
readplant.o \
readtill.o \
readpest.o \
readfert.o \
readurban.o \
readseptwq.o \
jdt.o readlup.o \
readru.o readcnst.o xmon.o readyr.o readmon.o readres.o readlwq.o lwqdef.o readrte.o readswq.o readsno.o readsepticbz.o readsdr.o readhru.o readchm.o readmgt.o layersplit.o estimate_ksat.o readsol.o readgw.o readops.o dstn1.o readwgn.o bmpinit.o readpnd.o readwus.o readsub.o readfig.o \
readatmodep.o \
soil_chem.o curno.o soil_phys.o rteinit.o h2omgt_init.o qman.o ttcoef.o hydroinit.o impnd_init.o readinpt.o \
std1.o \
std2.o \
openwth.o \
header.o headout.o \
storeinitial.o \
sim_inityr.o std3.o sim_initday.o wndgen.o ee.o atri.o rhgen.o clgen.o slrgen.o weatgn.o tgen.o pgen.o wmeas.o hmeas.o smeas.o tmeas.o expo.o pgenhr.o pmeas.o clicon.o resetlu.o percmicro.o sat_excess.o origtile.o depstor.o drains.o percmacro.o percmain.o routels.o sumhyd.o routeunit.o saveconc.o apex_day.o structure.o reccnst.o recday.o save.o recyear.o recmon.o rechour.o print_hyd.o addh.o transfer.o resinit.o irrigate.o irr_res.o reshr.o res.o adaptfvolh.o adaptfsurh.o adaptfmaxd.o resnut.o lakeq.o routres.o rchinit.o rtday.o rtsed.o rthsed.o noqual.o hhnoqual.o rtpest.o rthpest.o theta.o rtbact.o irr_rch.o rchuse.o rtout.o rtmusk.o rtsed_yangsand.o rtsed_Molinas_Wu.o rtsed_kodatie.o rtsed_bagnold.o watqual.o watqual2.o hhwatqual.o rthr.o rthmusk.o route.o sub_subbasin.o varinit.o water_hru.o ttcoef_wway.o schedule_ops.o albedo.o solt.o canopyint.o snom.o dailycn.o surq_daycn.o surq_greenampt.o volq.o surfst_h2o.o alph.o pkq.o tran.o eiusle.o ovr_sed.o cfactor.o ysed.o crackflow.o crackvol.o surface.o autoirr.o etpot.o etact.o wattable.o confert.o conapply.o graze.o swu.o tstr.o nfix.o nuts.o nup.o npup.o anfert.o grow.o plantmod.o nminrl.o nitvol.o pminrl2.o pminrl.o biozone.o gwmod.o gwmod_deep.o washp.o decay.o pestlch.o enrsb.o pesty.o orgn.o psed.o nrain.o nlch.o solp.o subwq.o bacteria.o regres.o urban.o sweep.o urbanhr.o latsed.o gwnutr.o gw_no3.o surfstor.o substor.o filter.o wetlan.o pond.o hrupond.o pondhr.o hrupondhr.o pothole.o urb_bmp.o watuse.o watbal.o sumv.o tair.o bmp_ri_pond.o bmp_sed_pond.o bmp_sand_filter.o distrib_bmps.o icl.o subday.o impndday.o hruday.o virtual.o NCsed_leach.o orgncswat.o bmpfixed.o grass_wway.o filtw.o buffer.o ndenit.o carbon_zhang2.o carbon_new.o dormant.o subbasin.o bmp_wet_pond.o bmp_det_pond.o command.o rchday.o rseday.o writed.o hrumon.o impndmon.o submon.o rchmon.o rsedmon.o hruyr.o impndyr.o subyr.o rchyr.o rsedyr.o writea.o writem.o tillfactor.o newtillmix.o burnop.o rootfr.o killop.o harvgrainop.o harvkillop.o apply.o fert.o irrsub.o plantop.o harvestop.o sched_mgt.o operatn.o soil_write.o simulate.o \
swbl.o vbl.o finalbal.o \
rchaa.o rsedaa.o hruaa.o impndaa.o subaa.o stdaa.o writeaa.o \
pestw.o \
rewind_init.o

# Options
#compilerOpt = -fast
compilerOpt = -debug full
	
# All
all: SWAT

# Main file
SWAT: $(objectsMain) 
	ifort $(compilerOpt) -o SWAT $(objectsMain)

SWATdebug: $(objectsMain) 
	ifort $(compilerOpt) -o SWATdebug $(objectsMain)

# First order files
parm.mod: modparm.o
	ifort $(compilerOpt) -c modparm.f	
modparm.o: modparm.f
	ifort $(compilerOpt) -c modparm.f
main.o: parm.mod main.f
	ifort $(compilerOpt) -c main.f
getallo.o: parm.mod caps.o hruallo.o getallo.f
	ifort $(compilerOpt) -c getallo.f
allocate_parms.o: parm.mod zero0.o zero1.o zero2.o zeroini.o zero_urbn.o allocate_parms.f
	ifort $(compilerOpt) -c allocate_parms.f
readfile.o: parm.mod gcycl.o aunif.o readfile.f
	ifort $(compilerOpt) -c readfile.f
readfileadapt.o: parm.mod readfileadapt.f
	ifort $(compilerOpt) -c readfileadapt.f	
readbsn.o: parm.mod ascrv.o readbsn.f
	ifort $(compilerOpt) -c readbsn.f
readwwq.o: parm.mod readwwq.f
	ifort $(compilerOpt) -c readwwq.f
readfcst.o: parm.mod readfcst.f
	ifort $(compilerOpt) -c readfcst.f
readplant.o: parm.mod readplant.f
	ifort $(compilerOpt) -c readplant.f
readtill.o: parm.mod readtill.f
	ifort $(compilerOpt) -c readtill.f
readpest.o: parm.mod readpest.f
	ifort $(compilerOpt) -c readpest.f
readfert.o: parm.mod readfert.f
	ifort $(compilerOpt) -c readfert.f
readurban.o: parm.mod regres.o readurban.f
	ifort $(compilerOpt) -c readurban.f
readseptwq.o: parm.mod readseptwq.f
	ifort $(compilerOpt) -c readseptwq.f
readlup.o: parm.mod readlup.f
	ifort $(compilerOpt) -c jdt.o readlup.f
readfig.o: parm.mod readru.o readcnst.o xmon.o readyr.o readmon.o readres.o readlwq.o lwqdef.o readrte.o readswq.o readsub.o readfig.f
	ifort $(compilerOpt) -c readfig.f
readatmodep.o: parm.mod readatmodep.f
	ifort $(compilerOpt) -c readatmodep.f
readinpt.o: parm.mod soil_chem.o soil_phys.o rteinit.o h2omgt_init.o hydroinit.o impnd_init.o readinpt.f
	ifort $(compilerOpt) -c readinpt.f
std1.o: parm.mod std1.f
	ifort $(compilerOpt) -c std1.f
std2.o: parm.mod std2.f
	ifort $(compilerOpt) -c std2.f
openwth.o: parm.mod openwth.f
	ifort $(compilerOpt) -c openwth.f
headout.o: parm.mod header.o headout.f
	ifort $(compilerOpt) -c headout.f
storeinitial.o: parm.mod storeinitial.f
	ifort $(compilerOpt) -c storeinitial.f
simulate.o: parm.mod sim_inityr.o std3.o sim_initday.o wndgen.o rhgen.o clgen.o slrgen.o weatgn.o tgen.o pgen.o wmeas.o hmeas.o smeas.o tmeas.o pmeas.o clicon.o resetlu.o command.o writed.o writem.o tillfactor.o newtillmix.o sched_mgt.o operatn.o soil_write.o simulate.f
	ifort $(compilerOpt) -c simulate.f
finalbal.o: parm.mod swbl.o vbl.o finalbal.f
	ifort $(compilerOpt) -c finalbal.f
writeaa.o: parm.mod rchaa.o rsedaa.o hruaa.o impndaa.o subaa.o stdaa.o writeaa.f
	ifort $(compilerOpt) -c writeaa.f
pestw.o: parm.mod pestw.f
	ifort $(compilerOpt) -c pestw.f
rewind_init.o: parm.mod rewind_init.f
	ifort $(compilerOpt) -c rewind_init.f
		
# Second order files
caps.o: caps.f
	ifort $(compilerOpt) -c caps.f
hruallo.o: parm.mod hruallo.f
	ifort $(compilerOpt) -c hruallo.f

zero0.o: parm.mod zero0.f
	ifort $(compilerOpt) -c zero0.f
zero1.o: parm.mod zero1.f
	ifort $(compilerOpt) -c zero1.f
zero2.o: parm.mod zero2.f
	ifort $(compilerOpt) -c zero2.f
zeroini.o: parm.mod zeroini.f
	ifort $(compilerOpt) -c zeroini.f
zero_urbn.o: parm.mod zero_urbn.f
	ifort $(compilerOpt) -c zero_urbn.f
	
gcycl.o: parm.mod gcycl.f
	ifort $(compilerOpt) -c gcycl.f
aunif.o: parm.mod aunif.f
	ifort $(compilerOpt) -c aunif.f

ascrv.o: parm.mod ascrv.f
	ifort $(compilerOpt) -c ascrv.f

jdt.o: parm.mod jdt.f
	ifort $(compilerOpt) -c jdt.f

readru.o: parm.mod readru.f
	ifort $(compilerOpt) -c readru.f
readcnst.o: parm.mod readcnst.f
	ifort $(compilerOpt) -c readcnst.f
xmon.o: parm.mod xmon.f
	ifort $(compilerOpt) -c xmon.f
readyr.o: parm.mod readyr.f
	ifort $(compilerOpt) -c readyr.f
readmon.o: parm.mod readmon.f
	ifort $(compilerOpt) -c readmon.f
readres.o: parm.mod readres.f
	ifort $(compilerOpt) -c readres.f
readlwq.o: parm.mod readlwq.f
	ifort $(compilerOpt) -c readlwq.f
lwqdef.o: parm.mod lwqdef.f
	ifort $(compilerOpt) -c lwqdef.f
readrte.o: parm.mod readrte.f
	ifort $(compilerOpt) -c readrte.f
readswq.o: parm.mod readswq.f
	ifort $(compilerOpt) -c readswq.f
readsub.o: parm.mod readsno.o readsepticbz.o readsdr.o readhru.o readchm.o readmgt.o readsol.o readgw.o readops.o readwgn.o readpnd.o readwus.o readsub.f
	ifort $(compilerOpt) -c readsub.f

soil_chem.o: parm.mod soil_chem.f
	ifort $(compilerOpt) -c soil_chem.f
soil_phys.o: parm.mod curno.o soil_phys.f
	ifort $(compilerOpt) -c soil_phys.f
rteinit.o: parm.mod rteinit.f
	ifort $(compilerOpt) -c rteinit.f
h2omgt_init.o: parm.mod h2omgt_init.f
	ifort $(compilerOpt) -c h2omgt_init.f
hydroinit.o: parm.mod  ttcoef.o hydroinit.f
	ifort $(compilerOpt) -c hydroinit.f
impnd_init.o: parm.mod impnd_init.f
	ifort $(compilerOpt) -c impnd_init.f

header.o: parm.mod header.f
	ifort $(compilerOpt) -c header.f

sim_inityr.o: parm.mod sim_inityr.f
	ifort $(compilerOpt) -c sim_inityr.f
std3.o: parm.mod std3.f
	ifort $(compilerOpt) -c std3.f
sim_initday.o: parm.mod sim_initday.f
	ifort $(compilerOpt) -c sim_initday.f
clicon.o: parm.mod wndgen.o rhgen.o clgen.o slrgen.o weatgn.o tgen.o pgen.o wmeas.o hmeas.o smeas.o tmeas.o pmeas.o clicon.f
	ifort $(compilerOpt) -c clicon.f
resetlu.o: parm.mod resetlu.f
	ifort $(compilerOpt) -c resetlu.f
command.o: parm.mod routels.o sumhyd.o routeunit.o saveconc.o apex_day.o structure.o reccnst.o recday.o save.o recyear.o recmon.o rechour.o print_hyd.o addh.o transfer.o routres.o route.o subbasin.o command.f
	ifort $(compilerOpt) -c command.f
writed.o: parm.mod rchday.o rseday.o writed.f
	ifort $(compilerOpt) -c writed.f
writem.o: parm.mod hrumon.o impndmon.o submon.o rchmon.o rsedmon.o writea.o writem.f
	ifort $(compilerOpt) -c writem.f
newtillmix.o: parm.mod tillfactor.o newtillmix.f
	ifort $(compilerOpt) -c newtillmix.f
operatn.o: parm.mod sched_mgt.o operatn.f
	ifort $(compilerOpt) -c operatn.f
soil_write.o: parm.mod soil_write.f
	ifort $(compilerOpt) -c soil_write.f
	
regres.o: parm.mod regres.f
	ifort $(compilerOpt) -c regres.f

swbl.o: parm.mod swbl.f
	ifort $(compilerOpt) -c swbl.f
vbl.o: parm.mod vbl.f
	ifort $(compilerOpt) -c vbl.f

rchaa.o: parm.mod rchaa.f
	ifort $(compilerOpt) -c rchaa.f
rsedaa.o: parm.mod rsedaa.f
	ifort $(compilerOpt) -c rsedaa.f
hruaa.o: parm.mod hruaa.f
	ifort $(compilerOpt) -c hruaa.f
impndaa.o: parm.mod impndaa.f
	ifort $(compilerOpt) -c impndaa.f
subaa.o: parm.mod subaa.f
	ifort $(compilerOpt) -c subaa.f
stdaa.o: parm.mod stdaa.f
	ifort $(compilerOpt) -c stdaa.f
	
# Third order files
readsno.o: parm.mod readsno.f
	ifort $(compilerOpt) -c readsno.f
readsepticbz.o: parm.mod readsepticbz.f
	ifort $(compilerOpt) -c readsepticbz.f
readsdr.o: parm.mod readsdr.f
	ifort $(compilerOpt) -c readsdr.f
readhru.o: parm.mod readhru.f
	ifort $(compilerOpt) -c readhru.f
readchm.o: parm.mod readchm.f
	ifort $(compilerOpt) -c readchm.f
readmgt.o: parm.mod readmgt.f
	ifort $(compilerOpt) -c readmgt.f
readsol.o: parm.mod layersplit.o estimate_ksat.o readsol.f
	ifort $(compilerOpt) -c readsol.f
readgw.o: parm.mod readgw.f
	ifort $(compilerOpt) -c readgw.f
readops.o: parm.mod readops.f
	ifort $(compilerOpt) -c readops.f
readwgn.o: parm.mod dstn1.o readwgn.f
	ifort $(compilerOpt) -c readwgn.f
readpnd.o: parm.mod bmpinit.o readpnd.f
	ifort $(compilerOpt) -c readpnd.f
readwus.o: parm.mod readwus.f
	ifort $(compilerOpt) -c readwus.f

curno.o: parm.mod curno.f
	ifort $(compilerOpt) -c curno.f

ttcoef.o: parm.mod qman.o ttcoef.f
	ifort $(compilerOpt) -c ttcoef.f

wndgen.o: parm.mod wndgen.f
	ifort $(compilerOpt) -c wndgen.f
rhgen.o: parm.mod ee.o atri.o rhgen.f
	ifort $(compilerOpt) -c rhgen.f
clgen.o: parm.mod clgen.f
	ifort $(compilerOpt) -c clgen.f
slrgen.o: parm.mod slrgen.f
	ifort $(compilerOpt) -c slrgen.f
weatgn.o: parm.mod weatgn.f
	ifort $(compilerOpt) -c weatgn.f
tgen.o: parm.mod tgen.f
	ifort $(compilerOpt) -c tgen.f
pgen.o: parm.mod pgen.f
	ifort $(compilerOpt) -c pgen.f
wmeas.o: parm.mod wmeas.f
	ifort $(compilerOpt) -c wmeas.f
hmeas.o: parm.mod hmeas.f
	ifort $(compilerOpt) -c hmeas.f
smeas.o: parm.mod smeas.f
	ifort $(compilerOpt) -c smeas.f
tmeas.o: parm.mod tmeas.f
	ifort $(compilerOpt) -c tmeas.f
pmeas.o: parm.mod pgenhr.o pmeas.f
	ifort $(compilerOpt) -c pmeas.f

routels.o: parm.mod percmain.o routels.f
	ifort $(compilerOpt) -c routels.f
sumhyd.o: parm.mod sumhyd.f
	ifort $(compilerOpt) -c sumhyd.f
routeunit.o: parm.mod routeunit.f
	ifort $(compilerOpt) -c routeunit.f
saveconc.o: parm.mod saveconc.f
	ifort $(compilerOpt) -c saveconc.f
apex_day.o: parm.mod apex_day.f
	ifort $(compilerOpt) -c apex_day.f
structure.o: parm.mod structure.f
	ifort $(compilerOpt) -c structure.f
reccnst.o: parm.mod reccnst.f
	ifort $(compilerOpt) -c reccnst.f
recday.o: parm.mod recday.f
	ifort $(compilerOpt) -c recday.f
save.o: parm.mod save.f
	ifort $(compilerOpt) -c save.f
recyear.o: parm.mod recyear.f
	ifort $(compilerOpt) -c recyear.f
recmon.o: parm.mod recmon.f
	ifort $(compilerOpt) -c recmon.f
rechour.o: parm.mod rechour.f
	ifort $(compilerOpt) -c rechour.f
print_hyd.o: parm.mod print_hyd.f
	ifort $(compilerOpt) -c print_hyd.f
addh.o: parm.mod addh.f
	ifort $(compilerOpt) -c addh.f
transfer.o: parm.mod transfer.f
	ifort $(compilerOpt) -c transfer.f
routres.o: parm.mod resinit.o irr_res.o reshr.o res.o resnut.o lakeq.o routres.f
	ifort $(compilerOpt) -c routres.f
route.o: parm.mod rchinit.o rtday.o rtsed.o rthsed.o noqual.o hhnoqual.o rtpest.o rthpest.o rtbact.o irr_rch.o rchuse.o rtout.o rtmusk.o rtsed_yangsand.o rtsed_Molinas_Wu.o rtsed_kodatie.o rtsed_bagnold.o watqual.o watqual2.o hhwatqual.o rthr.o rthmusk.o route.f
	ifort $(compilerOpt) -c route.f
	
subbasin.o: parm.mod sub_subbasin.o varinit.o water_hru.o schedule_ops.o albedo.o solt.o surface.o autoirr.o etpot.o etact.o wattable.o confert.o conapply.o graze.o plantmod.o nminrl.o nitvol.o pminrl2.o pminrl.o biozone.o gwmod.o gwmod_deep.o washp.o decay.o pestlch.o enrsb.o pesty.o orgn.o psed.o nrain.o nlch.o solp.o subwq.o bacteria.o urban.o urbanhr.o latsed.o gwnutr.o gw_no3.o surfstor.o substor.o filter.o wetlan.o hrupond.o hrupondhr.o pothole.o urb_bmp.o watuse.o watbal.o sumv.o virtual.o NCsed_leach.o orgncswat.o bmpfixed.o grass_wway.o filtw.o buffer.o carbon_zhang2.o carbon_new.o dormant.o subbasin.f
	ifort $(compilerOpt) -c subbasin.f
bmp_wet_pond.o: parm.mod bmp_wet_pond.f
	ifort $(compilerOpt) -c bmp_wet_pond.f
bmp_det_pond.o: parm.mod bmp_det_pond.f
	ifort $(compilerOpt) -c bmp_det_pond.f

rchday.o: parm.mod rchday.f
	ifort $(compilerOpt) -c rchday.f
rseday.o: parm.mod rseday.f
	ifort $(compilerOpt) -c rseday.f

hrumon.o: parm.mod hrumon.f
	ifort $(compilerOpt) -c hrumon.f
impndmon.o: parm.mod impndmon.f
	ifort $(compilerOpt) -c impndmon.f
submon.o: parm.mod submon.f
	ifort $(compilerOpt) -c submon.f
rchmon.o: parm.mod rchmon.f
	ifort $(compilerOpt) -c rchmon.f
rsedmon.o: parm.mod rsedmon.f
	ifort $(compilerOpt) -c rsedmon.f
writea.o: parm.mod hruyr.o impndyr.o subyr.o rchyr.o rsedyr.o writea.f
	ifort $(compilerOpt) -c writea.f

tillfactor.o: parm.mod tillfactor.f
	ifort $(compilerOpt) -c tillfactor.f

sched_mgt.o: parm.mod burnop.o killop.o harvgrainop.o harvkillop.o apply.o fert.o irrsub.o plantop.o harvestop.o sched_mgt.f
	ifort $(compilerOpt) -c sched_mgt.f

# Forth order
layersplit.o: parm.mod layersplit.f
	ifort $(compilerOpt) -c layersplit.f
estimate_ksat.o: parm.mod estimate_ksat.f
	ifort $(compilerOpt) -c estimate_ksat.f

dstn1.o: parm.mod dstn1.f
	ifort $(compilerOpt) -c dstn1.f

bmpinit.o: parm.mod bmpinit.f
	ifort $(compilerOpt) -c bmpinit.f

qman.o: parm.mod qman.f
	ifort $(compilerOpt) -c qman.f

ee.o: parm.mod ee.f
	ifort $(compilerOpt) -c ee.f
atri.o: parm.mod atri.f
	ifort $(compilerOpt) -c atri.f

pgenhr.o: parm.mod expo.o pgenhr.f
	ifort $(compilerOpt) -c pgenhr.f

percmain.o: parm.mod percmicro.o sat_excess.o origtile.o drains.o percmacro.o percmain.f
	ifort $(compilerOpt) -c percmain.f

resinit.o: parm.mod resinit.f
	ifort $(compilerOpt) -c resinit.f
irr_res.o: parm.mod irrigate.o irr_res.f
	ifort $(compilerOpt) -c irr_res.f
reshr.o: parm.mod reshr.f
	ifort $(compilerOpt) -c reshr.f
res.o: parm.mod adaptfvolh.o adaptfsurh.o adaptfmaxd.o res.f
	ifort $(compilerOpt) -c res.f
resnut.o: parm.mod resnut.f
	ifort $(compilerOpt) -c resnut.f
lakeq.o: parm.mod lakeq.f
	ifort $(compilerOpt) -c lakeq.f

rchinit.o: parm.mod rchinit.f
	ifort $(compilerOpt) -c rchinit.f
rtday.o: parm.mod rtday.f
	ifort $(compilerOpt) -c rtday.f
rtsed.o: parm.mod rtsed.f
	ifort $(compilerOpt) -c rtsed.f
rthsed.o: parm.mod rthsed.f
	ifort $(compilerOpt) -c rthsed.f
noqual.o: parm.mod noqual.f
	ifort $(compilerOpt) -c noqual.f
hhnoqual.o: parm.mod hhnoqual.f
	ifort $(compilerOpt) -c hhnoqual.f
rtpest.o: parm.mod rtpest.f
	ifort $(compilerOpt) -c rtpest.f
rthpest.o: parm.mod rthpest.f
	ifort $(compilerOpt) -c rthpest.f
rtbact.o: parm.mod theta.o rtbact.f
	ifort $(compilerOpt) -c rtbact.f
irr_rch.o: parm.mod irr_rch.f
	ifort $(compilerOpt) -c irr_rch.f
rchuse.o: parm.mod rchuse.f
	ifort $(compilerOpt) -c rchuse.f
rtout.o: parm.mod rtout.f
	ifort $(compilerOpt) -c rtout.f
rtmusk.o: parm.mod rtmusk.f
	ifort $(compilerOpt) -c rtmusk.f
rtsed_yangsand.o: parm.mod rtsed_yangsand.f
	ifort $(compilerOpt) -c rtsed_yangsand.f
rtsed_Molinas_Wu.o: parm.mod rtsed_Molinas_Wu.f
	ifort $(compilerOpt) -c rtsed_Molinas_Wu.f
rtsed_kodatie.o: parm.mod rtsed_kodatie.f
	ifort $(compilerOpt) -c rtsed_kodatie.f
rtsed_bagnold.o: parm.mod rtsed_bagnold.f
	ifort $(compilerOpt) -c rtsed_bagnold.f
watqual.o: parm.mod watqual.f
	ifort $(compilerOpt) -c watqual.f
watqual2.o: parm.mod watqual2.f
	ifort $(compilerOpt) -c watqual2.f
hhwatqual.o: parm.mod hhwatqual.f
	ifort $(compilerOpt) -c hhwatqual.f

rthr.o: parm.mod rthr.f
	ifort $(compilerOpt) -c rthr.f
rthmusk.o: parm.mod rthmusk.f
	ifort $(compilerOpt) -c rthmusk.f

sub_subbasin.o: parm.mod sub_subbasin.f
	ifort $(compilerOpt) -c sub_subbasin.f
varinit.o: parm.mod varinit.f
	ifort $(compilerOpt) -c varinit.f
water_hru.o: parm.mod water_hru.f
	ifort $(compilerOpt) -c water_hru.f
schedule_ops.o: parm.mod ttcoef_wway.o schedule_ops.f
	ifort $(compilerOpt) -c schedule_ops.f
albedo.o: parm.mod albedo.f
	ifort $(compilerOpt) -c albedo.f
solt.o: parm.mod solt.f
	ifort $(compilerOpt) -c solt.f
surface.o: parm.mod canopyint.o snom.o dailycn.o volq.o surfst_h2o.o alph.o pkq.o tran.o eiusle.o ovr_sed.o cfactor.o ysed.o crackflow.o crackvol.o surface.f
	ifort $(compilerOpt) -c surface.f
autoirr.o: parm.mod autoirr.f
	ifort $(compilerOpt) -c autoirr.f
etpot.o: parm.mod etpot.f
	ifort $(compilerOpt) -c etpot.f
etact.o: parm.mod etact.f
	ifort $(compilerOpt) -c etact.f
wattable.o: parm.mod wattable.f
	ifort $(compilerOpt) -c wattable.f
confert.o: parm.mod confert.f
	ifort $(compilerOpt) -c confert.f
conapply.o: parm.mod conapply.f
	ifort $(compilerOpt) -c conapply.f
graze.o: parm.mod graze.f
	ifort $(compilerOpt) -c graze.f
plantmod.o: parm.mod swu.o grow.o plantmod.f
	ifort $(compilerOpt) -c plantmod.f
nminrl.o: parm.mod nminrl.f
	ifort $(compilerOpt) -c nminrl.f
nitvol.o: parm.mod nitvol.f
	ifort $(compilerOpt) -c nitvol.f
pminrl2.o: parm.mod pminrl2.f
	ifort $(compilerOpt) -c pminrl2.f
pminrl.o: parm.mod pminrl.f
	ifort $(compilerOpt) -c pminrl.f
biozone.o: parm.mod biozone.f
	ifort $(compilerOpt) -c biozone.f
gwmod.o: parm.mod gwmod.f
	ifort $(compilerOpt) -c gwmod.f
gwmod_deep.o: parm.mod gwmod_deep.f
	ifort $(compilerOpt) -c gwmod_deep.f
washp.o: parm.mod washp.f
	ifort $(compilerOpt) -c washp.f
decay.o: parm.mod decay.f
	ifort $(compilerOpt) -c decay.f
pestlch.o: parm.mod pestlch.f
	ifort $(compilerOpt) -c pestlch.f
enrsb.o: parm.mod enrsb.f
	ifort $(compilerOpt) -c enrsb.f
pesty.o: parm.mod pesty.f
	ifort $(compilerOpt) -c pesty.f
orgn.o: parm.mod orgn.f
	ifort $(compilerOpt) -c orgn.f
psed.o: parm.mod psed.f
	ifort $(compilerOpt) -c psed.f
nrain.o: parm.mod nrain.f
	ifort $(compilerOpt) -c nrain.f
nlch.o: parm.mod nlch.f
	ifort $(compilerOpt) -c nlch.f
solp.o: parm.mod solp.f
	ifort $(compilerOpt) -c solp.f
subwq.o: parm.mod subwq.f
	ifort $(compilerOpt) -c subwq.f
bacteria.o: parm.mod bacteria.f
	ifort $(compilerOpt) -c bacteria.f
urban.o: parm.mod urban.f
	ifort $(compilerOpt) -c urban.f
urbanhr.o: parm.mod sweep.o urbanhr.f
	ifort $(compilerOpt) -c urbanhr.f
latsed.o: parm.mod latsed.f
	ifort $(compilerOpt) -c latsed.f
gwnutr.o: parm.mod gwnutr.f
	ifort $(compilerOpt) -c gwnutr.f
gw_no3.o: parm.mod gw_no3.f
	ifort $(compilerOpt) -c gw_no3.f
surfstor.o: parm.mod surfstor.f
	ifort $(compilerOpt) -c surfstor.f
substor.o: parm.mod substor.f
	ifort $(compilerOpt) -c substor.f
filter.o: parm.mod filter.f
	ifort $(compilerOpt) -c filter.f
wetlan.o: parm.mod wetlan.f
	ifort $(compilerOpt) -c wetlan.f
hrupond.o: parm.mod pond.o hrupond.f
	ifort $(compilerOpt) -c hrupond.f
hrupondhr.o: parm.mod pondhr.o hrupondhr.f
	ifort $(compilerOpt) -c hrupondhr.f
pothole.o: parm.mod pothole.f
	ifort $(compilerOpt) -c pothole.f
urb_bmp.o: parm.mod urb_bmp.f
	ifort $(compilerOpt) -c urb_bmp.f
watuse.o: parm.mod watuse.f
	ifort $(compilerOpt) -c watuse.f
watbal.o: parm.mod watbal.f
	ifort $(compilerOpt) -c watbal.f
sumv.o: parm.mod sumv.f
	ifort $(compilerOpt) -c sumv.f
virtual.o: parm.mod tair.o distrib_bmps.o subday.o impndday.o hruday.o virtual.f
	ifort $(compilerOpt) -c virtual.f
NCsed_leach.o: parm.mod NCsed_leach.f90
	ifort $(compilerOpt) -c NCsed_leach.f90
orgncswat.o: parm.mod orgncswat.f
	ifort $(compilerOpt) -c orgncswat.f
bmpfixed.o: parm.mod bmpfixed.f
	ifort $(compilerOpt) -c bmpfixed.f
grass_wway.o: parm.mod grass_wway.f
	ifort $(compilerOpt) -c grass_wway.f
filtw.o: parm.mod filtw.f
	ifort $(compilerOpt) -c filtw.f
buffer.o: parm.mod buffer.f
	ifort $(compilerOpt) -c buffer.f
carbon_zhang2.o: parm.mod ndenit.o carbon_zhang2.f90
	ifort $(compilerOpt) -c carbon_zhang2.f90
carbon_new.o: parm.mod ndenit.o carbon_new.f
	ifort $(compilerOpt) -c carbon_new.f
dormant.o: parm.mod dormant.f
	ifort $(compilerOpt) -c dormant.f

hruyr.o: parm.mod hruyr.f
	ifort $(compilerOpt) -c hruyr.f
impndyr.o: parm.mod impndyr.f
	ifort $(compilerOpt) -c impndyr.f
subyr.o: parm.mod subyr.f
	ifort $(compilerOpt) -c subyr.f
rchyr.o: parm.mod rchyr.f
	ifort $(compilerOpt) -c rchyr.f
rsedyr.o: parm.mod rsedyr.f
	ifort $(compilerOpt) -c rsedyr.f

burnop.o: parm.mod burnop.f
	ifort $(compilerOpt) -c burnop.f
killop.o: parm.mod rootfr.o killop.f
	ifort $(compilerOpt) -c killop.f
harvgrainop.o: parm.mod harvgrainop.f
	ifort $(compilerOpt) -c harvgrainop.f
harvkillop.o: parm.mod rootfr.o harvkillop.f
	ifort $(compilerOpt) -c harvkillop.f
apply.o: parm.mod apply.f
	ifort $(compilerOpt) -c apply.f
fert.o: parm.mod fert.f
	ifort $(compilerOpt) -c fert.f
irrsub.o: parm.mod irrsub.f
	ifort $(compilerOpt) -c irrsub.f
plantop.o: parm.mod plantop.f
	ifort $(compilerOpt) -c plantop.f
harvestop.o: parm.mod rootfr.o harvestop.f
	ifort $(compilerOpt) -c harvestop.f

# Fifth order
expo.o: parm.mod expo.f
	ifort $(compilerOpt) -c expo.f
	
percmicro.o: parm.mod percmicro.f
	ifort $(compilerOpt) -c percmicro.f
sat_excess.o: parm.mod sat_excess.f
	ifort $(compilerOpt) -c sat_excess.f
origtile.o: parm.mod origtile.f
	ifort $(compilerOpt) -c origtile.f
drains.o: parm.mod depstor.o drains.f
	ifort $(compilerOpt) -c drains.f
percmacro.o: parm.mod percmacro.f
	ifort $(compilerOpt) -c percmacro.f

irrigate.o: parm.mod irrigate.f
	ifort $(compilerOpt) -c irrigate.f

adaptfvolh.o: parm.mod adaptfvolh.f
	ifort $(compilerOpt) -c adaptfvolh.f
adaptfsurh.o: parm.mod adaptfsurh.f
	ifort $(compilerOpt) -c adaptfsurh.f
adaptfmaxd.o: parm.mod adaptfmaxd.f
	ifort $(compilerOpt) -c adaptfmaxd.f

theta.o: parm.mod theta.f
	ifort $(compilerOpt) -c theta.f

ttcoef_wway.o: parm.mod ttcoef_wway.f
	ifort $(compilerOpt) -c ttcoef_wway.f

canopyint.o: parm.mod canopyint.f
	ifort $(compilerOpt) -c canopyint.f
snom.o: parm.mod snom.f
	ifort $(compilerOpt) -c snom.f
dailycn.o: parm.mod dailycn.f
	ifort $(compilerOpt) -c dailycn.f
volq.o: parm.mod surq_daycn.o surq_greenampt.o volq.f
	ifort $(compilerOpt) -c volq.f
surfst_h2o.o: parm.mod surfst_h2o.f
	ifort $(compilerOpt) -c surfst_h2o.f
alph.o: parm.mod alph.f
	ifort $(compilerOpt) -c alph.f
pkq.o: parm.mod pkq.f
	ifort $(compilerOpt) -c pkq.f
tran.o: parm.mod tran.f
	ifort $(compilerOpt) -c tran.f
eiusle.o: parm.mod eiusle.f
	ifort $(compilerOpt) -c eiusle.f
ovr_sed.o: parm.mod ovr_sed.f
	ifort $(compilerOpt) -c ovr_sed.f
cfactor.o: parm.mod cfactor.f
	ifort $(compilerOpt) -c cfactor.f
ysed.o: parm.mod ysed.f
	ifort $(compilerOpt) -c ysed.f
crackflow.o: parm.mod crackflow.f
	ifort $(compilerOpt) -c crackflow.f
crackvol.o: parm.mod crackvol.f
	ifort $(compilerOpt) -c crackvol.f

swu.o: parm.mod swu.f
	ifort $(compilerOpt) -c swu.f
grow.o: parm.mod tstr.o nup.o npup.o anfert.o grow.f
	ifort $(compilerOpt) -c grow.f

sweep.o: parm.mod sweep.f
	ifort $(compilerOpt) -c sweep.f

pond.o: parm.mod pond.f
	ifort $(compilerOpt) -c pond.f

pondhr.o: parm.mod pondhr.f
	ifort $(compilerOpt) -c pondhr.f

tair.o: parm.mod tair.f
	ifort $(compilerOpt) -c tair.f
distrib_bmps.o: parm.mod bmp_ri_pond.o bmp_sed_pond.o bmp_sand_filter.o distrib_bmps.f
	ifort $(compilerOpt) -c distrib_bmps.f
subday.o: parm.mod icl.o subday.f
	ifort $(compilerOpt) -c subday.f
impndday.o: parm.mod impndday.f
	ifort $(compilerOpt) -c impndday.f
hruday.o: parm.mod icl.o hruday.f
	ifort $(compilerOpt) -c hruday.f

ndenit.o: parm.mod ndenit.f
	ifort $(compilerOpt) -c ndenit.f

rootfr.o: parm.mod rootfr.f
	ifort $(compilerOpt) -c rootfr.f

# Sixt order
depstor.o: parm.mod depstor.f
	ifort $(compilerOpt) -c depstor.f

surq_daycn.o: parm.mod surq_daycn.f
	ifort $(compilerOpt) -c surq_daycn.f
surq_greenampt.o: parm.mod surq_greenampt.f
	ifort $(compilerOpt) -c surq_greenampt.f

tstr.o: parm.mod tstr.f
	ifort $(compilerOpt) -c tstr.f
nup.o: parm.mod nfix.o nuts.o nup.f
	ifort $(compilerOpt) -c nup.f
npup.o: parm.mod npup.f
	ifort $(compilerOpt) -c npup.f
anfert.o: parm.mod anfert.f
	ifort $(compilerOpt) -c anfert.f

bmp_ri_pond.o: parm.mod bmp_ri_pond.f
	ifort $(compilerOpt) -c bmp_ri_pond.f
bmp_sed_pond.o: parm.mod bmp_sed_pond.f
	ifort $(compilerOpt) -c bmp_sed_pond.f
bmp_sand_filter.o: parm.mod bmp_sand_filter.f
	ifort $(compilerOpt) -c bmp_sand_filter.f

icl.o: parm.mod icl.f
	ifort $(compilerOpt) -c icl.f

# Seventh order
nfix.o: parm.mod nfix.f
	ifort $(compilerOpt) -c nfix.f
nuts.o: parm.mod nuts.f
	ifort $(compilerOpt) -c nuts.f

# Clean intermediate files	
clean:
	rm parm.mod
	rm modparm.o
	rm $(objectsMain)