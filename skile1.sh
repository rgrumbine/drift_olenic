#QSUB -lT 150
#QSUB -lM 8Mw
#QSUB -o skout
#QSUB -me

cd $TMPDIR
RHOME=/wd2/wd21/wd21rg/skile2
OHOME=/wd2/wd21/wd21rg/skile2
CHOME=/wd2/wd21/wd21rg/skile2

NCPUS=1
export NCPUS

tag=`date +'%y%m%d'`

ja

## copy over the directory with all info/files/etc
cp $RHOME/skile2.model    .

if [ ! -s $CHOME/forecast.points ] ; then
  $RHOME/skpoints > forecast.points
 else
  cp $CHOME/forecast.points .
fi


##make unit assignments
assign -a forecast.points   fort.50
assign -a fl.out            fort.60
assign -a ops.out           fort.61
assign -a akout             fort.62
assign -a debugger          fort.9

#get the ice line points
fetch niceline -mVS -t"dsn=nmc.prod.jicice(npice),disp=shr"
fetch siceline -mVS -t"dsn=nmc.prod.jicice(spice),disp=shr"
assign -a niceline   fort.51
assign -a siceline   fort.52

#copy the iceline to the geo directory, every day.
cp niceline ${OHOME}/nice.$tag
cp siceline ${OHOME}/sice.$tag
rcp niceline ${OHOME2}/nice.$tag
rcp siceline ${OHOME2}/sice.$tag
rcp niceline ${OHOME3}/nice.$tag
rcp siceline ${OHOME3}/sice.$tag

#units for the mrf data
fetch a -fTB -mVS -t"dsn=nmc.prod.crsf00.mrf,disp=shr"
fetch b -fTB -mVS -t"dsn=nmc.prod.crsf12.mrf,disp=shr"
fetch c -fTB -mVS -t"dsn=nmc.prod.crsf24.mrf,disp=shr"
fetch d -fTB -mVS -t"dsn=nmc.prod.crsf36.mrf,disp=shr"
fetch e -fTB -mVS -t"dsn=nmc.prod.crsf48.mrf,disp=shr"
fetch f -fTB -mVS -t"dsn=nmc.prod.crsf60.mrf,disp=shr"
fetch g -fTB -mVS -t"dsn=nmc.prod.crsf72.mrf,disp=shr"
fetch h -fTB -mVS -t"dsn=nmc.prod.crsf84.mrf,disp=shr"
fetch i -fTB -mVS -t"dsn=nmc.prod.crsf96.mrf,disp=shr"
fetch j -fTB -mVS -t"dsn=nmc.prod.crsf108.mrf,disp=shr"
fetch k -fTB -mVS -t"dsn=nmc.prod.crsf120.mrf,disp=shr"
fetch l -fTB -mVS -t"dsn=nmc.prod.crsf132.mrf,disp=shr"
fetch m -fTB -mVS -t"dsn=nmc.prod.crsf144.mrf,disp=shr"
assign -a a -Fcos -Cascii -Nibm fort.10
assign -a b -Fcos -Cascii -Nibm fort.11
assign -a c -Fcos -Cascii -Nibm fort.12
assign -a d -Fcos -Cascii -Nibm fort.13
assign -a e -Fcos -Cascii -Nibm fort.14
assign -a f -Fcos -Cascii -Nibm fort.15
assign -a g -Fcos -Cascii -Nibm fort.16
assign -a h -Fcos -Cascii -Nibm fort.17
assign -a i -Fcos -Cascii -Nibm fort.18
assign -a j -Fcos -Cascii -Nibm fort.19
assign -a k -Fcos -Cascii -Nibm fort.20
assign -a l -Fcos -Cascii -Nibm fort.21
assign -a m -Fcos -Cascii -Nibm fort.22

#execute the model
skile2.model
cp ops.out $OHOME/sk2.$tag
cp akout   $OHOME/ak.$tag

#copy over the skile1 forecast
fetch skile1.fore -mVS -t"dsn=nmc.prod.nseaicev.ft08,disp=shr"
cp skile1.fore $OHOME/sk1.$tag

ja -chlst
