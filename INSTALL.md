curl https://www.nsnam.org/release/ns-allinone-3.16.tar.bz2 > ns3.tar.bz2
tar -xjf ns3.tar.bz2
cd ns-allinone-3.16
waf configure --disable-tests --disable-examples --enable-mpi
--doxygen-no-build --disable-nsclick --disable-gtk --disable-python 
--prefix=/usr/local/ -d optimized

waf build

for file in  /usr/local/lib/libns3.16*.so; do  
  dst_dir=`echo $file | sed -e "s/ns3.16//g" -e "s/-optimized//g"`; 
  sudo cp $file $dst_dir; 
done
