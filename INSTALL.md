# on ubuntu and debian 
sudo apt-get install libns3-3 libns3-dev

# on macosx 
> brew install open-mpi gsl boost
> curl https://www.nsnam.org/release/ns-allinone-3.16.tar.bz2 > ns3.tar.bz2
> tar -xjf ns3.tar.bz2
> cd ns-allinone-3.16/ns-3.16
> waf configure --disable-tests --disable-examples --enable-mpi \
--doxygen-no-build --disable-nsclick --disable-gtk --disable-python \ 
--prefix=/usr/local/ -d optimized
> waf build
> waf install

# Install opam 

# Configure opam 

opam remote add signpostd git://github.com/crotsos/opam-repo-dev.git
opam switch 4.00.1+mirage-ns3-direct


