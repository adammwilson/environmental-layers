
module load R
module load mpi-sgi/mpt.2.06a67
export MPIHOME=/nasa/sgi/mpt/2.06a67/
oexport R_LIBS="/u/awilso10/R/x86_64-unknown-linux-gnu-library/2.15/"

  CC=mpicc R CMD INSTALL --no-test-load --configure-args="--with-Rmpi-type=OPENMPI   --with-Rmpi-libpath=$MPIHOME/lib --with-Rmpi-include=$MPIHOME/include"   ~/Rmpi_0.6-1.tar.gz

mpirun -np 1 R --no-save


/u/scicon/tools/bin/qps
