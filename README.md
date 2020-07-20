# generate_graphs
Fortran codes to generate graphs with the configuration model The codes here
are distributed under GNU GPL v3 as a Free Software; please see the LICENSE.

The file random_functions.f95 contains the auxiliary codes for random number
generation and related things. To generate Erdos-Renyi graphs, use er.f95, and
to generate Scale-free graphs, use sf.f95. These files need to be edited before
use. See the description below.

For scale-free graphs (file sf.f95), choose scaling index alpha (lines 12 to
15), by default one of the values 2.2, 2.4, 2.6 and 2.8 could be chosen, or you
can comment all these and add your own value. Note that in that case, you must
calculate the normalization constant properly (lines 21 to 24) and add it
there. The same applies if you choose to change the value of kmin. The number
of graphs that are generated is by default 10000. If you want any other number,
change the number in the loop on line 35 accordingly. The size of the graph is
10000 by default, if you need to use some other value, change n on line 5. 

For Erdos-Renyi graph (file er.f95), choose the appropriate value of the
average degree (lines 12 to 15). Also the number of graphs generated and size
of the graph can be changed just like the scale-free case described above. 

For both the codes, additional directories need to be created by the use; see
paths in the IF-ELSE blocks on 36 (sf.f95) or 26 (er.f95). 

Once the above are done, the codes can be compiled using the following
commands:

gfortran random_functions.f95 sf.f95 -o sf 
gfortran random_functions.f95 er.f95 -o er

Our experience is that gfortran optimization flag -O3 speeds these up, but this
hasn't been tested for all possible situations.

Finally codes can be run as "./sf" or "./er"

Please send any queries to snehal@inferred.co
