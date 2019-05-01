    comp:=ifort
     opt:=-c
parallel:=-qopenmp -debug -g -traceback -check all -fp-stack-check

pattern:=*.f90
source :=$(wildcard $(pattern))
   obj :=$(patsubst %.f90, %.o, $(source))
      main : $(obj)
	     $(comp) $(parallel) $^ -o $@

       %.o : %.f90
		$(comp) $(parallel) $(opt) $< -o $@

    main.o : alg.o

       result : main
	        time ./$< > output
       result-r : main
		rm output
		make result
		cat output
        clean :
	 rm -f *.o *.mod main

        clean-all :
	 rm -f *.o *.mod main *.eps *.dat result
	
