

Requirements:

        - sdf2table has to be somewhere in the PATH env.
          It is part of the SDF 2.6.3 linux binary distribution 
          http://www.meta-environment.org/releases/sdf-2.6.3-linux-i386.bin.sh
          
        - sbt is used to build smith. 
          http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

Getting smith to run:

        git clone https://github.com/linges/smith.git  
        cd smith/  
        sbt compile  
        sbt "run testFiles main"  

        If everything worked out, you should see something like:  
        "Result: 5"  

Usage:
 
        smith <module path> <module name> [term]  
        Smith expects:  
          - A module path, where it will look for the needed modules.  
          - A module name, e.g.: main or foo.bar.A .  
          - A optional term which is evaluated in the context of the given module.  
            If the term is omitted, "main" is used.   
        
        That means, in order to run it with sbt, you have to call:  
        sbt "run <module path> <module name> [term]"  
        or you start a sbt session, for example:  
        sbt  
        >>> run testFiles main   
