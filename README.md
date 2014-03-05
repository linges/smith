#Smith

Smith is the implementations of my master thesis.

##Abstract:

Functional programming languages come with several predefined binding structures,
such as lambda abstractions, let expressions, monadic do notations and list comprehension.
We specify and implement a programming language that allows to define new sequential and parallel binders. 
In addition, it is possible to add new syntax for such binders and other language elements.
These binding structures are implemented through functions, which have to fulfill certain type requirements. 
Theses implementations are then used to transform the application of binders and bindings
to more primitive language expressions, such as abstraction and application. 
Definitions of binder and bindings are type checked, so that only the type information 
and not the implementation is required to check their application. 
Errors in the definitions of such binding structures can be found before they are used. 
Furthermore, the consistency requirements for binding structures ensure type safety 
for the application with arguments of the appropriate type. 
The semantic analysis is performed before the transformation to primitive expressions. 
As a result error messages are relative to the original input. 



##Requirements:

        * sdf2table has to be somewhere in the PATH env.
          It is part of the SDF 2.6.3 linux binary distribution 
          http://www.meta-environment.org/releases/sdf-2.6.3-linux-i386.bin.sh
          
        * sbt is used to build smith. 
          http://www.scala-sbt.org/release/docs/Getting-Started/Setup.html

##Getting smith to run:

        git clone https://github.com/linges/smith.git  
        cd smith/  
        sbt compile  
        sbt "run testFiles main"  

        If everything worked out, you should see something like:  
        "Result: 5"  

##Usage:
 
        smith <module path> <module name> [term]  
        Smith expects:  
          * A module path, where it will look for the needed modules.  
          * A module name, e.g.: main or foo.bar.A .  
          * A optional term which is evaluated in the context of the given module.  
            If the term is omitted, "main" is used.   
        
        That means, in order to run it with sbt, you have to call:  
        sbt "run <module path> <module name> [term]"  
        or you start a sbt session, for example:  
        sbt  
        >>> run testFiles main   
