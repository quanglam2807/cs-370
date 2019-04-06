Quang Lam
02/11/2019

1. What are the three ways of thinking about programming, often called programming paradigms?
- The Imperative Model
- The Functional Model
- The Logic Model

2. Name at least one language for each of the three methods of programming described in the previous question?
- The Imperative Model: Algol 60, C++, C, Java, VB.net, Python
- The Functional Model: Lisp, Scheme, Scala, Clojure, Elixir, Haskell, Caml, Standard ML.
- The Logic Model: Prolog

3. Name one person who had a great deal to do with the development of the imperative programming model. Name another who contributed to the functional model. Finally, name a person who was responsible for the development of the logic model of programming?
- The Imperative Model: John Backus, who developed Algol 60.
- The Functional Model: John McCarthy, who developed Lisp.
- The Logic Model: Alain Colmerauer, Robert Kowalski, who developed Prolog.

4. What are the primary characteristics of each of the imperative, functional, and logic models?
- The Imperative Model: is to get data as input, transform it via updates to memory, and then produce output based on this imperatively changed data.
- The Functional Model: wouldn’t allow any updates to existing values as new values are constructed from old values.
- The Logic Model: uses a database of facts or rules where a single program tries to answer questions with a yes or no answer based on it.

5. Who are recognized as the founders of each of the languages this text covers: Java, C++, Python, Standard ML, and Prolog?
- Java: James Gosling
- C++: Bjarne Stroustrup
- Python: Guido van Rossum
- Standard ML: Robin Milner
- Prolog: Alain Colmerauer, Robert Kowalsk

6. Name a language, other than Python, C++, or Java, that is imperative object-oriented in nature.
- C# (addl: Objective-C, Ruby, Swift).

7. Name a language besides Standard ML, that is a functional programming language.
- Lisp (addl: Scheme, Scala, Clojure, Elixir, Haskell, Caml).

8. What other logic programming languages are there other than Prolog? You might have to get creative on this one.
- Datalog.
- Context-Free Grammar?

9. Why is compiling a program preferred over interpreting a program?
Compiling a program means translating to the source code into machine language. This allows the program to be optimized specifically for the machine (the CPU architecture and operating system) it compiled to. So the executable (compiled) program will run faster. Also the exectuable can be run without the extra steps of compiling (except when changes are made) or intepreting.

10. Why is interpreting a program preferred over compiling a program?
Intepreting a program means the program is read and run directly by a interpreter which means it doesn't need to be first translated to machine language (so saving a step). So it's easier and faster to run the program during the development which fasten the development cycle. Also, the interpreting process runs directly on the source code and doesn't generate an executable, we only need to keep track of the source code and don't need to do anything extra when changes are made. Lastly, source code is not platform-dependable so we can easily move/share the program between multiple platforms.

11. What benefits do virtual machine languages have over interpreted languages?
Virtual machine languages are also later interpreted by an interpreter (in this case, called virtual machine) but it's first compiled to bytecode. The compiled bytecode cannot be executed by the hardware directly but can only executed by the virtual machine. Because of this, virtual machine languages share most of the characteristics of interpreted languages but the VM interpreter can be smaller and faster than traditional interpreters.

12. What is a bytecode program? Name two languages that use bytecode in their implementation.
- A bytecode program is a consistent implementation of a set of low-level instruction which can be read and run by a virtual machine.
- Two languages that use bytecode in their implementation: Python & Java.

13. Why are types important in a programming language?
Because it's necessary to specify which operations make sense on which types of data.

14. What does it mean for a programming language to be dynamically typed?
Dynamically typed languages defer all type checking until the last possible second when the program is actually executing.

15. What does it mean for a programming language to be statically typed?
Statically typed language go through a step during before execution where type checking is performed to see if the operations are defined for the given types of operands.
