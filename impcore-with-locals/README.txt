Name : ... Fernando Flores ...
Time spent on assignment : 2hrs
Collaborators : N/A

Looking at the file parse.c it was obvious where I had to check for duplicates. The more difficult part
was figuring out where to get our local variables and bind them to the environment. My initial thought
was to change the parameters for the eval function, however that would involve changing things in files that
we were not supposed to touch. Looking at the structs I noticed you could extract locals and their properties, initialize
them and inject them into the environment. We could simply bind them to the global env. and initialize them
directly using the bindVal() function.

