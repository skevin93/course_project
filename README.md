# course_project
## Quick start

Clone the repository:
```
$ git clone git@github.com:skevin93/course_project.git
```

This will fetch the entire repository in a directory called *course_project*.

To build the code:
```
$ make [-j4]
```
This will generate the `duschinsky` executable inside the directory

##Execute duschinky
Once compiled, you can launch the executable simply with
```
./duschinsky [-h|-I] [input.inp] [output.out]
```
There are two possible options:
   * -h, which print a brief help
   * -I, which toggle the interactive mode