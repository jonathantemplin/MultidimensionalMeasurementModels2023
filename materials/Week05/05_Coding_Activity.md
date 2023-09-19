# Class Coding Activity

## Making R Libraries Writable on Argon

As you likely discovered in HW1, using R on Argon causes an issue with installed libraries.

I have found a fix:

* Create a directory in your home directory to store the libraries:

```
mkdir ~/R
mkdir ~/R/libs
pwd
```

* Create a file called `.Renviron` in your home directory:

```
touch ~/.Renviron
```

* Edit the file with `nano` or `vim` and add the following line:
```
nano ~/.Renviron
```

Then, in Nano, type: 

```R_LIBS_USER= [myhomedirectory]/R/libs```

where `[myhomedirectory]` is the output of the `pwd` command above.

* Exit `nano` by typing `ctrl-x` and then `y` to save the file

* Now, when you install libraries in R, they will be installed in your home directory and will be available to you on Argon.
* One caution: If you use different versions of R , you will have to reinstall the libraries to work with the different versions of R
  * (i.e., my Apptainer image for one analysis but Argon for another)

## Additional Coding: Work on HW2

