#Interactive Erlang Notebook
IErlang is an Erlang language kernel for [IPython.](http://ipython.org) It allows users to use IPython's Notebook frontend, except where IPython executes python code, IErlang can execute erlang code and compile and execute erlang modules.

This is on top of all of IPython's other frontend features like Markdown rendering, HTML rendering, saving notebooks for later use and even the ability to view IErlang Notebooks in [IPython's NBViewer](http://nbviewer.ipython.org/)

###Disclaimer
Please note, the development of this language kernel for IPython is at it's very early stages. This is Alpha. Take with a large pinch of salt. No sugar necessary, because it's pretty damn sweet! :)


#Demo
[You can view a demo IErlang notebook on IPython's Nbviewer](http://nbviewer.ipython.org/gist/anonymous/10775415)


#IErlang Installation
Note that IErlang has only been tested with ***IPython 2.0.0***

**IErlang (for now) only works with OSX and Ubuntu that has IPython running on python 2.7.\*** 

###Steps
1. Install IPython (preferably in a virtualenv)
2. Make ierlang-dev directory in $HOME directory
3. Downlaod IErlang
4. Install erlzmq2
5. Add ERL_LIBS environment variable
6. Edit IPython Unserializer
7. Edit IErlang startup script

###1. Install IPython (if not already installed)
```
sudo easy_install ipython[all]
```

###2. Make an IErlang directory in your home folder
```
cd $HOME
mkdir ierlang-dev`
cd ierlang-dev
```

###3. Download IErlang
Make sure you're in *ierlang-dev/*
```
git clone http://github.com/robbielynch/ierlang.git
```

###4. Install erlang bindings for ZMQ - **erlzmq2** - by running these commands:
Make sure you're in ierlang-dev/
 ```
git clone http://github.com/zeromq/erlzmq2.git
cd erlzmq2
make
make docs
make test
```

###5. Add erlzmq2 to ERL_LIBS environment variable
####OSX
Edit the **~/.bash_profile**

```
nano ~/.bash_profile
```
Paste the following code at the bottom of the file
```
ERL_LIBS=~/ierlang-dev/erlzmq2:$ERL_LIBS
export ERL_LIBS
export PATH=$PATH:$ERL_LIBS
```
Save the file, then run the command:
```
source ~/.bash_profile
```
    
####Ubuntu
Edit the **~/.bashrc**
```
gedit ~/.bash_profile
```
Paste the following code at the bottom of the file
```
#ERL_LIB
ERL_LIBS=~/ierlang-dev/erlzmq2:$ERL_LIBS
export ERL_LIBS
export PATH=$PATH:$ERL_LIBS
```
Save the file, then run the command:
```
source ~/.bashrc
```


###6. Edit IPython
This is not an ideal situation where it's required to edit IPython's code. However, it is a necessary step. But why?
>>#####Why edit IPython?
In erlang, strings are represented as lists of integers.
Therefore, when IPython receives messages from IErlang kernel,  it doesn't know what to do with the lists of integers, so it ends up doing nothing.

>>>Example of erlang string
```
    MyString = "awesome!".
    %%This is represented as
    [97,119,101,115,111,109,101,33]
```

>>#####IPython/kernel/zmq/session.py
>>This is the edited file and contains the Erlang message parser.

####Now, time to edit the file:
* `Copy` the contents of file `~/ierlang-dev/ierlang/ipython_edited_files/session.py`
* Open the IPython file located at.... and paste what you've copied above into it.
  * **OSX**
  ```
  /Library/Python/2.7/site-packages/YOUR-IPYTHON-PACKAGE-NAME.egg/IPython/kernel/zmq/session.py
  ```
  You might need to change the permissions of this file to edit it
  ```
  sudo chmod 777 /Library/Python/2.7/site-packages/YOUR-IPYTHON-PACKAGE-NAME.egg/IPython/kernel/zmq/session.py
  ```

  * **Ubuntu**
  ```
  /usr/local/lib/python2.7/dist-packages/IPython/kernel/zmq/session.py
  ```
  You might need to change the permissions of this file to edit it
  ```
  sudo chmod 777 /usr/local/lib/python2.7/dist-packages/IPython/kernel/zmq/session.py
  ```
 

###7. Edit the IErlang start up script
Navigate to `~/ierlang-dev/ierlang/src`
* Open both files beginning with `start-ierlang-*.sh` and check that the escript location corresponds to the escript location on your machine. Change it if it not correct.


###8. Start IErlang
* Navigate to `~/ierlang-dev/ierlang/`
* Run the script `./ierlang-notebook.sh`
* Pray that it works



#Possible Errors
* If you see an error in the terminal referring to python code:
`No such file or directory` and if you see `subprocess.py` a couple of lines above this:
  This means that `ierlang/src/start-ierl-notebook.sh` is not referring to the correct location of your escript.

* If you see an error about `erlzmq:context()`, this means your ERL_LIBS are not configured correctly.


#Known Issues
* Encoding tuples inside other data structures
* Encoding floats inside other data structures
* **ERROR MESSAGES** - They do not appear (Something to do with pyerr on ierlang side)
* Cannot create notebooks outside ierlang/src directory
* Horrible installation and setup
* Having to edit the IPython code in order to parse erlang strings (lists of ints)
    * Note that the editing of the IPython code does not affect the normal IPython in any way.
* **No Variable bindings** (yet)
* Tuples are converted to lists when encoding
* Project structure - it's a mess
* Not handling all of IPython's messages (yet)
