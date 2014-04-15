#IErlang Installation
Note that IErlang has only been tested with ***IPython 2.0.0***

**IErlang (for now) only works with OSX and Ubuntu that has IPython running on python 2.7.\*** 

##OSX and Ubuntu
1. Install IPython (if not already installed)
```
sudo easy_install ipython[all]
```

2. Make an IErlang directory in your home folder
```
cd $HOME
mkdir ierlang-dev`
cd ierlang-dev
```

3. Install erlang bindings for ZMQ - **erlzmq2** - by running these commands:
 ```
git clone http://github.com/zeromq/erlzmq2.git
cd erlzmq2
make
make docs
make test
```

##OSX Specific Instructions

 * Add erlzmq2 to ERL_LIBS environment by editing the **~/.bash_profile**
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
    

##Ubuntu Specific Instructions

 * Add erlzmq2 to ERL_LIBS environment by editing the **~/.bashrc**
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

##OSX and Ubuntu
###Edit IPython
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
* Open the file located at
  * #####OSX
  ```
  /Library/Python/2.7/site-packages/YOUR-IPYTHON-PACKAGE-NAME.egg/IPython/kernel/zmq/session.py
  ```
  You might need to change the permissions of this file to edit it.
  ```
  sudo chmod 777 /Library/Python/2.7/site-packages/YOUR-IPYTHON-PACKAGE-NAME.egg/IPython/kernel/zmq/session.py
  ```

  * #####Ubuntu
  ```
  /Library/Python/2.7/site-packages/YOUR-IPYTHON-PACKAGE-NAME.egg/IPython/kernel/zmq/session.py```
  You might need to change the permissions of this file to edit it.
  ```
  sudo chmod 777 /usr/local/lib/Python/2.7/dist-packages/YOUR-IPYTHON-PACKAGE-NAME.egg/IPython/kernel/zmq/session.py
  ```
