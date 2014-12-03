<a href="http://imgur.com/7rr6Tbr"><img src="http://i.imgur.com/7rr6Tbr.png" title="IErlang"/></a>


# Interactive Erlang Notebook

IErlang is an Erlang language kernel for [IPython.](http://ipython.org) It
allows users to use IPython's Notebook frontend, except where IPython executes
python code, IErlang can execute erlang code and compile and execute erlang
modules.

This is on top of all of IPython's other frontend features like Markdown
rendering, HTML rendering, saving notebooks for later use and even the ability
to view IErlang Notebooks in [IPython's NBViewer](http://nbviewer.ipython.org/)


## Requirements:

* Python 2.7 in your ``$PATH``
* Erlang installed with ``erl`` and ``escript`` in your ``$PATH``
* ``virtualenv`` installed
* ``rebar`` installed

## Installation

You need only do the following to get started:

```bash
$ make demo
```

This will do the following:

* Get all Python deps and patch IPython for IErlang support
* Get all Erlang deps and compile them
* Compile IErlang
* Start up an IErlang Notebook, loading a demo in your browser


## Notes

### Disclaimer

The development of this language kernel for IPython is at it's very early
stages. This is Alpha. Take with a large pinch of salt. No sugar necessary,
because it's pretty damn sweet! :)


### IPython Workaround

Please note: in order to use IErlang, it is required to edit one of IPython's
files. In IPython 2.0.0, this does not affect the usage of the original IPython.


### Feedback

I am eager to receive [feedback](robbie.lynch@outlook.com) from anyone who has
attempted to use IErlang. I would love to hear some thoughts on how to improve
IErlang.


## Resources

* [Demo](http://nbviewer.ipython.org/gist/anonymous/10775415)
* [IErlang Wiki](https://github.com/robbielynch/ierlang/wiki)
