ifeq ($(shell which erl),)
$(error Can't find Erlang executable 'erl')
exit 1
endif

PY2=python2.7
PY2_VENV=./.venv-py2
PY2_VENV_BIN=$(PY2_VENV)/bin/$(PY2)
PATCHED=patches/patched
SESSION_SRC=$(PY2_VENV)/lib/python2.7/site-packages/IPython/kernel/zmq/session.py

ESCRIPT=$(shell which escript)
IPY_KERN=$(shell pwd)/bin/start_kernel
DEP_LIBS1=$(shell pwd)/deps/erlzmq:$(shell pwd)/deps/mochiweb
DEP_LIBS=$(DEP_LIBS1):$(shell pwd)/deps/sandbox:$(shell pwd)/deps/uuid
IERLANG_LIB=$(shell pwd)
ERLLIBS=$(ERL_LIBS):$(DEP_LIBS):$(IERLANG_LIB)
IERLANG_DEMO=notebooks/ierlang_demo.ipynb

OPT_KERN_MGR_CMD='["$(ESCRIPT)", "$(IPY_KERN)", "{connection_file}"]'

$(PY2_VENV_BIN):
	@virtualenv --python=$(PY2) $(PY2_VENV)

py2deps: $(PY2_VENV_BIN)
	@. $(PY2_VENV)/bin/activate && \
	pip install -r requirements.txt
	@make $(PATCHED)

$(PATCHED):
	patch $(SESSION_SRC) < patches/ierlang.patch
	touch $(PATCHED)

compile:
	@echo "Compiling IErlang..."
	@rebar get-deps
	@rebar compile

py2shell-base:
	@echo "Starting IErlang Console..."
	. $(PY2_VENV)/bin/activate && \
	ERL_LIBS=$(ERLLIBS) \
	ipython2 console \
	--KernelManager.kernel_cmd=$(OPT_KERN_MGR_CMD) \
	--Session.key="" \
	--Session.keyfile=""

py2shell-no-deps:
	@rebar compile skip_deps=true
	@make py2shell-base

py2shell: py2deps compile py2shell-base

erl:
	ERL_LIBS=$(ERLLIBS) erl

pyclean:
	rm -rf $(PY2_VENV) $(PATCHED)

erlclean:
	rebar clean

clean: pyclean erlclean

demo-base:
	@echo "Starting IErlang Notebook Demo..."
	@. $(PY2_VENV)/bin/activate && \
	ERL_LIBS=$(ERLLIBS) \
	ipython2 notebook $(IERLANG_DEMO) \
	--KernelManager.kernel_cmd=$(OPT_KERN_MGR_CMD) \
	--Session.key="" \
	--Session.keyfile=""

demo: py2deps compile demo-base

demo-no-deps:
	@rebar compile skip_deps=true
	@make demo-base

py2notebook: IERLANG_DEMO=notebooks
py2notebook: py2deps compile demo-base
