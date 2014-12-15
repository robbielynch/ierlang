ifeq ($(shell which erl),)
$(error Can't find Erlang executable 'erl')
exit 1
endif

PY2=python2.7
PY2_VENV=./.venv-py2
ESCRIPT=$(shell which escript)
IPY_KERN=$(shell pwd)/bin/start_kernel
DEP_LIBS=deps/erlzmq:deps/mochiweb:deps/sandbox:deps/uuid
IERLANG_LIB=$(shell pwd)
ERLLIBS=$(ERL_LIBS):$(DEP_LIBS):$(IERLANG_LIB)

py2venv:
	@virtualenv --python=$(PY2) $(PY2_VENV)

py2deps: py2venv
	@. $(PY2_VENV)/bin/activate && \
	pip install -r requirements.txt

compile:
	@echo "Compiling IErlang..."
	@rebar get-deps
	@rebar compile

py2notebook: py2deps compile
	@echo "Starting IErlang Notebook..."
	@. $(PY2_VENV)/bin/activate && \
	ERL_LIBS=$(ERLLIBS) \
	ipython2 notebook \
	--KernelManager.kernel_cmd='["$(ESCRIPT)", "$(IPY_KERN)", "{connection_file}"]' \
	--Session.key="" \
	--Session.keyfile=""

py2shell: py2deps compile
	@echo "Starting IErlang Console..."
	@. $(PY2_VENV)/bin/activate && \
	ERL_LIBS=$(ERLLIBS) \
	ipython2 console \
	--KernelManager.kernel_cmd='["$(ESCRIPT)", "$(IPY_KERN)", "{connection_file}"]' \
	--Session.key="" \
	--Session.keyfile=""

erl:
	ERL_LIBS=$(ERLLIBS) erl
