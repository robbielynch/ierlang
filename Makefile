ifeq ($(shell which erl),)
$(error Can't find Erlang executable 'erl')
exit 1
endif

PY2=python2.7
PY2_VENV=./.venv-py2

py2venv:
	@virtualenv --python=$(PY2) $(PY2_VENV)

py2deps: py2venv
	@. $(PY2_VENV)/bin/activate && \
	pip install -r requirements.txt

compile:
	@echo "Compiling IErlang..."
	cd src && \
	erlc {zmq_manager,message_builder,message_sender,uuid,mochijson2,mochinum,control_server,shell_server,heartbeat_server,code_manager,restrictions,sandbox}.erl

py2notebook: py2deps compile
	@echo "Starting IErlang Notebook..."
	@. $(PY2_VENV)/bin/activate && \
	cd src && \
	ERL_LIBS=$(ERL_LIBS) ./start-ierl-notebook.sh

py2shell: py2deps compile
	@echo "Starting IErlang Console..."
	@. $(PY2_VENV)/bin/activate && \
	cd src && \
	ERL_LIBS=$(ERL_LIBS) ./start-ierl-console.sh
