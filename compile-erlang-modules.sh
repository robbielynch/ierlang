#!/bin/sh
cd src
erlc zmq_manager.erl

erlc message_builder.erl
erlc message_parser.erl
erlc message_sender.erl

erlc uuid.erl
erlc mochijson2.erl
erlc mochinum.erl

erlc control_server.erl
erlc shell_server.erl
erlc heartbeat_server.erl

erlc code_manager.erl

erlc restrictions.erl
erlc sandbox.erl