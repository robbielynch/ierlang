%% -*- erlang -*-

-record(parsed_code, {
  code,
  silent,
  store_history,
  user_variables,
  user_expressions,
  allow_stdin
}).

-record(parsed_header, {
  username,
  uuid,
  message_id,
  message_type,
  date
}).

-record(parsed_text, {
  text,
  line,
  block,
  cursor_pos
}).

-record(reply_message, {
  uuid,
  delim          = <<"<IDS|MSG>">>,
  hmac           = <<"">>,
  header,
  parent_header,
  metadata       = <<"{}">>,
  content
}).

-record(request, {
  header,
  parsed_header,
  content
}).

-record(shell_state, {
  shell_socket,
  io_pub_socket,
  key,
  bindings,
  execution_count
}).
