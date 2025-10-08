#!/bin/bash
# Starts three Erlang nodes in separate macOS Terminal windows, compiles .erl files,
# creates a small ring (kista <-> stockholm) and from spanga adds keys & runs a lookup check.

COOKIE="1234"
BASE_DIR="$PWD"
NODE_KISTA="kista"
NODE_STOCK="stockholm"
NODE_SPANGA="spanga"
ERL_FILES=("test" "node2" "storage" "key" "node3" "node4" "node1")

# Helper function to write Erlang commands directly to a file (no shell escaping)
write_erlang_cmd() {
  local cmd_file=$1
  shift
  # Write the Erlang expressions directly to the file with proper syntax
  {
    for f in "${ERL_FILES[@]}"; do
      echo -n "compile:file(\"${f}.erl\"), "
    done
    # Output the rest of the command passed as arguments
    echo "$@"
  } > "${cmd_file}"
}

open_terminal() {
  local NODE=$1
  shift
  local tmp_file="${BASE_DIR}/start_${NODE}.sh"
  local cmd_file="${BASE_DIR}/start_${NODE}.cmd"

  # Write the Erlang command to a .cmd file (using the helper)
  write_erlang_cmd "${cmd_file}" "$@"

  # Create a wrapper script that runs erl with the command from the file
  cat > "${tmp_file}" <<EOF
#!/bin/bash
cd "${BASE_DIR}"
erl -sname ${NODE} -setcookie ${COOKIE} -kernel connect_all false -eval "\$(cat '${cmd_file}')"
EOF
  chmod +x "${tmp_file}"

  # Launch Terminal with the wrapper script
  osascript -e "tell application \"Terminal\" to do script \"/bin/bash '${tmp_file}'\""
}

# Launch the three terminals with Erlang commands (no escaping needed - written directly to .cmd files)
open_terminal "${NODE_KISTA}" \
  "io:format(\"kista: compiled files~n\"), timer:sleep(200), P = test:start(node2), erlang:register(n1,P), io:format(\"kista: registered n1 ~p~n\", [P]), timer:sleep(infinity)."

sleep 1

open_terminal "${NODE_STOCK}" \
  "io:format(\"stockholm: compiled files~n\"), net_adm:ping('${NODE_KISTA}@Riccardos-MacBook-Air'), timer:sleep(500), P = test:start(node2, {n1,'${NODE_KISTA}@Riccardos-MacBook-Air'}), io:format(\"stockholm: started node2 ~p~n\", [P]), timer:sleep(infinity)."

sleep 1

open_terminal "${NODE_SPANGA}" \
  "io:format(\"spanga: compiled files~n\"), net_adm:ping('${NODE_KISTA}@Riccardos-MacBook-Air'), timer:sleep(2000), Keys = test:keys(1000), io:format(\"spanga: generated keys (~p)~n\", [length(Keys)]), test:add(Keys, {n1,'${NODE_KISTA}@Riccardos-MacBook-Air'}), io:format(\"spanga: added keys to ring via n1~n\"), {n1,'${NODE_KISTA}@Riccardos-MacBook-Air'} ! probe, timer:sleep(500), test:check(1, Keys, {n1,'${NODE_KISTA}@Riccardos-MacBook-Air'}), timer:sleep(infinity)."

echo "✅ Started kista, stockholm, spanga. spanga will add keys and run a check."