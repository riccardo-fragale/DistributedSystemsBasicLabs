#!/bin/bash
# Starts five Erlang nodes in separate macOS Terminal windows:
# - kista, stockholm, spanga form initial 3-node ring
# - lulea joins later to demonstrate handover
# - goteborg is a client that sends probes, stores keys, and does lookups

COOKIE="1234"
BASE_DIR="$PWD"
NODE_KISTA="kista"
NODE_STOCK="stockholm"
NODE_SPANGA="spanga"
NODE_LULEA="lulea"
NODE_GOTEBORG="goteborg"
HOST="Riccardos-MacBook-Air"
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

# 1. Launch kista (first ring node, register as n1)
open_terminal "${NODE_KISTA}" \
  "io:format(\"kista: compiled~n\"), timer:sleep(200), P = test:start(node2), erlang:register(n1,P), io:format(\"kista: registered n1 ~p~n\", [P]), timer:sleep(infinity)."

sleep 1

# 2. Launch stockholm (joins ring via kista, register as n2)
open_terminal "${NODE_STOCK}" \
  "io:format(\"stockholm: compiled~n\"), net_adm:ping('${NODE_KISTA}@${HOST}'), timer:sleep(500), P = test:start(node2, {n1,'${NODE_KISTA}@${HOST}'}), erlang:register(n2,P), io:format(\"stockholm: joined ring, registered n2 ~p~n\", [P]), timer:sleep(infinity)."

sleep 1

# 3. Launch spanga (joins ring via kista, register as n3)
open_terminal "${NODE_SPANGA}" \
  "io:format(\"spanga: compiled~n\"), net_adm:ping('${NODE_KISTA}@${HOST}'), timer:sleep(500), P = test:start(node2, {n1,'${NODE_KISTA}@${HOST}'}), erlang:register(n3,P), io:format(\"spanga: joined ring, registered n3 ~p~n\", [P]), timer:sleep(infinity)."

sleep 1

# 4. Launch goteborg (client node - NOT part of the ring)
# - Wait for ring stabilization
# - Perform demonstrations 1-3
# - For demo 4, signal lulea to join (lulea is in a separate terminal)
open_terminal "${NODE_GOTEBORG}" \
  "io:format(\"goteborg: compiled (client)~n\"), net_adm:ping('${NODE_KISTA}@${HOST}'), net_adm:ping('${NODE_STOCK}@${HOST}'), net_adm:ping('${NODE_SPANGA}@${HOST}'), timer:sleep(5000), io:format(\"~n=== DEMONSTRATION 1: Key Storage ===~n\"), [Key] = test:keys(1), io:format(\"goteborg: generated key ~p~n\", [Key]), io:format(\"goteborg: storing key via kista (n1)~n\"), test:add(Key, bread, {n1,'${NODE_KISTA}@${HOST}'}), timer:sleep(1000), io:format(\"~n=== Checking which node stored the key ===~n\"), {n1,'${NODE_KISTA}@${HOST}'} ! print_storage, timer:sleep(200), {n2,'${NODE_STOCK}@${HOST}'} ! print_storage, timer:sleep(200), {n3,'${NODE_SPANGA}@${HOST}'} ! print_storage, timer:sleep(1000), io:format(\"~n=== DEMONSTRATION 2: Ring Probing ===~n\"), io:format(\"--- Probe from kista ---~n\"), {n1,'${NODE_KISTA}@${HOST}'} ! probe, timer:sleep(500), io:format(\"--- Probe from stockholm ---~n\"), {n2,'${NODE_STOCK}@${HOST}'} ! probe, timer:sleep(500), io:format(\"--- Probe from spanga ---~n\"), {n3,'${NODE_SPANGA}@${HOST}'} ! probe, timer:sleep(500), io:format(\"~n=== DEMONSTRATION 3: Distributed Lookup ===~n\"), io:format(\"--- Lookup from kista ---~n\"), Result1 = test:lookup(Key, {n1,'${NODE_KISTA}@${HOST}'}), io:format(\"Result: ~p~n\", [Result1]), io:format(\"--- Lookup from stockholm ---~n\"), Result2 = test:lookup(Key, {n2,'${NODE_STOCK}@${HOST}'}), io:format(\"Result: ~p~n\", [Result2]), io:format(\"--- Lookup from spanga ---~n\"), Result3 = test:lookup(Key, {n3,'${NODE_SPANGA}@${HOST}'}), io:format(\"Result: ~p~n\", [Result3]), io:format(\"~n=== DEMONSTRATION 4: Handover on Node Join ===~n\"), io:format(\"--- Adding 10 more keys to have data to handover ---~n\"), MoreKeys = test:keys(10), test:add(MoreKeys, {n1,'${NODE_KISTA}@${HOST}'}), timer:sleep(1000), io:format(\"--- Storage BEFORE new node (lulea) joins ---~n\"), {n1,'${NODE_KISTA}@${HOST}'} ! print_storage, timer:sleep(200), {n2,'${NODE_STOCK}@${HOST}'} ! print_storage, timer:sleep(200), {n3,'${NODE_SPANGA}@${HOST}'} ! print_storage, timer:sleep(1000), io:format(\"--- Signaling lulea to join the ring (check lulea terminal) ---~n\"), {join_signal,'${NODE_LULEA}@${HOST}'} ! start_join, timer:sleep(5000), io:format(\"--- Storage AFTER lulea joined (handover should have occurred) ---~n\"), {n1,'${NODE_KISTA}@${HOST}'} ! print_storage, timer:sleep(200), {n2,'${NODE_STOCK}@${HOST}'} ! print_storage, timer:sleep(200), {n3,'${NODE_SPANGA}@${HOST}'} ! print_storage, timer:sleep(200), {n4,'${NODE_LULEA}@${HOST}'} ! print_storage, timer:sleep(1000), io:format(\"~n=== Demo complete - Review the handover messages above ===~n\"), timer:sleep(infinity)."

sleep 1

# 5. Launch lulea (4th ring node) - waits for signal from goteborg before joining
open_terminal "${NODE_LULEA}" \
  "io:format(\"lulea: compiled~n\"), erlang:register(join_signal, self()), net_adm:ping('${NODE_KISTA}@${HOST}'), io:format(\"lulea: waiting for join signal from goteborg...~n\"), receive start_join -> io:format(\"lulea: received join signal, joining ring via kista~n\") end, timer:sleep(500), P = test:start(node2, {n1,'${NODE_KISTA}@${HOST}'}), erlang:register(n4,P), io:format(\"lulea: joined ring, registered n4 ~p~n\", [P]), timer:sleep(infinity)."

echo "✅ Started 5 terminals:"
echo "   - Ring nodes: kista (n1), stockholm (n2), spanga (n3)"
echo "   - New node: lulea (n4, joins after demos 1-3)"
echo "   - Client: goteborg (sends probes and does lookups)"