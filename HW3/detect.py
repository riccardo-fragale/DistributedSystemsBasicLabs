import os

# Path to the log file (relative to script location)
log_path = os.path.join(os.path.dirname(__file__), "test", "testLamp3.csv")

rows = []
with open(log_path) as f:
    for line in f:
        line = line.strip()
        if not line:
            continue
        # Split only the first two commas: time, actor, rest
        parts = line.split(",", 2)
        if len(parts) != 3:
            continue
        t = int(parts[0].strip())
        actor = parts[1].strip()
        event = parts[2].strip()
        rows.append((t, actor, event))

send_times = {}
violations = []

for t, actor, event in rows:
    if event.startswith("{sending"):
        msg = event.split(",", 1)[1][:-2]   # e.g. {hello,68}
        send_times[msg] = t
    elif event.startswith("{received"):
        msg = event.split(",", 1)[1][:-2]
        if msg not in send_times:
            violations.append((t, actor, msg, "received before any send"))
        else:
            send_t = send_times[msg]
            if t < send_t:
                violations.append((t, actor, msg,
                                   f"received at {t} before send at {send_t}"))

if violations:
    print("Ordering Violations Detected:")
    for v in violations:
        print(v)
else:
    print("✅ No ordering violations found.")
