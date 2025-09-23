import re
import csv

# regexes for parsing
vector_re = re.compile(r"\{(\w+),(\d+)\}")
event_re = re.compile(r"\{(sending|received),\{hello,(\d+)\}\}")

def parse_vector(vec_str):
    """Parse vector like [{john,1},{ringo,2}] -> dict"""
    return {proc: int(val) for proc, val in vector_re.findall(vec_str)}

def parse_line(line):
    """
    Parse a log line like:
    [{john,1},{ringo,2}],ringo,{received,{hello,77}}
    -> (vector, process, action, msgid)
    """
    line = line.strip()
    if not line:
        return None

    # vector clock
    vec_match = re.match(r"(\[.*\]),(\w+),(\{.*\})", line)
    if not vec_match:
        return None
    vec_str, process, event_str = vec_match.groups()

    # parse vector
    vector = parse_vector(vec_str)

    # parse event (sending/received)
    event_match = event_re.search(event_str)
    if not event_match:
        return None
    action, msgid = event_match.groups()

    return vector, process, action, int(msgid)

def dominates(vc_recv, vc_send):
    """Check if vc_recv >= vc_send element-wise"""
    for k, v in vc_send.items():
        if vc_recv.get(k, 0) < v:
            return False
    return True

def check_log(filename):
    sends = {}
    anomalies = []

    with open(filename) as f:
        for i, line in enumerate(f, 1):
            parsed = parse_line(line)
            if not parsed:
                continue
            clock, proc, action, msgid = parsed

            if action == "sending":
                sends[msgid] = (proc, clock, i)

            elif action == "received":
                if msgid not in sends:
                    anomalies.append((i, f"Receive {msgid} has no matching send"))
                    continue

                sender, send_clock, send_line = sends[msgid]

                # check dominance
                if not dominates(clock, send_clock):
                    anomalies.append((i, f"Receive {msgid} at {proc} does not dominate send clock {send_clock} (sent at line {send_line})"))

                # check increment
                recv_val = clock.get(proc, 0)
                expected = send_clock.get(proc, 0) + 1
                if recv_val < expected:
                    anomalies.append((i, f"Receive {msgid} at {proc} did not increment its own counter (has {recv_val}, expected ≥ {expected})"))

    return anomalies

if __name__ == "__main__":
    anomalies = check_log("testVect1.csv")  # <-- your file
    if not anomalies:
        print("✅ Log is consistent")
    else:
        print("⚠️ Anomalies found:")
        for line_no, desc in anomalies:
            print(f"  Line {line_no}: {desc}")
