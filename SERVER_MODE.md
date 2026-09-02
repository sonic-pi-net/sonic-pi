# Sonic Pi — Server Mode (HTTP API, no auth)

Run Sonic Pi headless and drive it over HTTP — no GUI, no token auth, just `POST /run` with code.

```
curl -X POST http://localhost:8000/run -H 'Content-Type: application/json' \
  -d '{"code":"play 70; sleep 0.5; play 72"}'
```

> **No authentication. Binds `127.0.0.1` by default.** Only use `--host 0.0.0.0` on a trusted network.

## 1. Quick start

### From source (this repo)

```bash
# default: http://127.0.0.1:8000
./bin/sonic-pi-server.sh

# custom port / LAN
./bin/sonic-pi-server.sh --port 3000
./bin/sonic-pi-server.sh --host 0.0.0.0 --port 8000

# env vars also work
SONIC_PI_SERVER_PORT=8000 SONIC_PI_SERVER_HOST=127.0.0.1 ./bin/sonic-pi-server.sh

# help
app/server/ruby/bin/sonic-pi-server.rb --help
```

Direct Ruby (no wrapper script):

```bash
ruby app/server/ruby/bin/sonic-pi-server.rb --host 127.0.0.1 --port 8000
```

### Packaged builds

The release packaging must include the two server-mode files and a Ruby with `webrick`; this change adds the source-tree entry point only. Until a release is built with those requirements, use a source checkout.

If startup reports `cannot load such file -- webrick`, install the matching `webrick` package/gem or use Sonic Pi's bundled Ruby. `json` is part of Ruby's standard library; the rest of the runtime is vendored under `app/server/ruby`.

### Verify

```bash
curl http://localhost:8000/
curl http://localhost:8000/health
curl http://localhost:8000/logs?limit=20
```

## 2. API

All responses are JSON. CORS is enabled (`Access-Control-Allow-Origin: *`).

| Method | Path | Body | Description |
|--------|------|------|-------------|
| `GET` | `/`, `/health`, `/status` | — | Help, version, endpoint list, current token/host/port |
| `GET` | `/version` | — | `{version, booted}` |
| `GET` | `/logs?limit=100` | — | Last buffered Spider logs (max 500, default limit 100). Each entry `{ts, type, msg}` where `type` is `info`, `log`, `error`, `syntax_error`, `run`, or `stop` |
| `POST` | `/run`, `/run-code`, `/eval` | JSON or `text/plain` | Run Sonic Pi code |
| `POST` | `/stop`, `/stop-all`, `/stop-all-jobs` | JSON `{job_id}` optional | Stop all jobs or one job |

### `POST /run`

Preferred content type: `application/json`. `text/plain` is also accepted (raw code as body).

JSON body:

```json
{
  "code": "live_loop :a do\n  play scale(:c4, :major).tick\n  sleep 0.25\nend",
  "workspace": "api"
}
```

- `code` (required) — also accepted as `body` or `src`. Query param `?code=` works too for quick tests.
- `workspace` (optional, default `"api"`) — workspace name tagged on the job (shows up in `/logs` and spider logs).

Raw text alternative:

```bash
curl -X POST http://localhost:8000/run \
  -H 'Content-Type: text/plain' \
  --data-binary 'play 70; sleep 0.5; play 72'
```

Success (200):

```json
{"status":"ok","message":"code sent","workspace":"api"}
```

Error (400):

```json
{"status":"error","error":"missing \"code\" (JSON string). Example: {\"code\":\"play 70\"} or text/plain body"}
```

### `POST /stop`

Stop all jobs, or one job by id:

```bash
# stop everything (like pressing Stop in the GUI)
curl -X POST http://localhost:8000/stop

# stop one job
curl -X POST http://localhost:8000/stop \
  -H 'Content-Type: application/json' \
  -d '{"job_id": 42}'

# also accepts jobId / id / ?job_id=42
```

Success:

```json
{"status":"ok","message":"stopped all jobs"}
{"status":"ok","message":"stopped job 42"}
```

### `GET /logs`

```bash
curl http://localhost:8000/logs?limit=50 | jq .
```

```json
{
  "logs": [
    {"ts":"12:34:56.789","type":"info","msg":"Server ready — token 12345"},
    {"ts":"12:34:57.012","type":"run","msg":"POST /run workspace=api code=live_loop :a do"},
    {"ts":"12:34:57.100","type":"log","msg":"run 1: synth :beep, {note: 70}"},
    {"ts":"12:34:57.200","type":"error","msg":"run 1 line 2: undefined method `plaay' | plaay 70"}
  ],
  "count": 4
}
```

Logs are also streamed to stdout (useful for `journalctl` / Docker).

## 3. Examples

### cURL

```bash
# health
curl http://localhost:8000/

# one-shot note
curl -X POST http://localhost:8000/run \
  -H 'Content-Type: application/json' \
  -d '{"code":"play 70"}'

# live loop (runs until stopped)
curl -X POST http://localhost:8000/run \
  -H 'Content-Type: application/json' \
  -d '{"code":"live_loop :drums do\n  sample :bd_haus\n  sleep 0.5\nend"}'

# with workspace tag
curl -X POST http://localhost:8000/run \
  -H 'Content-Type: application/json' \
  -d '{"code":"play chord(:c4, :major)","workspace":"my-buffer"}'

# stop
curl -X POST http://localhost:8000/stop

# check what happened
curl http://localhost:8000/logs?limit=20
```

### Python

```python
import requests

BASE = "http://localhost:8000"

requests.post(f"{BASE}/run", json={"code": "play 70"})
requests.post(f"{BASE}/run", json={"code": "live_loop :a do\n  play scale(:c4,:major).tick\n  sleep 0.25\nend"})
print(requests.get(f"{BASE}/logs", params={"limit": 5}).json())

# stop all
requests.post(f"{BASE}/stop")
```

### JavaScript / Node

```js
await fetch("http://localhost:8000/run", {
  method: "POST",
  headers: {"Content-Type": "application/json"},
  body: JSON.stringify({code: "play 70"})
});
await fetch("http://localhost:8000/logs").then(r => r.json()).then(console.log);
await fetch("http://localhost:8000/stop", {method: "POST"});
```

### Browser (CORS enabled)

```js
// from any http://localhost:* page — no proxy needed
fetch("http://localhost:8000/run", {
  method: "POST",
  headers: {"Content-Type": "application/json"},
  body: JSON.stringify({code: 'sample :bd_haus; sleep 0.5'})
});
```

## 4. How it works

```
HTTP (WEBrick :8000) → /run → OSC UDP /run-code → Spider (Ruby runtime)
                                      ↕
                                   Daemon (port discovery, keep-alive, SuperSonic boot)
                                      ↕
                                   SuperSonic (audio engine, scope/metrics shm)
```

- Uses `app/server/ruby/bin/headless_boot.rb:HeadlessBoot` — the same harness as `headless-run.rb` / `headless-record.rb`.
- `HeadlessBoot#boot!` spawns `app/server/ruby/bin/daemon.rb`, parses its `daemon gui_listen gui_send scsynth osc_cues token` line, keeps the daemon alive with `/daemon/keep-alive`, and waits for Spider + SuperSonic (`/ack` + `/supersonic/info` / `Live Coding begin`).
- `HeadlessBoot` forwards Spider messages to registered listeners after its normal handlers run, so the HTTP log buffer does not interfere with the boot Promises or console output. `/logs` is a 500-entry ring buffer.
- Code is sent as a single `/run-code` OSC message with `workspace` — same as the GUI's `Run` button.

## 5. Configuration

| Flag / Env | Default | Description |
|------------|---------|-------------|
| `--host HOST` / `SONIC_PI_SERVER_HOST` | `127.0.0.1` | Bind address. Use `0.0.0.0` to expose on LAN (no auth!) |
| `--port PORT` / `SONIC_PI_SERVER_PORT` | `8000` | HTTP port |
| `-h, --help` | — | Help |
| `SONIC_PI_HOME` | `~/.sonic-pi` | Where logs/config live (like GUI). See `PACKAGING.md` |
| `SONIC_PI_ROOT` / `SONIC_PI_ETC_PATH` | auto | Installed tree root (packagers) |

## 6. Deployment

### systemd (Linux / Raspberry Pi)

```ini
# /etc/systemd/system/sonic-pi-server.service
[Unit]
Description=Sonic Pi Server Mode
After=sound.target network.target

[Service]
Type=simple
User=pi
WorkingDirectory=/opt/sonic-pi
ExecStart=/opt/sonic-pi/bin/sonic-pi-server.sh --host 127.0.0.1 --port 8000
Restart=on-failure
Environment=SONIC_PI_HOME=/home/pi

[Install]
WantedBy=multi-user.target
```

```bash
sudo systemctl daemon-reload
sudo systemctl enable --now sonic-pi-server
journalctl -u sonic-pi-server -f
curl http://localhost:8000/
```

### Docker

```dockerfile
FROM ruby:3.2-slim
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential pkg-config libasound2-dev jackd2 supercollider-server \
    && rm -rf /var/lib/apt/lists/*
COPY . /sonic-pi
WORKDIR /sonic-pi
EXPOSE 8000
CMD ["./bin/sonic-pi-server.sh", "--host", "0.0.0.0", "--port", "8000"]
```

> Audio in containers needs `--device /dev/snd` and often `--group-add audio` or host PulseAudio/PipeWire forwarding — out of scope here.

## 7. Security

- **No auth.** Anyone who can reach the HTTP port can run arbitrary Sonic Pi code (which can `require`, shell out via Ruby, etc.).
- Default bind is `127.0.0.1` (loopback only). Don't expose to the internet without a reverse proxy that adds auth (e.g. `nginx` + basic auth, Tailscale, WireGuard).
- If you must bind `0.0.0.0`, firewall it: `ufw allow from 192.168.1.0/24 to any port 8000`.

## 8. Troubleshooting

| Symptom | Fix |
|---------|-----|
| `Address already in use - bind(2) for 127.0.0.1:8000` | Another server/GUI on that port. Use `--port 8001` or `lsof -i :8000` |
| Boot hangs at `waiting for server...` | Check `~/.sonic-pi/log/daemon.log` and `spider.log`. Often JACK/PipeWire or missing SuperSonic binary (`app/server/native/sonic-pi-supersonic`) |
| No sound | Server mode leaves mixer at full (unlike `headless-run.rb` which mutes). Check system volume, JACK/PipeWire, and `~/.sonic-pi/log/supersonic.log` |
| `webrick` not found | Ruby < 3 bundled webrick; Ruby 3+ needs `gem install webrick`. Or use the bundled `app/server/native/ruby` |
| CORS error in browser | Server sends `Access-Control-Allow-Origin: *` — ensure you're POSTing to the exact host/port the server printed |

## 9. Files

| Path | Role |
|------|------|
| `app/server/ruby/bin/sonic-pi-server.rb` | HTTP server (this feature) |
| `bin/sonic-pi-server.sh` | Wrapper — picks `app/server/native/ruby` if present else `ruby` |
| `app/server/ruby/bin/headless_boot.rb` | Shared boot harness |
| `app/server/ruby/bin/daemon.rb` | Port discovery, SuperSonic + Spider spawn, keep-alive |
| `app/server/ruby/bin/repl.rb` | Interactive REPL (also headless, but stdin-based) |
| `app/server/ruby/bin/headless-run.rb` | One-shot runner (muted) |
| `app/server/ruby/bin/headless-record.rb` | Realtime WAV recorder |

## 10. See also

- [README](README.md) — project overview
- [BUILD-LINUX](BUILD-LINUX.md), [BUILD-MAC](BUILD-MAC.md), [BUILD-WINDOWS](BUILD-WINDOWS.md), [BUILD-RASPBERRY-PI](BUILD-RASPBERRY-PI.md) — building
- [PACKAGING](PACKAGING.md) — `SONIC_PI_ROOT` / `SONIC_PI_HOME` knobs
