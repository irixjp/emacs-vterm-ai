// vterm-ai-status.js --- OpenCode plugin: expose live busy/idle/asking
// status to vterm-ai (https://github.com/irixjp/emacs-vterm-ai).
//
// Why this exists:
//
// OpenCode's SQLite database (opencode.db) only records whether the last
// message has finished, which is enough to infer busy/idle after the
// fact. It never records that a permission request is currently pending
// (that state lives only in memory inside the running process), and the
// terminal title OpenCode sets does not change with state either. So
// there is no way for an outside observer (like vterm-ai, polling from
// Emacs) to detect the "asking" state without help from inside the
// process itself.
//
// This plugin hooks OpenCode's internal event bus and writes a small
// per-directory JSON file every time the session's busy/idle status
// changes, or a permission request is opened/resolved. vterm-ai's
// OpenCode provider (vterm-ai-opencode.el) reads this file and uses it
// to fill the gap the database and terminal title can't cover.
//
// Install:
//   Global (recommended, applies to every project):
//     cp vterm-ai-status.js ~/.config/opencode/plugins/
//   Or from Emacs: M-x vterm-ai-opencode-install-plugin
//
// Status file:
//   ~/.local/share/opencode/vterm-ai-status/<sanitized-directory>.json
//   { "status": "idle" | "busy" | "waiting", "directory": "...",
//     "pid": <opencode process pid>, "updated": <epoch ms> }
//
// The "pid" field lets the Emacs side distinguish a live status from a
// stale leftover of a previous OpenCode process that used to run in the
// same directory (e.g. if this plugin's process got killed before it
// could clean up).

import { writeFileSync, mkdirSync } from "fs"
import { homedir } from "os"
import path from "path"

const STATUS_DIR = path.join(homedir(), ".local", "share", "opencode", "vterm-ai-status")

// Must match the sanitization done by `vterm-ai-opencode--status-file`
// in vterm-ai-opencode.el exactly, or the two sides will look for
// different filenames.
function sanitize(directory) {
  return directory.replace(/[^a-zA-Z0-9]/g, "-")
}

export const VtermAiStatusPlugin = async (ctx) => {
  let statusFile
  try {
    mkdirSync(STATUS_DIR, { recursive: true })
    statusFile = path.join(STATUS_DIR, sanitize(ctx.directory) + ".json")
  } catch (e) {
    // If we can't create the status directory, silently do nothing
    // rather than breaking the host OpenCode process.
    return {}
  }

  let baseStatus = "idle"
  let asking = false

  function write() {
    const status = asking ? "waiting" : baseStatus
    try {
      writeFileSync(
        statusFile,
        JSON.stringify({
          status,
          directory: ctx.directory,
          pid: process.pid,
          updated: Date.now(),
        })
      )
    } catch (e) {
      // Best effort only.
    }
  }

  write()

  return {
    event: async ({ event }) => {
      switch (event.type) {
        case "session.status": {
          const t = event.properties && event.properties.status && event.properties.status.type
          if (t === "busy" || t === "idle") {
            baseStatus = t
            write()
          }
          break
        }
        case "session.idle":
          baseStatus = "idle"
          write()
          break
        // Tool permission requests. Confirmed against OpenCode 1.18.31.
        case "permission.asked":
        // Direct-to-user questions, if/when supported by the running
        // OpenCode version. Harmless no-op if these never fire.
        case "question.asked":
          asking = true
          write()
          break
        case "permission.replied":
        case "permission.rejected":
        case "question.replied":
        case "question.rejected":
          asking = false
          write()
          break
      }
    },
  }
}
