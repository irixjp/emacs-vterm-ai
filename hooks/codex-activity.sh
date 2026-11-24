#!/bin/sh
# Codex hook for vterm-ai: record what the session is doing, for the dashboard's "Now:" line.
#
# Register it in $CODEX_HOME/hooks.json for UserPromptSubmit, PreToolUse,
# PermissionRequest, Stop, Interrupt, and SessionEnd (see README.md), then
# trust it once with /hooks in Codex.
#
# Writes one line, overwritten on every event, to
#   ${CODEX_HOME:-~/.codex}/vterm-ai-activity/<session_id>
# as
#   <event>\t<epoch>\t<codex pid>\t<summary>
# where the pid is the topmost `codex` process above this hook (the one
# vterm-ai's Codex provider reports), and the summary is
#   PreToolUse         "Bash: <first line of the command>", "apply_patch: <files>",
#                      or "<tool>: <path>" / "<tool>"
#   PermissionRequest  "approve " + the same summary
#   other events       empty (the dashboard then shows nothing)
# SessionEnd deletes the file.  The hook never fails: it always exits 0.

in=$(cat)
command -v jq >/dev/null 2>&1 || exit 0

sid=$(printf '%s' "$in" | jq -r '.session_id // empty' 2>/dev/null)
ev=$(printf '%s' "$in" | jq -r '.hook_event_name // empty' 2>/dev/null)
[ -n "$sid" ] && [ -n "$ev" ] || exit 0
case "$sid" in */* | .*) exit 0 ;; esac

dir="${CODEX_HOME:-$HOME/.codex}/vterm-ai-activity"
file="$dir/$sid"

if [ "$ev" = SessionEnd ]; then
  rm -f "$file"
  exit 0
fi

summary=$(printf '%s' "$in" | jq -r '
  def first_line: tostring | split("\n") | map(select(test("\\S"))) | (.[0] // "") | sub("^\\s+"; "") | .[0:160];
  def tool_summary:
    (.tool_name // "") as $t
    | (.tool_input // {}) as $i
    | if ($i | type) != "object" then $t
      elif $t == "Bash" then "Bash: " + (($i.command // "") | first_line)
      elif $t == "apply_patch" or (($i.command // "") | tostring | test("\\*\\*\\* (Update|Add|Delete) File: ")) then
        "apply_patch: " + ([($i.command // "") | tostring | scan("\\*\\*\\* (?:Update|Add|Delete) File: ([^\n]+)") | .[0]] | join(", ") | .[0:160])
      elif ($i.file_path // $i.path) then $t + ": " + (($i.file_path // $i.path) | first_line)
      else $t end;
  if .hook_event_name == "PreToolUse" then tool_summary
  elif .hook_event_name == "PermissionRequest" then "approve " + tool_summary
  else "" end
' 2>/dev/null | tr '\t\r' '  ')

# The topmost `codex` process above this hook, as vterm-ai's provider sees it.
pid=$PPID
top=-
while [ "${pid:-1}" -gt 1 ]; do
  case "$(ps -o comm= -p "$pid" 2>/dev/null)" in
    codex | */codex) top=$pid ;;
  esac
  pid=$(ps -o ppid= -p "$pid" 2>/dev/null | tr -d ' ')
done

mkdir -p "$dir" 2>/dev/null || exit 0
printf '%s\t%s\t%s\t%s\n' "$ev" "$(date +%s)" "$top" "$summary" > "$file.tmp.$$" 2>/dev/null \
  && mv -f "$file.tmp.$$" "$file" 2>/dev/null
exit 0
