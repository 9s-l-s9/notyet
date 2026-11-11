# notyet

Search and edit a calendar the way notmuch handles mail: by search terms,
one line per hit, scriptable, without ever touching an event ID.

```
$ notyet agenda 3
2026-11-06 Fri 09:00-11:00  Bewerbung Acme schreiben  #Fokus  [2026-11-06/bewerbung-acme-schreiben]
2026-11-06 Fri 16:30-17:30  Bewerbungsgespräch: Initech  @Teams  [2026-11-06/bewerbungsgespraech-initech]
2026-11-09 Mon all-day  Frist Initech Online-Test  #Frist  [2026-11-09/frist-initech-online-test]

$ notyet edit acme date:fr --start 10:00 --for 90m
$ notyet tag +Bewerbung -- globex date:2026-11
$ notyet new "Nachhaken: Antwort Acme?" --type Nachhaken --start "+5d 9:00"
```

notyet works on a **vdir**, a directory with one `.ics` file per event,
kept in sync with a CalDAV server by
[vdirsyncer](https://vdirsyncer.pimutils.org).  After every change it runs
the configured sync command, so the server sees the change at once.

## Why

- **No IDs.**  An event is picked by what you know about it: words, tags,
  dates.  If a search matches more than one event, `edit` and `rm` refuse and
  list the candidates.  Every hit carries a readable reference,
  `DATE/slug-of-title`, usable as `ref:` in the next query.
- **For agents too.**  `--format json`, a plain-text export (one file per
  event plus `agenda.txt`) for `ls` and `rg`, and safe defaults: invitations
  from other people are read-only unless `--force`, `rm` is a dry run unless
  `--confirm`.
- **Types.**  Event types (Gespräch, Frist, Fokus, …) are tags with defaults:
  duration, reminders, a colour for the terminal.
- **Colours CalDAV cannot set.**  Some servers (Mailfence) keep event colours
  internally and ignore `COLOR`, but keep a colour when the event is later
  rewritten over CalDAV.  notyet uses that: a **pool** of placeholder events
  titled `notyet-pool: TYPE`, coloured once in the web UI and parked on
  2000-01-01.  `notyet new --type Fokus` takes a Fokus placeholder and turns
  it into the new event, colour included.

## Search terms

Joined by AND; `or`, `not`, `-term` and parentheses work as in notmuch.
Matching ignores case and diacritics (`gesprach` finds *Gespräch*).

| term | matches |
|---|---|
| `acme`, `"online test"` | title, location, notes, tags, organizer, attendees |
| `title:` `loc:` `desc:` `organizer:` `with:` | one field |
| `tag:Fokus`, `type:Frist` | a tag (types are tags) |
| `date:today`, `date:2026-11`, `date:mo..fr`, `date:today..+7d`, `date:this-week`, `date:past` | occurrences overlapping that time; `A..B` includes all of B |
| `ref:2026-11-06/bewerbung` | reference prefix |
| `is:allday` `is:recurring` `is:invite` `is:mine` `is:pool` | |
| `has:reminder` `has:location` `has:tag` `has:type` | |
| `calendar:NAME` `uid:…` | |

Dates and times accept `2026-11-10 14:00`, `10.11. 9:30`, `morgen 14:00`,
`fr`, `+3d`, `14:00` (today); durations `90m`, `1h30m`, `2d`, `1w`.
Recurring events are expanded inside the searched date range (daily,
weekly, monthly, yearly with INTERVAL, COUNT, UNTIL, BYDAY, BYMONTHDAY,
BYMONTH, EXDATE, overridden occurrences); without a date term a series is
listed once.

## Commands

```
notyet agenda [DAYS] [QUERY...]
notyet search QUERY...            [--format text|json|refs] [--limit N]
notyet show QUERY...
notyet count QUERY...
notyet new TITLE --start WHEN [--end WHEN | --for DUR] [--type T] [--tag T]...
           [--location L] [--desc D] [--remind DUR|start|none]... [--all-day]
notyet edit QUERY... [--title] [--start] [--end] [--for] [--date] [--shift ±DUR]
           [--location] [--desc] [--type] [--tag]... [--untag]... [--remind]...
notyet tag +TAG -TAG [--] QUERY...
notyet rm QUERY... [--confirm]
notyet done QUERY...
notyet carry [--days N] [QUERY...]
notyet types | pool [add TYPE N] | sync | export
```

Writes accept `--dry-run` and `--no-sync`.

### To-dos that do not drop out of sight

A to-do is an all-day event titled `To-do: …` or any event of type Fokus.
`notyet done` marks one finished: tag `Erledigt`, title `Erledigt: …`
(renaming it to `Erledigt: …` in the web UI counts as well). `notyet carry`
moves every unfinished to-do of the last 7 days to today as an all-day
`To-do: …`, noting `Offen seit DATE` (and the missed time block) in the
description. It only touches days before today, so running it often is
harmless; the calendar sync timer runs it after every sync.

## Configuration

`~/.config/notyet/config.scm` (or `$NOTYET_CONFIG`) is one alist:

```scheme
((calendars ("mailfence" . "~/Calendar/mailfence/events"))
 (default-calendar . "mailfence")
 (timezone . "Europe/Berlin")
 (me "me@example.org")                    ; you, for the invitation check
 (uid-domain . "example.org")             ; new UIDs: 20261110-title@example.org
 (sync-command "vdirsyncer" "sync")       ; run after each change, or #f
 (export-dir . "~/Calendar/text")         ; plain-text mirror, or #f
 (reminder-action . "EMAIL")              ; or "DISPLAY"
 (types
  ("Fokus" (color . "green") (duration . "2h"))
  ("Frist" (color . "red") (all-day . #t) (reminder . "2d"))
  ("Nachhaken" (color . "orange") (duration . "15m") (reminder . "start"))))
```

## Development

```
guix shell -m manifest.scm -- guile -L . tests/run.scm
guix build -f guix.scm
./bin/notyet help          # runs from the checkout
```

Pure Guile, no dependencies beyond tzdata.  The iCalendar reader keeps every
property and parameter verbatim, so files written back lose nothing notyet
does not understand.

License: GPL-3.0-or-later.
