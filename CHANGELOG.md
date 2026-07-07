# Changelog

## 1.4.0 (2026-07-07)

### Changed

- `org-recur-schedule-date` is no longer an interactive command. It always
  required a Lisp argument, so invoking it via `M-x` only ever signaled an
  error.
- `org-recur-agenda-mode` no longer implicitly enables itself in all future
  agenda buffers after being enabled once. To enable it in every agenda
  buffer, use the `org-agenda-mode` hook (see the readme's recommended
  configuration).
- Toggling `org-recur-agenda-mode` in one agenda buffer no longer affects
  highlighting in other agenda buffers (e.g. with `org-agenda-sticky`).

### Fixed

- `Wkdy` matching no longer depends on the global value of
  `case-fold-search`, and a customized `org-recur-weekday` is now treated as
  a literal string (matched as a whole word) rather than a regexp.
- Empty recurrence syntax (`||`) in a heading is treated as no recurrence,
  instead of rescheduling the task to today.
- Corrected docs: automatic archiving is disabled when `org-log-done` is
  `'note` (not `t`, as the warning message and docstring claimed).

### Added

- A mascot!
- Tests for date comparison and next-date selection.

## 1.3.3 and earlier

Not recorded here -- see the
[commit history](https://github.com/mrcnski/org-recur/commits/master).
