# posts

One public folder per LinkedIn post, holding the data, the R code and the exported charts,
so each post can link to its code and data. This repo is public: keep client names, spend
and private notes out of it.

## Keeping STATUS.md current

`STATUS.md` is the one page that says where the project stands now: what waits on Alex, what
is blocked, what comes next, what was delivered to whom. Alex's morning brief reads it. Read
it at the start of a session.

- When you finish a piece of work, update `STATUS.md` before the final commit, in the same
  commit or the one after it. Change the sections the work touched and the `Last updated` line.
- Overwrite it, never append. Git keeps the history. Keep it to about one screen.
- Keep the headings in every version. Write "nothing" under a heading that has nothing, so an
  empty section is never mistaken for one nobody filled in.
- Mark inference as inference ("probably", "not confirmed"). Record a delivery only when Alex
  says it happened.
- Move a "Needs Alex" item out once he answers.
