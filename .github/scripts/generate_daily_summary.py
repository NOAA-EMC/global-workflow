#!/usr/bin/env python3
"""
Generate a daily activity summary from GitHub and write it to daily_summary_patch.md.

Environment variables:
    GH_TOKEN            Personal access token with  repo read  +  wiki write  scope.
    AUTHOR_USERNAME     GitHub username(s) to filter activity (comma-separated).
    SUMMARY_DATE        ISO date (YYYY-MM-DD) to summarise. Defaults to yesterday.
    REPOS               Comma-separated list of 'owner/repo' strings to inspect.
"""

import os
import sys
import requests
from datetime import date, timedelta, timezone, datetime

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

TOKEN = os.environ.get("GH_TOKEN", "")
if not TOKEN:
    sys.exit("Error: GH_TOKEN is not set.")

AUTHORS = [a.strip() for a in os.environ.get("AUTHOR_USERNAME", "").split(",") if a.strip()]

SUMMARY_DATE_STR = os.environ.get("SUMMARY_DATE", "").strip()
SUMMARY_DATE = date.fromisoformat(SUMMARY_DATE_STR) if SUMMARY_DATE_STR else date.today() - timedelta(days=1)

REPOS_RAW = os.environ.get("REPOS", "NOAA-EMC/global-workflow,NOAA-EMC/GDASapp")
REPOS = [r.strip() for r in REPOS_RAW.split(",") if r.strip()]

DAY_START = datetime(SUMMARY_DATE.year, SUMMARY_DATE.month, SUMMARY_DATE.day, 0,  0,  0,  tzinfo=timezone.utc)
DAY_END   = datetime(SUMMARY_DATE.year, SUMMARY_DATE.month, SUMMARY_DATE.day, 23, 59, 59, tzinfo=timezone.utc)

HEADERS = {
    "Authorization": f"Bearer {TOKEN}",
    "Accept": "application/vnd.github+json",
    "X-GitHub-Api-Version": "2022-11-28",
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def gh_get(url: str, params: dict = None) -> list:
    """Paginated GET — returns all items."""
    results, p = [], {"per_page": 100, **(params or {})}
    while url:
        r = requests.get(url, headers=HEADERS, params=p)
        r.raise_for_status()
        data = r.json()
        results.extend(data if isinstance(data, list) else [data])
        url = None
        p = {}
        for part in r.headers.get("Link", "").split(","):
            part = part.strip()
            if 'rel="next"' in part:
                url = part.split(";")[0].strip().strip("<>")
    return results

def in_window(dt_str: str) -> bool:
    if not dt_str:
        return False
    dt = datetime.fromisoformat(dt_str.replace("Z", "+00:00"))
    return DAY_START <= dt <= DAY_END

def authored_by_us(item: dict) -> bool:
    if not AUTHORS:
        return True
    for field in ("author", "user", "pusher"):
        val = item.get(field)
        login = val.get("login", "") if isinstance(val, dict) else (val or "")
        if login in AUTHORS:
            return True
    return False

def md_link(text: str, url: str) -> str:
    return f"[{text}]({url})"

# ---------------------------------------------------------------------------
# Data collection
# ---------------------------------------------------------------------------

all_commits, all_prs, all_issues = [], [], []

for repo in REPOS:
    # Commits ---------------------------------------------------------------
    try:
        for c in gh_get(f"https://api.github.com/repos/{repo}/commits",
                        {"since": DAY_START.isoformat(), "until": DAY_END.isoformat()}):
            author = (c.get("author") or {}).get("login", "") or \
                     c.get("commit", {}).get("author", {}).get("name", "")
            if AUTHORS and author not in AUTHORS:
                continue
            all_commits.append({
                "repo": repo, "sha": c["sha"][:7],
                "message": c["commit"]["message"].splitlines()[0][:120],
                "author": author, "url": c["html_url"],
            })
    except Exception as e:
        print(f"Warning — commits {repo}: {e}")

    # Pull Requests ---------------------------------------------------------
    for state in ("open", "closed"):
        try:
            for pr in gh_get(f"https://api.github.com/repos/{repo}/pulls",
                             {"state": state, "sort": "updated", "direction": "desc"}):
                action_date = pr.get("merged_at") or pr.get("updated_at") or pr.get("created_at", "")
                if not in_window(action_date):
                    continue
                author = (pr.get("user") or {}).get("login", "")
                if AUTHORS and author not in AUTHORS:
                    continue
                all_prs.append({
                    "repo": repo, "number": pr["number"],
                    "title": pr["title"][:120],
                    "state": "merged" if pr.get("merged_at") else pr["state"],
                    "author": author, "url": pr["html_url"],
                })
        except Exception as e:
            print(f"Warning — PRs ({state}) {repo}: {e}")

    # Issues ----------------------------------------------------------------
    try:
        for iss in gh_get(f"https://api.github.com/repos/{repo}/issues",
                          {"state": "all", "sort": "updated", "direction": "desc", "filter": "all"}):
            if iss.get("pull_request"):
                continue
            if not in_window(iss.get("updated_at") or iss.get("created_at", "")):
                continue
            author = (iss.get("user") or {}).get("login", "")
            if AUTHORS and author not in AUTHORS:
                continue
            labels = [lb["name"] for lb in iss.get("labels", [])]
            all_issues.append({
                "repo": repo, "number": iss["number"],
                "title": iss["title"][:120], "state": iss["state"],
                "author": author, "url": iss["html_url"], "labels": labels,
                "is_question": any("question" in lb.lower() or "help" in lb.lower() for lb in labels),
            })
    except Exception as e:
        print(f"Warning — issues {repo}: {e}")

# ---------------------------------------------------------------------------
# Markdown generation
# ---------------------------------------------------------------------------

def state_badge(state: str, is_issue: bool = False) -> str:
    if state == "merged":   return "🟣 merged"
    if state == "open":     return "🟢 open"
    if state == "closed":   return "✅ closed" if is_issue else "🔴 closed"
    return state

lines = [
    f"## {SUMMARY_DATE.strftime('%B %d, %Y')}\n",
    f"> Auto-generated daily activity summary — {', '.join(REPOS)}"
    + (f" · filter: {', '.join(AUTHORS)}" if AUTHORS else "")
    + ".\n",
    "### 💾 Commits",
]
if all_commits:
    for c in all_commits:
        lines.append(f"- {md_link(f'`{c[\"sha\"]}`', c['url'])} **{c['repo']}** — {c['message']} *(by {c['author']})*")
else:
    lines.append("_No commits matching the filter for this date._")

lines += ["", "### 🔀 Pull Requests"]
if all_prs:
    for pr in all_prs:
        lines.append(f"- {md_link(f'#{pr[\"number\"]}', pr['url'])} **{pr['repo']}** — {pr['title']} ({state_badge(pr['state'])}, by {pr['author']})")
else:
    lines.append("_No pull request activity matching the filter for this date._")

regular_issues   = [i for i in all_issues if not i["is_question"]]
question_issues  = [i for i in all_issues if     i["is_question"]]

lines += ["", "### 🐛 Issues"]
if regular_issues:
    for iss in regular_issues:
        lbl = f" `{'` `'.join(iss['labels'])}`" if iss["labels"] else ""
        lines.append(f"- {md_link(f'#{iss[\"number\"]}', iss['url'])} **{iss['repo']}** — {iss['title']} ({state_badge(iss['state'], True)}, by {iss['author']}){lbl}")
else:
    lines.append("_No issue activity matching the filter for this date._")

lines += ["", "### ❓ Questions / Help Requests"]
if question_issues:
    for iss in question_issues:
        lines.append(f"- {md_link(f'#{iss[\"number\"]}', iss['url'])} **{iss['repo']}** — {iss['title']} ({state_badge(iss['state'], True)}, by {iss['author']})")
else:
    lines.append("_No question-tagged issues activity matching the filter for this date._")

lines += ["", "---\n"]

with open("daily_summary_patch.md", "w") as f:
    f.write("\n".join(lines))

print(f"Summary written: {SUMMARY_DATE}  |  {len(all_commits)} commits  "
      f"|  {len(all_prs)} PRs  |  {len(all_issues)} issues")
