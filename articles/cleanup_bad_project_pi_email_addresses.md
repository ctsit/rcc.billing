# cleanup_bad_project_pi_email_addresses

Note: this script can no longer read mailboxes. This started in October
2022 when UF moved its Office 365 service to OAuth2. The script will
throw warnings about this issue, but it still runs.

Locate bad email addresses in Project PI data, replace them if possible,
erase them if not.

This script reads the mail of a dummy in box that is used as the sender
of numerous automated emails. When those messages generate bounce
messages, they are delivered to the dummy inbox. This script reads that
inbox, searches for message subject lines it recognizes and parses the
message body to locate bad email addresses. It searches REDCap user
information for a suitable replacement email address. If it finds one it
replaces the bad address where it occurs. Otherwise it erases the bad
email address from Project PI data.
