# Security Notes

- HTTPS only in production.
- Temporary file storage with TTL and automatic deletion.
- No raw master records exposed. Mask sensitive fields in results.
- Rate limiting and file size limits enforced.
- Audit logs for uploads and matching runs.
- ActivityInfo access uses a read-only API token stored in environment variables.
