# Deployment

## Backend
- Containerize FastAPI with Uvicorn or Gunicorn.
- Place behind NGINX for TLS termination.

## Frontend
- Build static assets with Vite.
- Serve via NGINX or CDN.

## Data Refresh
- ActivityInfo master data is fetched on each match run and stored as a local snapshot.
- Configure `DEDUP_ACTIVITYINFO_TOKEN` and optional `DEDUP_ACTIVITYINFO_DATABASE_IDS` or `DEDUP_ACTIVITYINFO_FORM_IDS`.
