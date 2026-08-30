# Minimal otp helper: keyring-backed TOTP secret storage and otpauth helper
# TOTP verification requires the 'otp' package. Email OTP sending handled in app server.

get_mfa_secret <- function(email) {
  svc <- paste0("dedup_mfa_", email)
  if (requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      val <- keyring::key_get(service = svc, username = email)
      return(val)
    }, error = function(e) {
      return("")
    })
  }
  return("")
}

set_mfa_secret <- function(email, secret) {
  svc <- paste0("dedup_mfa_", email)
  if (requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      keyring::key_set_with_value(service = svc, username = email, password = secret)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  return(FALSE)
}

delete_mfa_secret <- function(email) {
  svc <- paste0("dedup_mfa_", email)
  if (requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      keyring::key_delete(service = svc)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  return(FALSE)
}

otpauth_uri <- function(secret, email, issuer = NULL) {
  # Build otpauth URI for QR codes and authenticator apps
  if (is.null(issuer)) issuer <- "Deduplication"
  label <- URLencode(paste0(issuer, ":", email), reserved = TRUE)
  params <- paste0("secret=", URLencode(secret, reserved = TRUE), "&issuer=", URLencode(issuer, reserved = TRUE))
  paste0("otpauth://totp/", label, "?", params)
}

verify_totp_code <- function(email, code) {
  # Verify a TOTP code if otp is installed and a secret exists
  secret <- get_mfa_secret(email)
  if (is.null(secret) || !nzchar(secret)) return(FALSE)
  if (!requireNamespace("otp", quietly = TRUE)) {
    warning("otp package not installed; TOTP not available")
    return(FALSE)
  }
  # otp::TOTP$new() returns a TOTP object for a given secret (assumes base32 secret)
  ok <- FALSE
  tryCatch({
    current <- as.character(otp::TOTP$new(secret)$now())
    if (identical(as.character(code), current)) ok <<- TRUE
    # allow code drift +/-1 step if otp offers window parameter - but here simple check
  }, error = function(e) {
    ok <<- FALSE
  })
  isTRUE(ok)
}
