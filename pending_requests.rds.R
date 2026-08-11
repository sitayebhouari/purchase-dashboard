# في RStudio Console
pending_requests <- data.frame(
  id = integer(0),
  username = character(0),
  password = character(0),  # مؤقت - سيتم تغييره بعد الموافقة
  role_requested = character(0),
  status = character(0),  # "pending", "approved", "rejected"
  requested_at = character(0),
  approved_by = character(0),
  approved_at = character(0),
  stringsAsFactors = FALSE
)
saveRDS(pending_requests, "data/pending_requests.rds")