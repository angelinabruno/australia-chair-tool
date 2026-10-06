# ---- 1. Folder path: the only line to change on a new computer ---------------
# Every other script reads the folder from here. To run one script on its
# own, run this section first (select lines 1-12 and press Ctrl+Enter).

DASHBOARD_DIR <- "C:/Users/Angelina/Documents/CSIS/Dashboard"

if (!dir.exists(DASHBOARD_DIR))
  stop("DASHBOARD_DIR doesn't exist: ", DASHBOARD_DIR,
       "\nEdit the path in section 1 of this script.")

# Stored as an environment variable so it survives scripts that begin with
# rm(list = ls()).
Sys.setenv(DASHBOARD_DIR = normalizePath(DASHBOARD_DIR, winslash = "/"))


# ---- 2. Scripts to run, in order ---------------------------------------------
# Put # in front of a line to skip that script.

.scripts <- c(
  "1a. Overview.R",            # front page charts
  "1b. Economy.R",
  "1c. Data centres.R",
  "1d. Tech.R",
  "1e. Energy.R",
  "1f. Trade.R",
  "1g. Migration.R",
  # "1h. Patents.R",           # ~30 min. Remove the # to refresh patent data
                               # (IP RAPID updates weekly; quarterly is plenty).
  "1i. Investment.R"
)


# ---- 3. Run everything (no need to edit below this line) ---------------------
# Each script starts from the dashboard folder. If one fails, the rest still
# run. Names start with "." so rm(list = ls()) inside a script can't delete them.

.log <- NULL
for (.s in .scripts) {
  setwd(Sys.getenv("DASHBOARD_DIR"))
  message("\n==== ", .s, " ====")
  .t0  <- Sys.time()
  .res <- tryCatch({
    source(file.path(Sys.getenv("DASHBOARD_DIR"), .s))
    "OK"
  }, error = function(e) paste("FAILED:", conditionMessage(e)))
  .log <- rbind(.log, data.frame(
    script  = .s,
    result  = .res,
    minutes = round(as.numeric(difftime(Sys.time(), .t0, units = "mins")), 1)
  ))
}

setwd(Sys.getenv("DASHBOARD_DIR"))
message("\n==== Summary ====")
print(.log, right = FALSE)
message("\nNext step: open '2. Australia Chair Tool.qmd' and click Render.")
